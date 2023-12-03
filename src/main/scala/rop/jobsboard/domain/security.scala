package rop.jobsboard.domain

import cats.*
import cats.implicits.*
import org.http4s.{Response, Status}
import rop.jobsboard.domain.user.*
import tsec.authentication.{AugmentedJWT, JWTAuthenticator, SecuredRequest, TSecAuthService}
import tsec.authorization.{AuthorizationInfo, BasicRBAC}
import tsec.mac.jca.HMACSHA256

// internal to the server
object security {
  type Crypto              = HMACSHA256
  type JwtToken            = AugmentedJWT[Crypto, String]
  type Authenticator[F[_]] = JWTAuthenticator[F, String, User, Crypto]
  type AuthRoute[F[_]]     = PartialFunction[SecuredRequest[F, User, JwtToken], F[Response[F]]]
  type AuthRBAC[F[_]]      = BasicRBAC[F, Role, User, JwtToken]

  // Role Based Access Control - RBAC
  // BasicRBAC[F, Role, User, JwtToken]
  given authRole[F[_]: Applicative]: AuthorizationInfo[F, Role, User] with {
    override def fetchInfo(u: User): F[Role] = u.role.pure[F]
  }

  def allRoles[F[_]: MonadThrow]: AuthRBAC[F] = BasicRBAC.all[F, Role, User, JwtToken]

  def recruiterOnly[F[_]: MonadThrow]: AuthRBAC[F] = BasicRBAC(Role.RECRUITER)

  def adminOnly[F[_]: MonadThrow]: AuthRBAC[F] = BasicRBAC(Role.ADMIN)

  case class Authorizations[F[_]](rbacRoutes: Map[AuthRBAC[F], List[AuthRoute[F]]])
  object Authorizations {

    // step 3- Semigroup for Authorization
    given combiner[F[_]]: Semigroup[Authorizations[F]] = Semigroup.instance { (authA, authB) =>
      Authorizations(authA.rbacRoutes |+| authB.rbacRoutes)
    }
  }

  // AuthRoute -> Authorizations -> TSecAuthService -> HttpRoute
  // step 1- AuthRoute -> Authorizations = .restrictedTo extension method
  extension [F[_]](authRoute: AuthRoute[F])
    def restrictedTo(rbac: AuthRBAC[F]): Authorizations[F] =
      Authorizations(Map(rbac -> List(authRoute)))

  // step2- Authorizations -> TSecAuthService = implicit conversion
  given auth2tsec[F[_]: Monad]: Conversion[Authorizations[F], TSecAuthService[User, JwtToken, F]] =
    auths => {
      // This responds with 401 always
      val unauthorizedService: TSecAuthService[User, JwtToken, F] = TSecAuthService[User, JwtToken, F] { _ =>
        Response[F](Status.Unauthorized).pure[F]
      }

      auths.rbacRoutes // Map[RBAC, List[AuthRoute[F]]]
        .toSeq
        .foldLeft(unauthorizedService) { case (accumulator, (rbac, routes)) =>
          // merge routes into one
          val bigRoute = routes.reduce(_.orElse(_))
          // build a new service, or fall back to the accumulator if rbac/route fails
          TSecAuthService.withAuthorizationHandler(rbac)(bigRoute, accumulator.run)
        // TSecAuthService is a Kleisli, so we need to 'run' the 'accumulator'
        }
    }

}
