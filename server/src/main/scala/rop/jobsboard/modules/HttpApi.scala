package rop.jobsboard.modules

import cats.*
import cats.data.OptionT
import cats.effect.*
import cats.implicits.*
import org.http4s.*
import org.http4s.dsl.*
import org.http4s.dsl.impl.*
import org.http4s.server.*
import org.typelevel.log4cats.Logger
import rop.jobsboard.config.SecurityConfig
import rop.jobsboard.core.Users
import rop.jobsboard.domain.security.{Authenticator, JwtToken, SecuredHandler}
import rop.jobsboard.domain.user.User
import rop.jobsboard.http.routes.{AuthRoutes, HealthRoutes, JobRoutes}
import tsec.authentication.{BackingStore, IdentityStore, JWTAuthenticator, SecuredRequestHandler}
import tsec.common.SecureRandomId
import tsec.mac.jca.HMACSHA256

class HttpApi[F[_]: Concurrent: Logger] private (core: Core[F], authenticator: Authenticator[F]) {

  given securedHandler: SecuredHandler[F] = SecuredRequestHandler(authenticator)

  private val healthRoutes = HealthRoutes[F].routes
  private val jobRoutes    = JobRoutes[F](core.jobs, core.stripe).routes
  private val authRoutes   = AuthRoutes[F](core.auth, authenticator).routes

  val endpoints: HttpRoutes[F] = Router(
    "/api" -> (healthRoutes <+> jobRoutes <+> authRoutes)
  )
}

object HttpApi {

  def createAuthenticator[F[_]: Sync](users: Users[F], securityConfig: SecurityConfig): F[Authenticator[F]] = {
    // 1- identity store for retrieving users (in-memory map to find a user by key)
    // function: String => OptionT[F, User]
    // Updating the implementation to fetch the user in memory first, before checking the DB, because the initial behavior
    // of the 'find user' method in 'Users.scala', calls the DB every time to check the logged in user, which can be an
    // a resource-consuming process. The fetch should be done in the memory first.
    val idStoreF: F[IdentityStore[F, String, User]] =
      Ref
        .of[F, Map[String, User]](Map.empty) // F[Ref[F, Map[String, User]]]
        .map { ref =>
          new BackingStore[F, String, User] {
            override def get(email: String): OptionT[F, User] = {
              val effect = for {
                inMemoryUser <- ref.get.map(inMemMap => inMemMap.get(email))
                potentialUser <-
                  if (inMemoryUser.isEmpty) users.find(email)
                  else inMemoryUser.pure[F]
                _ <-
                  if (inMemoryUser.isEmpty) potentialUser.map(put).sequence
                  else None.pure[F]
              } yield potentialUser

              OptionT(effect)
            }

            override def put(user: User): F[User] =
              ref.modify(inMemMap => (inMemMap + (user.email -> user), user))

            override def update(user: User): F[User] =
              put(user)

            override def delete(email: String): F[Unit] =
              ref.modify(inMemMap => (inMemMap - email, ()))
          }
        }

    // 2- backing store for JWT tokens: BackingStore[F, id, JwtToken]
    val tokenStoreF = Ref.of[F, Map[SecureRandomId, JwtToken]](Map.empty).map { ref =>
      new BackingStore[F, SecureRandomId, JwtToken] {
        // Do not use a mutable map here, otherwise we're going to have a race conditions
        // We should use a concurrent coordination primitive in Cats Effect that will make all these operations atomic
        // and this tool is: 'Ref'
        override def get(id: SecureRandomId): OptionT[F, JwtToken] = OptionT(ref.get.map(_.get(id)))

        override def put(elem: JwtToken): F[JwtToken] = ref.modify(store => (store + (elem.id -> elem), elem))

        override def update(v: JwtToken): F[JwtToken] = put(v)

        override def delete(id: SecureRandomId): F[Unit] = ref.modify(store => (store - id, ()))
      }
    }

    // 3- hashing key
    val keyF = HMACSHA256.buildKey[F](securityConfig.secret.getBytes("UTF-8"))

    // 4- jwt authenticator
    for {
      key        <- keyF
      tokenStore <- tokenStoreF
      idStore    <- idStoreF
    } yield JWTAuthenticator.backed.inBearerToken(
      expiryDuration = securityConfig.jwtExpiryDuration,
      maxIdle = None,
      identityStore = idStore,
      tokenStore = tokenStore,
      signingKey = key
    )
  }

  // Concurrent + Sync == Async
  def apply[F[_]: Async: Logger](core: Core[F], securityConfig: SecurityConfig): Resource[F, HttpApi[F]] =
    Resource
      .eval(createAuthenticator(core.users, securityConfig))
      .map(authenticator => new HttpApi[F](core, authenticator))
}
