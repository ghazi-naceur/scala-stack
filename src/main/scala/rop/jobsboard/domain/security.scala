package rop.jobsboard.domain

import org.http4s.Response
import rop.jobsboard.domain.user.*
import tsec.authentication.{AugmentedJWT, JWTAuthenticator, SecuredRequest}
import tsec.mac.jca.HMACSHA256

// internal to the server
object security {
  type Crypto              = HMACSHA256
  type JwtToken            = AugmentedJWT[Crypto, String]
  type Authenticator[F[_]] = JWTAuthenticator[F, String, User, Crypto]
  type AuthRoute[F[_]]     = PartialFunction[SecuredRequest[F, User, JwtToken], F[Response[F]]]
}
