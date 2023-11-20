package rop.jobsboard.domain

import rop.jobsboard.domain.user.*
import tsec.authentication.{AugmentedJWT, JWTAuthenticator}
import tsec.mac.jca.HMACSHA256

// internal to the server
object security {
  type Crypto              = HMACSHA256
  type JwtToken            = AugmentedJWT[Crypto, String]
  type Authenticator[F[_]] = JWTAuthenticator[F, String, User, Crypto]
}
