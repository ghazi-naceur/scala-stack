package rop.jobsboard.fixature

import cats.data.*
import cats.effect.*
import cats.implicits.*
import org.http4s.headers.Authorization
import org.http4s.{AuthScheme, Credentials, Request}
import rop.jobsboard.domain.security.*
import rop.jobsboard.domain.user.*
import rop.jobsboard.fixature.UserFixture
import tsec.authentication.{IdentityStore, JWTAuthenticator, SecuredRequestHandler}
import tsec.jws.mac.JWTMac
import tsec.mac.jca.HMACSHA256

import scala.concurrent.duration.*
import scala.language.postfixOps

trait SecuredRouteFixture extends UserFixture {

  val mockedAuthenticator: Authenticator[IO] = {
    // key for hashing
    val key = HMACSHA256.unsafeGenerateKey

    // identity store for retrieving users (in-memory map to find a user by key)
    val idStore: IdentityStore[IO, String, User] = { (email: String) =>
      if (email == someEmail) OptionT.pure(Person)
      else if (email == anotherUserEmail) OptionT.pure(AnotherUser)
      else OptionT.none[IO, User]
    }
    // jwt authenticator
    JWTAuthenticator.unbacked.inBearerToken(
      1 day,   // expiration of tokens
      None,    // max idle time (optional)
      idStore, // identity store
      key      // hash key
    )
  }

  extension (r: Request[IO])
    def withBearerToken(jwtToken: JwtToken): Request[IO] =
      r.putHeaders {
        val jwtString = JWTMac.toEncodedString[IO, HMACSHA256](jwtToken.jwt)
        // Authorization: Bearer {jwtString}
        Authorization(Credentials.Token(AuthScheme.Bearer, jwtString))
      }

  given securedHandler: SecuredHandler[IO] = SecuredRequestHandler(mockedAuthenticator)
}
