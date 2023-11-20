package rop.jobsboard.core

import cats.effect.*
import cats.implicits.*
import org.typelevel.log4cats.Logger
import rop.jobsboard.domain.security.*
import rop.jobsboard.domain.auth.*
import rop.jobsboard.domain.user.*
import tsec.authentication.{AugmentedJWT, JWTAuthenticator}
import tsec.mac.jca.HMACSHA256

trait Auth[F[_]] {

  def login(email: String, password: String): F[Option[JwtToken]]
  def signup(newUserInfo: NewUserInfo): F[Option[User]]
  def changePassword(email: String, newPasswordInfo: NewPasswordInfo): F[Either[String, Option[User]]]
}

class LiveAuth[F[_]: MonadCancelThrow: Logger] private (users: Users[F], authenticator: Authenticator[F]) extends Auth[F] {
  override def login(email: String, password: String): F[Option[JwtToken]] = ???

  override def signup(newUserInfo: NewUserInfo): F[Option[User]] = ???

  override def changePassword(email: String, newPasswordInfo: NewPasswordInfo): F[Either[String, Option[User]]] = ???
}

object LiveAuth {
  def apply[F[_]: MonadCancelThrow: Logger](
      users: Users[F],
      authenticator: Authenticator[F]
  ): F[LiveAuth[F]] = new LiveAuth[F](users, authenticator).pure[F]
}
