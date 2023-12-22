package rop.jobsboard.core

import cats.data.OptionT
import cats.effect.*
import cats.implicits.*
import org.typelevel.log4cats.Logger
import rop.jobsboard.config.SecurityConfig
import rop.jobsboard.domain.security.*
import rop.jobsboard.domain.auth.*
import rop.jobsboard.domain.user.*
import tsec.authentication.{AugmentedJWT, BackingStore, IdentityStore, JWTAuthenticator}
import tsec.common.SecureRandomId
import tsec.mac.jca.HMACSHA256
import tsec.passwordhashers.jca.BCrypt
import tsec.passwordhashers.PasswordHash

trait Auth[F[_]] {

  def login(email: String, password: String): F[Option[User]]
  def signup(newUserInfo: NewUserInfo): F[Option[User]]
  def changePassword(email: String, newPasswordInfo: NewPasswordInfo): F[Either[String, Option[User]]]
  def delete(email: String): F[Boolean]
  def sendPasswordRecoveryToken(from: String, to: String): F[Unit]
  def recoverPasswordFromToken(from: String, to: String, token: String, newPassword: String): F[Boolean]
}

class LiveAuth[F[_]: Async: Logger] private (users: Users[F], tokens: Tokens[F], emails: Emails[F]) extends Auth[F] {
  override def login(email: String, password: String): F[Option[User]] =
    for {
      potentialUser <- users.find(email)
      potentialValidatedUser <- potentialUser.filterA(user =>
        BCrypt.checkpwBool[F](password, PasswordHash[BCrypt](user.hashedPassword))
      )
      // 'filterA' allows to return the wrapper type of the boolean predication function (BCrypt.checkpwBool[F])
    } yield potentialValidatedUser

  override def signup(newUserInfo: NewUserInfo): F[Option[User]] =
    users.find(newUserInfo.email).flatMap {
      case Some(value) => None.pure[F]
      case None =>
        for {
          // hash the new password
          hashedPassword <- BCrypt.hashpw[F](newUserInfo.password)
          user <- User(
            newUserInfo.email,
            hashedPassword,
            newUserInfo.firstName,
            newUserInfo.lastName,
            newUserInfo.company,
            Role.RECRUITER
          ).pure[F]
          // create a new user in the db
          _ <- users.create(user)
        } yield Some(user)
    }

  override def changePassword(email: String, newPasswordInfo: NewPasswordInfo): F[Either[String, Option[User]]] = {
    def checkAndUpdate(user: User, oldPassword: String, newPassword: String): F[Either[String, Option[User]]] =
      for {
        passwordCheck <- BCrypt.checkpwBool[F](newPasswordInfo.oldPassword, PasswordHash[BCrypt](user.hashedPassword))
        updatedResult <-
          if (passwordCheck) updateUser(user, newPassword).map(Right(_))
          else Left("Invalid password").pure[F]
      } yield updatedResult

    users.find(email).flatMap {
      case Some(user) =>
        val NewPasswordInfo(oldPassword, newPassword) = newPasswordInfo
        checkAndUpdate(user, oldPassword, newPassword)
      case None => Right(None).pure[F]
    }
  }

  override def delete(email: String): F[Boolean] =
    users.delete(email)

  override def sendPasswordRecoveryToken(from: String, to: String): F[Unit] =
    tokens.getToken(to).flatMap {
      case Some(token) => emails.sendPasswordRecoveryEmail(from, to, token)
      case None        => ().pure[F]
    }

  override def recoverPasswordFromToken(from: String, to: String, token: String, newPassword: String): F[Boolean] =
    for {
      potentialUser <- users.find(to)
      tokenIsValid  <- tokens.checkToken(to, token)
      result <- (potentialUser, tokenIsValid) match {
        case (Some(user), true) => updateUser(user, newPassword).map(_.nonEmpty)
        case _                  => false.pure[F]
      }
    } yield result

  private def updateUser(user: User, newPassword: String): F[Option[User]] =
    for {
      // hash the new password
      hashedPassword <- BCrypt.hashpw[F](newPassword)
      updatedUser    <- users.update(user.copy(hashedPassword = hashedPassword))
    } yield updatedUser
}

object LiveAuth {
  def apply[F[_]: Async: Logger](users: Users[F], tokens: Tokens[F], emails: Emails[F]): F[LiveAuth[F]] =
    new LiveAuth[F](users, tokens, emails).pure[F]
}
