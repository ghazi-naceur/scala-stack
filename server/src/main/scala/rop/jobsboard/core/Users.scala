package rop.jobsboard.core

import cats.*
import cats.implicits.*
import cats.effect.*
import doobie.*
import doobie.util.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import cats.effect.MonadCancelThrow
import doobie.util.transactor.Transactor
import org.typelevel.log4cats.Logger
import rop.jobsboard.domain.user.User

trait Users[F[_]] {

  def find(email: String): F[Option[User]]
  def create(user: User): F[String]
  def update(user: User): F[Option[User]]
  def delete(email: String): F[Boolean]
}

final class LiveUsers[F[_]: MonadCancelThrow: Logger] private (xa: Transactor[F]) extends Users[F] {
  override def find(email: String): F[Option[User]] = {
    Logger[F].info(s"Searching for user $email") *>
      sql"SELECT * FROM users WHERE email = $email"
        .query[User]
        .option
        .transact(xa)
  }

  override def create(user: User): F[String] =
    sql"""
          INSERT INTO users (email, hashedPassword, firstName, lastName, company, role)
          VALUES (${user.email}, ${user.hashedPassword}, ${user.firstName}, ${user.lastName}, ${user.company}, ${user.role})
       """.update.run
      .transact(xa)
      .map(_ => user.email)

  override def update(user: User): F[Option[User]] =
    for {
      _ <-
        sql"""
           UPDATE users
           SET
              hashedPassword = ${user.hashedPassword},
              firstName = ${user.firstName},
              lastName = ${user.lastName},
              company = ${user.company},
              role = ${user.role}
           WHERE
              email = ${user.email}
         """.update.run
          .transact(xa)
      potentialUser <- find(user.email)
    } yield potentialUser

  override def delete(email: String): F[Boolean] =
    sql"DELETE FROM users WHERE email = $email".update.run
      .transact(xa)
      .map(_ > 0)

}

object LiveUsers {

  def apply[F[_]: MonadCancelThrow: Logger](xa: Transactor[F]): F[LiveUsers[F]] =
    new LiveUsers[F](xa).pure[F]
}
