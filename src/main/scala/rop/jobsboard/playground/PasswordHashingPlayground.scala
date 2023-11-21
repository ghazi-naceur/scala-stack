package rop.jobsboard.playground

import cats.effect.{IO, IOApp}
import tsec.passwordhashers.PasswordHash
import tsec.passwordhashers.jca.BCrypt

object PasswordHashingPlayground extends IOApp.Simple {
  override def run: IO[Unit] = {
    BCrypt.hashpw[IO]("somepassword").flatMap(IO.println) *>
      BCrypt
        .checkpwBool[IO]("somepassword", PasswordHash[BCrypt]("$2a$10$uxXr1sHrOnK3bZ2La3cQueLt6gWqxPSnEQErtACifH5PsiKZODgQm"))
        .flatMap(IO.println)
  }
}
