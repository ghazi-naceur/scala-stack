package rop.jobsboard.core

import cats.effect.*
import cats.effect.implicits.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import cats.effect.testing.scalatest.AsyncIOSpec
import org.postgresql.util.PSQLException
import org.scalatest.Inside
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import rop.jobsboard.domain.user.User
import rop.jobsboard.fixature.UserFixture

class UsersSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers with Inside with DoobieSpec with UserFixture {

  override val initScript: String = "sql/users.sql"

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  "Users 'algebra'" - {
    "should retrieve user by email" in {
      transactor.use { xa =>
        val program = for {
          users     <- LiveUsers[IO](xa)
          retrieved <- users.find("someone@gmail.com")
        } yield retrieved

        program.asserting(_ shouldBe Some(Person))
      }
    }

    "should return None if the email doesn't exist" in {
      transactor.use { xa =>
        val program = for {
          users     <- LiveUsers[IO](xa)
          retrieved <- users.find("unknown@gmail.com")
        } yield retrieved

        program.asserting(_ shouldBe None)
      }
    }

    "should create a new user" in {
      transactor.use { xa =>
        val program = for {
          users  <- LiveUsers[IO](xa)
          userId <- users.create(NewUser)
          potentialUser <- sql"SELECT * FROM users WHERE email = ${NewUser.email}"
            .query[User]
            .option
            .transact(xa)
        } yield (userId, potentialUser)

        program.asserting { case (userId, potentialUser) =>
          userId shouldBe NewUser.email
          potentialUser shouldBe Some(NewUser)
        }

      }
    }

    "should fail creating a new user if the email already exists" in {
      transactor.use { xa =>
        val program = for {
          users  <- LiveUsers[IO](xa)
          userId <- users.create(Person).attempt // IO[Either[Throwable, String]]
        } yield userId

        program.asserting { outcome =>
          inside(outcome) {
            case Left(e) => e shouldBe a[PSQLException]
            case _       => fail()
          }
        }
      }
    }

    "should return None when updating a user that does not exist" in {
      transactor.use { xa =>
        val program = for {
          users         <- LiveUsers[IO](xa)
          potentialUser <- users.update(NewUser)
        } yield potentialUser

        program.asserting { _ shouldBe None }
      }
    }

    "should update an existent user" in {
      transactor.use { xa =>
        val program = for {
          users         <- LiveUsers[IO](xa)
          potentialUser <- users.update(UpdatedUser)
        } yield potentialUser

        program.asserting { _ shouldBe Some(UpdatedUser) }
      }
    }

    "should delete an existent user" in {
      transactor.use { xa =>
        val program = for {
          users          <- LiveUsers[IO](xa)
          deletionResult <- users.delete("someone@gmail.com")
          potentialUser <- sql"SELECT * FROM users WHERE email = 'someone@gmail.com'"
            .query[User]
            .option
            .transact(xa)
        } yield (deletionResult, potentialUser)

        program.asserting { case (deletionResult, potentialUser) =>
          deletionResult shouldBe true
          potentialUser shouldBe None
        }
      }
    }

    "should return false when trying to delete a non-existent user" in {
      transactor.use { xa =>
        val program = for {
          users <- LiveUsers[IO](xa)
          bool  <- users.delete("unknown@gmail.com")
        } yield bool

        program.asserting { _ shouldBe false }
      }
    }
  }
}
