package rop.jobsboard.fixature

import cats.effect.IO
import rop.jobsboard.core.Users
import rop.jobsboard.domain.user.{Role, User}

trait UserFixture {

  val mockedUsers: Users[IO] = new Users[IO] {
    override def find(email: String): IO[Option[User]] =
      if (email == userEmail) IO.pure(Some(Person))
      else IO.pure(None)
    override def create(user: User): IO[String]       = IO.pure(user.email)
    override def update(user: User): IO[Option[User]] = IO.pure(Some(user))
    override def delete(email: String): IO[Boolean]   = IO.pure(true)
  }

  val Person: User = User(
    "someone@gmail.com",
    "somepassowrd",
    Some("Someone"),
    Some("His lastname"),
    Some("Corp"),
    Role.ADMIN
  )
  val userEmail: String = Person.email
  val somePassword      = "secretpassword"

  val AnotherUser: User = User(
    "another@gmail.com",
    "anotherpassword",
    Some("Another"),
    Some("Lastname"),
    Some("Corp 2"),
    Role.RECRUITER
  )
  val anotherUserEmail: String = AnotherUser.email
  val anotherUserPassword      = "someoneelsepassword"

  val NewUser: User = User(
    "newuser@gmail.com",
    "simplepassword",
    Some("John"),
    Some("Doe"),
    Some("Some company"),
    Role.RECRUITER
  )

  val UpdatedUser: User = User(
    "someone@gmail.com",
    "$2a$10$PUD6CznGVHntJFsOOeV4NezBgBUs6irV3sC9fa6ufc0xp9VLYyHZ.",
    Some("User"),
    Some("His lastname"),
    Some("Adobe"),
    Role.RECRUITER
  )
}
