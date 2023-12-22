package rop.jobsboard.fixature

import cats.effect.IO
import rop.jobsboard.core.Users
import rop.jobsboard.domain.user.{NewUserInfo, Role, User}

trait UserFixture {

  val mockedUsers: Users[IO] = new Users[IO] {
    override def find(email: String): IO[Option[User]] =
      if (email == someEmail) IO.pure(Some(Person))
      else IO.pure(None)
    override def create(user: User): IO[String]       = IO.pure(user.email)
    override def update(user: User): IO[Option[User]] = IO.pure(Some(user))
    override def delete(email: String): IO[Boolean]   = IO.pure(true)
  }

  val Person: User = User(
    "someone@gmail.com",
    "$2a$10$uxXr1sHrOnK3bZ2La3cQueLt6gWqxPSnEQErtACifH5PsiKZODgQm", // == somepassword
    Some("Someone"),
    Some("His lastname"),
    Some("Corp"),
    Role.ADMIN
  )
  val someEmail: String = Person.email
  val somePassword      = "somepassword"

  val AnotherUser: User = User(
    "another@gmail.com",
    "$2a$10$6jl0rnj3uU8/.p97cUYPqu8idZRlD7ijuidQnb3K6ML/0cPaUAxoq", // == anotherpassword
    Some("Another"),
    Some("Lastname"),
    Some("Corp 2"),
    Role.RECRUITER
  )
  val anotherUserEmail: String = AnotherUser.email
  val anotherUserPassword      = "anotherpassword"

  val PersonInfo: NewUserInfo = NewUserInfo(
    Person.email,
    somePassword,
    Person.firstName,
    Person.lastName,
    Person.company
  )

  val AnotherUserInfo: NewUserInfo = NewUserInfo(
    AnotherUser.email,
    anotherUserPassword,
    AnotherUser.firstName,
    AnotherUser.lastName,
    AnotherUser.company
  )

  val NewUser: User = User(
    "newuser@gmail.com",
    "$2a$10$nQy.OgvzMdkhdyXQA8mKa.BFw2SNaU7j287CAfbU1wIO0XzW7dgPq", // == simplepassword
    Some("John"),
    Some("Doe"),
    Some("Some company"),
    Role.RECRUITER
  )

  val UpdatedUser: User = User(
    "someone@gmail.com",
    "$2a$10$mtFeYR3wCMp5n0BsK.LdXulT2KA/wpfFxH0r4gskzvkqvdofSW4XC", // == updatedpassword
    Some("User"),
    Some("His lastname"),
    Some("Adobe"),
    Role.RECRUITER
  )
}
