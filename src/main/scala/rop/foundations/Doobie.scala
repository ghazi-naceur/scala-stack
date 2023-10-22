package rop.foundations

import cats.effect.kernel.Resource
import cats.effect.{IO, IOApp, MonadCancelThrow}
import doobie.hikari.HikariTransactor
import doobie.implicits.*
import doobie.postgres.implicits.toPostgresMonadErrorOps
import doobie.util.ExecutionContexts
import doobie.util.transactor.Transactor

// 3
object Doobie extends IOApp.Simple {

  case class Student(id: Int, name: String)

  val xa: Transactor[IO] = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver", // JDBC connector
    "jdbc:postgresql://localhost:5432/demo", // if it's localhost, we can simply write "jdbc:postgresql:demo",
    "docker",
    "docker"
  )

  // Read
  def findAllStudentNames: IO[List[String]] = {
    val query  = sql"select name from students".query[String]
    val action = query.to[List] // a list of names is expected
    action.transact(xa)
  }

  // Write
  def saveStudent(id: Int, name: String): IO[Int] = {
    val query  = sql"insert into students(id, name) values ($id, $name)"
    val action = query.update.run
    action.transact(xa)
  }

  // Read as Case Classes with Fragments
  def findStudentsByInitial(letter: String): IO[List[Student]] = {
    val selectPart = fr"select id, name" // fr: query fragment
    val fromPart   = fr"from students"
    val wherePart  = fr"where left(name, 1) = $letter"

    // left(name, 1) == left most character out of name field == extract a substring from the name field with just 1 character
    //    which must be equal to the input letter === similar to 'startWith'

    val statement = selectPart ++ fromPart ++ wherePart
    val action    = statement.query[Student].to[List]
    action.transact(xa)
  }

  // Organize code:
  trait Students[F[_]] {
    // repository
    def findById(id: Int): F[Option[Student]]
    def findAll: F[List[Student]]
    def create(name: String): F[Int]
  }

  object Students {
    def make[F[_]: MonadCancelThrow](xa: Transactor[F]): Students[F] = new Students[F] {
      override def findById(id: Int): F[Option[Student]] = {
        sql"select id, name from students where id = $id"
          .query[Student]
          .option
          .transact(xa)
      }

      override def findAll: F[List[Student]] = {
        sql"select id, name from students"
          .query[Student]
          .to[List]
          .transact(xa)
      }

      override def create(name: String): F[Int] = {
        sql"insert into students(name) values ($name)".update
          .withUniqueGeneratedKeys[Int]("id")
          .transact(xa)
      }
    }
  }

  val postgresResource: Resource[IO, HikariTransactor[IO]] = for {
    ec <- ExecutionContexts.fixedThreadPool[IO](16)
    xa <- HikariTransactor.newHikariTransactor[IO](
      "org.postgresql.Driver",
      "jdbc:postgresql://localhost:5432/demo",
      "docker",
      "docker",
      ec
    )
  } yield xa

  val finalExample: IO[Unit] = postgresResource.use { xa =>
    val studentsRepo = Students.make[IO](xa)
    for {
      id     <- studentsRepo.create("netero")
      netero <- studentsRepo.findById(id)
      _      <- IO.println(s"'$netero' was just saved in DB")
    } yield ()
  }

  override def run: IO[Unit] = {
//    saveStudent(3, "itchigo").map(println)
//    findAllStudentNames.map(println)
//    findStudentsByInitial("t").map(println)
    finalExample
  }
}
