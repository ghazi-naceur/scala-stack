package rop.jobsboard.core

import cats.effect.*
import cats.effect.kernel.Resource
import doobie.hikari.HikariTransactor
import doobie.util.*
import doobie.implicits.*
import doobie.util.transactor.*
import org.testcontainers.containers.PostgreSQLContainer

trait DoobieSpec {

  // to be implemented by whatever test case interacts with the DB
  val initScript: String

  val postgres: Resource[IO, PostgreSQLContainer[Nothing]] = {
    val acquire = IO {
      val container: PostgreSQLContainer[Nothing] =
        new PostgreSQLContainer("postgres").withInitScript(initScript)
      container.start()
      container
    }

    val release = (container: PostgreSQLContainer[Nothing]) => IO(container.stop())

    Resource.make(acquire)(release)
  }

  // Set up Postgres transactor
  val transactor: Resource[IO, Transactor[IO]] = for {
    db <- postgres
    ce <- ExecutionContexts.fixedThreadPool[IO](1)
    xa <- HikariTransactor.newHikariTransactor[IO](
      "org.postgresql.Driver",
      db.getJdbcUrl,  // will be set by the postgres docker container
      db.getUsername, // will be set by the postgres docker container
      db.getPassword, // will be set by the postgres docker container
      ce
    )
  } yield xa
}
