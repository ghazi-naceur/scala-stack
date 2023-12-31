package rop.jobsboard.playground

import cats.effect.*
import doobie.*
import doobie.hikari.HikariTransactor
import doobie.implicits.*
import doobie.util.*
import rop.jobsboard.core.LiveJobs
import rop.jobsboard.domain.Job.JobInfo
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.io.StdIn

object JobPlayground extends IOApp.Simple {

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  val postgresResource: Resource[IO, HikariTransactor[IO]] = for {
    ec <- ExecutionContexts.fixedThreadPool(32)
    xa <- HikariTransactor.newHikariTransactor[IO](
      "org.postgresql.Driver",
      "jdbc:postgresql:board",
      "docker",
      "docker",
      ec
    )
  } yield xa

  val jobInfo: JobInfo = JobInfo.minimal(
    company = "EnisoCorp",
    title = "Scala developer",
    description = "This is a job description",
    externalUrl = "https://gmail.com",
    remote = true,
    location = "Amsterdam"
  )
  override def run: IO[Unit] = {
    postgresResource.use { xa =>
      for {
        jobs      <- LiveJobs[IO](xa)
        _         <- IO(println("Ready. Next...")) *> IO(StdIn.readLine())
        id        <- jobs.create("someone@gmail.com", jobInfo)
        _         <- IO(println("Next...")) *> IO(StdIn.readLine())
        list      <- jobs.all()
        _         <- IO(println(s"All jobs: $list. \nNext...")) *> IO(StdIn.readLine())
        _         <- jobs.update(id, jobInfo.copy(company = "Other company", title = "Java developer"))
        newJob    <- jobs.find(id)
        _         <- IO(println(s"New job: $newJob. \nNext...")) *> IO(StdIn.readLine())
        _         <- jobs.delete(id)
        _         <- IO(println(s"Deleted job: $newJob. \nNext...")) *> IO(StdIn.readLine())
        remaining <- jobs.all()
        _         <- IO(println(s"Remaining jobs: $remaining. \nNext...")) *> IO(StdIn.readLine())
      } yield ()
    }
  }
}
