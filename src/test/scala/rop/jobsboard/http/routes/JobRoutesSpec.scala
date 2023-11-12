package rop.jobsboard.http.routes

// These 2 imports should placed before the http4s imports
import io.circe.generic.auto.*
import org.http4s.circe.CirceEntityCodec.*

import cats.effect.*
import cats.implicits.*
import org.http4s.*
import org.http4s.implicits.*
import org.http4s.dsl.*

import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import rop.jobsboard.fixature.JobFixture
import rop.jobsboard.core.Jobs
import rop.jobsboard.domain.Job.*
import org.scalatest.freespec.*

import java.util.UUID
class JobRoutesSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers with Http4sDsl[IO] with JobFixture {

  val jobs: Jobs[IO] = new Jobs[IO] {
    override def create(ownerEmail: String, jobInfo: JobInfo): IO[UUID] = IO.pure(NewJobUuid)

    override def all(): IO[List[Job]] = IO.pure(List(TestJob))

    override def find(id: UUID): IO[Option[Job]] =
      if (id == TestJobUuid) IO.pure(Some(TestJob))
      else IO.pure(None)

    override def update(id: UUID, jobInfo: JobInfo): IO[Option[Job]] =
      if (id == TestJobUuid) IO.pure(Some(UpdatedTestJob))
      else IO.pure(None)

    override def delete(id: UUID): IO[Int] =
      if (id == TestJobUuid) IO.pure(1)
      else IO.pure(0)
  }

  given logger: Logger[IO]      = Slf4jLogger.getLogger[IO]
  val jobRoutes: HttpRoutes[IO] = JobRoutes[IO](jobs).routes

  "JobRoutes" - {
    "should return a job with a given id" in {
      for {
        // simulate an HTTP request
        responseOk <- jobRoutes.orNotFound.run(
          Request(method = Method.GET, uri = uri"/jobs/843df718-ec6e-4d49-9289-f799c0f40064")
        )
        responseNotFound <- jobRoutes.orNotFound.run(
          Request(method = Method.GET, uri = uri"/jobs/843df718-ec6e-4d49-9289-f799c0f99999")
        )
        retrieved <- responseOk.as[Job]
      } yield {
        responseOk.status shouldBe Status.Ok
        retrieved shouldBe TestJob
        responseNotFound.status shouldBe Status.NotFound
      }
    }

    "should return all jobs" in {
      for {
        // simulate an HTTP request
        response <- jobRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"/jobs")
        )
        retrieved <- response.as[List[Job]]
      } yield {
        response.status shouldBe Status.Ok
        retrieved shouldBe List(TestJob)
      }
    }

    "should create a new job" in {
      for {
        // simulate an HTTP request
        response <- jobRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"/jobs/create")
            .withEntity(TestJob.jobInfo)
        )
        retrieved <- response.as[UUID]
      } yield {
        response.status shouldBe Status.Created
        retrieved shouldBe NewJobUuid
      }
    }

    "should update a job" in {
      for {
        // simulate an HTTP request
        responseOk <- jobRoutes.orNotFound.run(
          Request(method = Method.PUT, uri = uri"/jobs/843df718-ec6e-4d49-9289-f799c0f40064")
            .withEntity(UpdatedTestJob.jobInfo)
        )
        responseNotFound <- jobRoutes.orNotFound.run(
          Request(method = Method.PUT, uri = uri"/jobs/843df718-ec6e-4d49-9289-f799c0f99999")
            .withEntity(UpdatedTestJob.jobInfo)
        )
      } yield {
        responseOk.status shouldBe Status.Ok
        responseNotFound.status shouldBe Status.NotFound
      }
    }

    "should delete a job" in {
      for {
        // simulate an HTTP request
        responseOk <- jobRoutes.orNotFound.run(
          Request(method = Method.DELETE, uri = uri"/jobs/843df718-ec6e-4d49-9289-f799c0f40064")
        )
        responseNotFound <- jobRoutes.orNotFound.run(
          Request(method = Method.DELETE, uri = uri"/jobs/843df718-ec6e-4d49-9289-f799c0f99999")
        )
      } yield {
        responseOk.status shouldBe Status.Ok
        responseNotFound.status shouldBe Status.NotFound
      }
    }
  }

}
