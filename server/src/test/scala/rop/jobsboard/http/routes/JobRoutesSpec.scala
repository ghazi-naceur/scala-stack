package rop.jobsboard.http.routes

// These 2 imports should placed before the http4s imports
import io.circe.generic.auto.*
import org.http4s.circe.CirceEntityCodec.*
//
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
import rop.jobsboard.fixature.{JobFixture, SecuredRouteFixture}
import rop.jobsboard.core.Jobs
import rop.jobsboard.domain.Job.*
import org.scalatest.freespec.*
import rop.jobsboard.domain.pagination.Pagination
import rop.jobsboard.core.{LiveStripe, Stripe}
import com.stripe.model.checkout.Session
import com.stripe.param.checkout.SessionCreateParams

import java.util.UUID
class JobRoutesSpec
    extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers
    with Http4sDsl[IO]
    with JobFixture
    with SecuredRouteFixture {

  val defaultJobFilter: JobFilter = JobFilter(companies = List("Some company"))

  val jobs: Jobs[IO] = new Jobs[IO] {
    override def create(ownerEmail: String, jobInfo: JobInfo): IO[UUID] = IO.pure(NewJobUuid)

    override def all(): IO[List[Job]] = IO.pure(List(TestJob))

    override def all(filter: JobFilter, pagination: Pagination): IO[List[Job]] =
      if (filter.remote) IO.pure(List())
      else IO.pure(List(TestJob))

    override def find(id: UUID): IO[Option[Job]] =
      if (id == TestJobUuid) IO.pure(Some(TestJob))
      else IO.pure(None)

    override def update(id: UUID, jobInfo: JobInfo): IO[Option[Job]] =
      if (id == TestJobUuid) IO.pure(Some(UpdatedTestJob))
      else IO.pure(None)

    override def activate(id: UUID): IO[Int] = IO.pure(1)

    override def delete(id: UUID): IO[Int] =
      if (id == TestJobUuid) IO.pure(1)
      else IO.pure(0)

    override def possibleFilters(): IO[JobFilter] = IO(defaultJobFilter)

  }

  val stripe: Stripe[IO] = new LiveStripe[IO]("key", "price", "example.com/success", "example.com/fail", "webhooksecret") {
    override def createCheckoutSession(jobId: String, userEmail: String): IO[Option[Session]] =
      IO.pure(Some(Session.create(SessionCreateParams.builder().build())))

    override def handleWebhookEvent[A](payload: String, signature: String, action: String => IO[A]): IO[Option[A]] =
      IO.pure(None)
  }

  given logger: Logger[IO]      = Slf4jLogger.getLogger[IO]
  val jobRoutes: HttpRoutes[IO] = JobRoutes[IO](jobs, stripe).routes

  "JobRoutes" - {
    "should return a job with a given id" in {
      for {
        // simulate an HTTP request
        responseOk <- jobRoutes.orNotFound.run(
          Request[IO](method = Method.GET, uri = uri"/jobs/843df718-ec6e-4d49-9289-f799c0f40064")
        )
        responseNotFound <- jobRoutes.orNotFound.run(
          Request[IO](method = Method.GET, uri = uri"/jobs/843df718-ec6e-4d49-9289-f799c0f99999")
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
          Request[IO](method = Method.POST, uri = uri"/jobs")
            .withEntity(JobFilter()) // empty filter
        )
        retrieved <- response.as[List[Job]]
      } yield {
        response.status shouldBe Status.Ok
        retrieved shouldBe List(TestJob)
      }
    }

    "should return all jobs that satisfy a certain filter" in {
      for {
        // simulate an HTTP request
        response <- jobRoutes.orNotFound.run(
          Request[IO](method = Method.POST, uri = uri"/jobs")
            .withEntity(JobFilter(remote = true))
        )
        retrieved <- response.as[List[Job]]
      } yield {
        response.status shouldBe Status.Ok
        retrieved shouldBe List()
      }
    }

    "should create a new job" in {
      for {
        jwtToken <- mockedAuthenticator.create(someEmail)
        response <- jobRoutes.orNotFound.run(
          Request[IO](method = Method.POST, uri = uri"/jobs/create")
            .withBearerToken(jwtToken)
            .withEntity(TestJob.jobInfo)
        )
        retrieved <- response.as[UUID]
      } yield {
        response.status shouldBe Status.Created
        retrieved shouldBe NewJobUuid
      }
    }

    "should update a job that exists" in {
      for {
        jwtToken <- mockedAuthenticator.create(someEmail)
        responseOk <- jobRoutes.orNotFound.run(
          Request[IO](method = Method.PUT, uri = uri"/jobs/843df718-ec6e-4d49-9289-f799c0f40064")
            .withBearerToken(jwtToken)
            .withEntity(UpdatedTestJob.jobInfo)
        )
        responseNotFound <- jobRoutes.orNotFound.run(
          Request[IO](method = Method.PUT, uri = uri"/jobs/843df718-ec6e-4d49-9289-f799c0f99999")
            .withBearerToken(jwtToken)
            .withEntity(UpdatedTestJob.jobInfo)
        )
      } yield {
        responseOk.status shouldBe Status.Ok
        responseNotFound.status shouldBe Status.NotFound
      }
    }

    "should unauthorize the update a job that the user (jwt_token) doesn't own" in {
      for {
        jwtToken <- mockedAuthenticator.create("unknown@gmail.com")
        response <- jobRoutes.orNotFound.run(
          Request[IO](method = Method.PUT, uri = uri"/jobs/843df718-ec6e-4d49-9289-f799c0f40064")
            .withBearerToken(jwtToken)
            .withEntity(UpdatedTestJob.jobInfo)
        )
      } yield {
        response.status shouldBe Status.Unauthorized
      }
    }

    "should delete a job" in {
      for {
        jwtToken <- mockedAuthenticator.create(someEmail)
        responseOk <- jobRoutes.orNotFound.run(
          Request[IO](method = Method.DELETE, uri = uri"/jobs/843df718-ec6e-4d49-9289-f799c0f40064")
            .withBearerToken(jwtToken)
        )
        responseNotFound <- jobRoutes.orNotFound.run(
          Request[IO](method = Method.DELETE, uri = uri"/jobs/843df718-ec6e-4d49-9289-f799c0f99999")
            .withBearerToken(jwtToken)
        )
      } yield {
        responseOk.status shouldBe Status.Ok
        responseNotFound.status shouldBe Status.NotFound
      }
    }

    "should surface all possible filters" in {
      for {
        response <- jobRoutes.orNotFound.run(
          Request[IO](method = Method.GET, uri = uri"/jobs/filters")
        )
        filter <- response.as[JobFilter]
      } yield filter shouldBe defaultJobFilter
    }
  }

}
