package rop.jobsboard.http.routes

import cats.*
import cats.effect.*
import cats.implicits.*
import io.circe.generic.auto.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.*
import org.http4s.dsl.*
import org.http4s.dsl.impl.*
import org.http4s.server.*
import rop.jobsboard.domain.Job.*
import rop.jobsboard.http.responses.FailureResponse

import scala.collection.mutable
import java.util.UUID

class JobRoutes[F[_]: Concurrent] private extends Http4sDsl[F] {

  // database
  private val database = mutable.Map[UUID, Job]()

  // REST endpoints
  // http POST '/jobs?offset=x&limit=y { filters }' // todo add query params and filters
  private val allJobsRoute: HttpRoutes[F] = HttpRoutes.of[F] { case POST -> Root =>
    Ok(database.values)
  }

  // http GET '/jobs/uuid'
  private val findJobRoute: HttpRoutes[F] = HttpRoutes.of[F] { case GET -> Root / UUIDVar(id) =>
    database.get(id) match
      case Some(job) => Ok(job)
      case None      => NotFound(FailureResponse(s"Job '$id' not found"))
  }

  // http POST '/jobs/create { jobInfo }'
  private def createJob(jobInfo: JobInfo): F[Job] =
    Job(
      id = UUID.randomUUID(),
      date = System.currentTimeMillis(),
      ownerEmail = "someone@gmail.com",
      jobInfo = jobInfo,
      active = true
    ).pure[F]

  private val createJobRoute: HttpRoutes[F] = HttpRoutes.of[F] { case req @ POST -> Root / "create" =>
    for {
      jobInfo  <- req.as[JobInfo]
      job      <- createJob(jobInfo)
      response <- Created(job.id)
    } yield response
  }

  // http PUT '/jobs/uuid { jobInfo }'
  private val updateJobRoute: HttpRoutes[F] = HttpRoutes.of[F] { case req @ PUT -> Root / UUIDVar(id) =>
    database.get(id) match
      case Some(job) =>
        for {
          jobInfo  <- req.as[JobInfo]
          _        <- database.put(id, job.copy(jobInfo = jobInfo)).pure[F]
          response <- Ok()
        } yield response
      case None => NotFound(FailureResponse(s"Cannot update job '$id', because it is not found"))
  }

  // http DELETE '/jobs/uuid'
  private val deleteJobRoute: HttpRoutes[F] = HttpRoutes.of[F] { case req @ DELETE -> Root / UUIDVar(id) =>
    database.get(id) match
      case Some(job) =>
        for {
          _        <- database.remove(id).pure[F]
          response <- Ok()
        } yield response

      case None => NotFound(FailureResponse(s"Cannot delete job '$id', because it is not found"))
  }

  val routes: HttpRoutes[F] = Router(
    "/jobs" -> (allJobsRoute <+> findJobRoute <+> createJobRoute <+> updateJobRoute <+> deleteJobRoute)
  )
}

object JobRoutes {
  def apply[F[_]: Concurrent] = new JobRoutes[F]
}
