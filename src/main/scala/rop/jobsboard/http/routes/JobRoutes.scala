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
import rop.jobsboard.http.responses.*
import rop.jobsboard.core.*
import org.typelevel.log4cats.Logger
import scala.collection.mutable
import java.util.UUID

class JobRoutes[F[_]: Concurrent: Logger] private (jobs: Jobs[F]) extends Http4sDsl[F] {

  // database

  // REST endpoints
  // http POST '/jobs?offset=x&limit=y { filters }' // todo add query params and filters
  // http post localhost:4041/api/jobs
  private val allJobsRoute: HttpRoutes[F] = HttpRoutes.of[F] { case POST -> Root =>
    for {
      jobsList <- jobs.all()
      response <- Ok(jobsList)
    } yield response
  }

  // http GET '/jobs/uuid'
  // http get localhost:4041/api/jobs/9d50c421-4ddf-4165-9941-a531106003f4
  private val findJobRoute: HttpRoutes[F] = HttpRoutes.of[F] { case GET -> Root / UUIDVar(id) =>
    for {
      job <- jobs.find(id)
      response <- job match
        case Some(job) => Ok(job)
        case None      => NotFound(FailureResponse(s"Job '$id' not found"))
    } yield response
  }

  // http POST '/jobs/create { jobInfo }'
  // http post localhost:4041/api/jobs/create company='EnisoCorp' title='Scala developer' description='This is a job description' externalUrl='https://google.com' remote=true location='Amsterdam'
  // http post localhost:4041/api/jobs/create < src/main/resources/payloads/jobinfo.json
  import rop.jobsboard.logging.syntax.*
  private val createJobRoute: HttpRoutes[F] = HttpRoutes.of[F] { case req @ POST -> Root / "create" =>
    for {
      _        <- Logger[F].info("Trying to add job")
      jobInfo  <- req.as[JobInfo].logError(e => s"Parsing payload failed: $e")
      jobId    <- jobs.create("todo@gmail.com", jobInfo)
      response <- Created(jobId)
    } yield response
  }

  // http PUT '/jobs/uuid { jobInfo }'
  // http put localhost:4041/api/jobs/9d50c421-4ddf-4165-9941-a531106003f4 < src/main/resources/payloads/jobinfo2.json
  private val updateJobRoute: HttpRoutes[F] = HttpRoutes.of[F] { case req @ PUT -> Root / UUIDVar(id) =>
    for {
      jobInfo      <- req.as[JobInfo]
      potentialJob <- jobs.update(id, jobInfo)
      response <- potentialJob match
        case Some(job) => Ok()
        case None      => NotFound(FailureResponse(s"Cannot update job '$id', because it is not found"))
    } yield response
  }

  // http DELETE '/jobs/uuid'
  // http delete localhost:4041/api/jobs/0e9d6971-6d59-4478-aa23-77e94b346b1d
  private val deleteJobRoute: HttpRoutes[F] = HttpRoutes.of[F] { case req @ DELETE -> Root / UUIDVar(id) =>
    jobs.find(id).flatMap {
      case Some(job) =>
        for {
          _        <- jobs.delete(job.id)
          response <- Ok()
        } yield response
      case None => NotFound(FailureResponse(s"Cannot delete job '$id', because it is not found"))
    }
  }

  val routes: HttpRoutes[F] = Router(
    "/jobs" -> (allJobsRoute <+> findJobRoute <+> createJobRoute <+> updateJobRoute <+> deleteJobRoute)
  )
}

object JobRoutes {
  def apply[F[_]: Concurrent: Logger](jobs: Jobs[F]) = new JobRoutes[F](jobs)
}
