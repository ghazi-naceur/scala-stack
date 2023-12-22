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
import org.typelevel.log4cats.Logger

import scala.collection.mutable
import scala.language.implicitConversions
import java.util.UUID
import rop.jobsboard.domain.Job.*
import rop.jobsboard.http.responses.*
import rop.jobsboard.core.*
import rop.jobsboard.domain.pagination.Pagination
import rop.jobsboard.domain.security.{allRoles, AuthRoute, Authenticator, SecuredHandler, restrictedTo}
import rop.jobsboard.http.validation.syntax.*
import tsec.authentication.*

class JobRoutes[F[_]: Concurrent: Logger: SecuredHandler] private (jobs: Jobs[F]) extends HttpValidationDsl[F] {

  object OffsetQueryParam extends OptionalQueryParamDecoderMatcher[Int]("offset")
  object LimitQueryParam  extends OptionalQueryParamDecoderMatcher[Int]("limit")

  // REST endpoints
  // http POST '/jobs?limit=x&offset=y { filters }'
  // http post localhost:4041/api/jobs
  private val allJobsRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ POST -> Root :? LimitQueryParam(limit) +& OffsetQueryParam(offset) =>
      for {
        filter   <- req.as[JobFilter]
        jobsList <- jobs.all(filter, Pagination(limit, offset))
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
  private val createJobRoute: AuthRoute[F] = { case req @ POST -> Root / "create" asAuthed _ =>
    req.request.validate[JobInfo] { jobInfo =>
      for {
        jobId    <- jobs.create("todo@gmail.com", jobInfo)
        response <- Created(jobId)
      } yield response
    }
  }

  // http PUT '/jobs/uuid { jobInfo }'
  // http put localhost:4041/api/jobs/9d50c421-4ddf-4165-9941-a531106003f4 < src/main/resources/payloads/jobinfo2.json
  private val updateJobRoute: AuthRoute[F] = { case req @ PUT -> Root / UUIDVar(id) asAuthed user =>
    req.request.validate[JobInfo] { jobInfo =>
      jobs.find(id).flatMap {
        case None => NotFound(FailureResponse(s"Cannot update job '$id', because it is not found"))
        case Some(job) if user.owns(job) || user.isAdmin => jobs.update(id, jobInfo) *> Ok()
        case _                                           => Forbidden(FailureResponse("You can only update your own jobs"))
      }
    }
  }

  // http DELETE '/jobs/uuid'
  // http delete localhost:4041/api/jobs/0e9d6971-6d59-4478-aa23-77e94b346b1d
  private val deleteJobRoute: AuthRoute[F] = { case req @ DELETE -> Root / UUIDVar(id) asAuthed user =>
    jobs.find(id).flatMap {
      case None => NotFound(FailureResponse(s"Cannot delete job '$id', because it is not found"))
      case Some(job) if user.owns(job) || user.isAdmin => jobs.delete(job.id) *> Ok()
      case _                                           => Forbidden(FailureResponse("You can only delete your own jobs"))
    }
  }

  val unauthedRoutes: HttpRoutes[F] = allJobsRoute <+> findJobRoute
  val authedRoutes: HttpRoutes[F] = SecuredHandler[F].liftService(
    createJobRoute.restrictedTo(allRoles)
      |+| updateJobRoute.restrictedTo(allRoles)
      |+| deleteJobRoute.restrictedTo(allRoles)
  )

  val routes: HttpRoutes[F] = Router(
    "/jobs" -> (unauthedRoutes <+> authedRoutes)
  )
}

object JobRoutes {
  def apply[F[_]: Concurrent: Logger: SecuredHandler](jobs: Jobs[F]) = new JobRoutes[F](jobs)
}
