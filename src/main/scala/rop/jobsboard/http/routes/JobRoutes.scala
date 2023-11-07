package rop.jobsboard.http.routes

import cats.*
import cats.implicits.*
import org.http4s.*
import org.http4s.dsl.*
import org.http4s.dsl.impl.*
import org.http4s.server.*

class JobRoutes[F[_]: Monad] private extends Http4sDsl[F] {

  // REST endpoints
  // http POST '/jobs?offset=x&limit=y { filters }' // todo add query params and filters
  private val allJobsRoute: HttpRoutes[F] = HttpRoutes.of[F] { case POST -> Root =>
    Ok("allJobsRoute")
  }

  // http GET '/jobs/uuid'
  private val findJobRoute: HttpRoutes[F] = HttpRoutes.of[F] { case GET -> Root / UUIDVar(id) =>
    Ok("findJobRoute")
  }

  // http POST '/jobs/create { jobInfo }'
  private val createJobRoute: HttpRoutes[F] = HttpRoutes.of[F] { case POST -> Root / "create" =>
    Ok("createJobRoute")
  }

  // http PUT '/jobs/uuid { jobInfo }'
  private val updateJobRoute: HttpRoutes[F] = HttpRoutes.of[F] { case PUT -> Root / UUIDVar(id) =>
    Ok("updateJobRoute")
  }

  // http DELETE '/jobs/uuid'
  private val deleteJobRoute: HttpRoutes[F] = HttpRoutes.of[F] { case DELETE -> Root / UUIDVar(id) =>
    Ok("deleteJobRoute")
  }

  val routes: HttpRoutes[F] = Router(
    "/jobs" -> (allJobsRoute <+> findJobRoute <+> createJobRoute <+> updateJobRoute <+> deleteJobRoute)
  )
}

object JobRoutes {
  def apply[F[_]: Monad] = new JobRoutes[F]
}
