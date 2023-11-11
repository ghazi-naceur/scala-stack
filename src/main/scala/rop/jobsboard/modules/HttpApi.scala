package rop.jobsboard.modules

import cats.*
import cats.effect.*
import cats.implicits.*
import org.http4s.*
import org.http4s.dsl.*
import org.http4s.dsl.impl.*
import org.http4s.server.*
import org.typelevel.log4cats.Logger
import rop.jobsboard.http.routes.{HealthRoutes, JobRoutes}

class HttpApi[F[_]: Concurrent: Logger] private (core: Core[F]) {

  private val healthRoutes = HealthRoutes[F].routes
  private val jobRoutes    = JobRoutes[F](core.jobs).routes

  val endpoints: HttpRoutes[F] = Router(
    "/api" -> (healthRoutes <+> jobRoutes)
  )
}

object HttpApi {
  def apply[F[_]: Concurrent: Logger](core: Core[F]): Resource[F, HttpApi[F]] =
    Resource.pure(new HttpApi[F](core))
}
