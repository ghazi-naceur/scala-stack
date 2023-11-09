package rop.jobsboard.http

import cats.*
import cats.implicits.*
import cats.effect.*
import org.http4s.*
import org.http4s.dsl.*
import org.http4s.dsl.impl.*
import org.http4s.server.*
import rop.jobsboard.http.routes.{HealthRoutes, JobRoutes}

class HttpApi[F[_]: Concurrent] private {

  private val healthRoutes = HealthRoutes[F].routes
  private val jobRoutes    = JobRoutes[F].routes

  val endpoints: HttpRoutes[F] = Router(
    "/api" -> (healthRoutes <+> jobRoutes)
  )
}

object HttpApi {
  def apply[F[_]: Concurrent] = new HttpApi[F]
}
