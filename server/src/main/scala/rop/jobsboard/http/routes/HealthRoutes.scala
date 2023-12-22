package rop.jobsboard.http.routes

import cats.*
import org.http4s.*
import org.http4s.dsl.*
import org.http4s.dsl.impl.*
import org.http4s.server.*

class HealthRoutes[F[_]: Monad] private extends Http4sDsl[F] {

  // REST endpoints
  // http GET '/health'
  // http GET '/something'
  private val healthRoute: HttpRoutes[F] = HttpRoutes.of[F] { case GET -> Root =>
    Ok("All going great!")
  }

  val routes: HttpRoutes[F] = Router(
    "/health" -> healthRoute
  )
}

object HealthRoutes {
  def apply[F[_]: Monad] = new HealthRoutes[F]
}
