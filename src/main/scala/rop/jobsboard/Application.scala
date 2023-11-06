package rop.jobsboard

import cats.{Applicative, Monad}
import cats.effect.{IO, IOApp, MonadCancel}
import cats.*
import cats.data.Kleisli
import cats.implicits.*
import io.circe.generic.auto.*
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.headers.*
import org.http4s.{Header, HttpRoutes}
import org.http4s.dsl.*
import org.http4s.dsl.impl.*
import org.http4s.server.*
import org.http4s.ember.server.EmberServerBuilder
import org.typelevel.ci.CIString
import pureconfig.ConfigSource
import pureconfig.error.ConfigReaderException
import rop.jobsboard.config.*
import rop.jobsboard.config.syntax.*
import rop.jobsboard.http.routes.HealthRoutes

/*
  1- Add plain health endpoint to our app
  2- Add minimal configuration
  3- Basic http server layout
 */

object Application extends IOApp.Simple {

  override def run: IO[Unit] = {
    ConfigSource.default
      .loadF[IO, EmberConfig]
      .flatMap(config =>
        EmberServerBuilder
          .default[IO]
          .withHost(config.host)
          .withPort(config.port)
          .withHttpApp(HealthRoutes[IO].routes.orNotFound)
          .build
          .use(_ => IO.println("Server ready") *> IO.never)
      )
  }
}
