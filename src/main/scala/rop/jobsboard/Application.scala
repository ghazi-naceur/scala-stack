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
import rop.jobsboard.http.HttpApi
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object Application extends IOApp.Simple {

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  override def run: IO[Unit] = {
    ConfigSource.default
      .loadF[IO, EmberConfig]
      .flatMap(config =>
        EmberServerBuilder
          .default[IO]
          .withHost(config.host)
          .withPort(config.port)
          .withHttpApp(HttpApi[IO].endpoints.orNotFound)
          .build
          .use(_ => IO.println("Server ready") *> IO.never)
      )
  }
}
