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
import org.http4s.server.middleware.CORS
import org.typelevel.ci.CIString
import pureconfig.ConfigSource
import pureconfig.error.ConfigReaderException
import rop.jobsboard.config.*
import rop.jobsboard.config.syntax.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import rop.jobsboard.modules.{Core, Database, HttpApi}

object Application extends IOApp.Simple {

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  override def run: IO[Unit] = {
    ConfigSource.default
      .loadF[IO, AppConfig]
      .flatMap { case AppConfig(postgresConfig, emberConfig, securityConfig, tokenConfig, emailServiceConfig, stripeConfig) =>
        val appResource = for {
          xa      <- Database.makePostgresResource[IO](postgresConfig)
          core    <- Core[IO](xa, tokenConfig, emailServiceConfig, stripeConfig)
          httpApi <- HttpApi[IO](core, securityConfig)
          server <-
            EmberServerBuilder
              .default[IO]
              .withHost(emberConfig.host)
              .withPort(emberConfig.port)
              .withHttpApp(
                CORS(httpApi.endpoints).orNotFound
              ) // TODO remove this when deploying, because the default CORS is allowing all requests (potential security issues)
              .build
        } yield server

        appResource.use(_ => IO.println("Server ready") *> IO.never)
      }
  }
}
