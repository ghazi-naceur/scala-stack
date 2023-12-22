package rop.jobsboard.modules

import cats.effect.{Async, Resource}
import doobie.hikari.HikariTransactor
import doobie.util.ExecutionContexts
import rop.jobsboard.config.PostgresConfig

object Database {

  def makePostgresResource[F[_]: Async](postgresConfig: PostgresConfig): Resource[F, HikariTransactor[F]] = for {
    ec <- ExecutionContexts.fixedThreadPool(postgresConfig.nbThreads)
    xa <- HikariTransactor.newHikariTransactor[F](
      "org.postgresql.Driver",
      postgresConfig.url,
      postgresConfig.user,
      postgresConfig.password,
      ec
    )
  } yield xa
}
