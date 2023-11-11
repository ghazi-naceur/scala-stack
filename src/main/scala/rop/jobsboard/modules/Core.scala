package rop.jobsboard.modules

import cats.effect.MonadCancelThrow
import rop.jobsboard.core.Jobs
import cats.effect.*
import doobie.*
import doobie.hikari.HikariTransactor
import doobie.implicits.*
import doobie.util.*
import rop.jobsboard.core.LiveJobs
import rop.jobsboard.domain.Job.JobInfo

final class Core[F[_]] private (val jobs: Jobs[F])

// modules spin-up order: postgres -> jobs -> core -> httpApi -> Application
object Core {

  def postgresResource[F[_]: Async]: Resource[F, HikariTransactor[F]] = for {
    ec <- ExecutionContexts.fixedThreadPool(32)
    xa <- HikariTransactor.newHikariTransactor[F](
      "org.postgresql.Driver", // todo move to config
      "jdbc:postgresql:board",
      "docker",
      "docker",
      ec
    )
  } yield xa

  def apply[F[_]: Async]: Resource[F, Core[F]] =
    postgresResource[F]
      .evalMap(postgres => LiveJobs[F](postgres))
      .map(jobs => new Core[F](jobs))
}
