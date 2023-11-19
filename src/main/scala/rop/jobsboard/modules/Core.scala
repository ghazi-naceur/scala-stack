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
import org.typelevel.log4cats.Logger

final class Core[F[_]] private (val jobs: Jobs[F])

// modules spin-up order: postgres -> jobs -> core -> httpApi -> Application
object Core {

  def apply[F[_]: Async: Logger](xa: Transactor[F]): Resource[F, Core[F]] =
    Resource
      .eval(LiveJobs[F](xa))
      .map(jobs => new Core[F](jobs))
}
