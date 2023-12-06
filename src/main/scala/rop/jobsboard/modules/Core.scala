package rop.jobsboard.modules

import cats.effect.MonadCancelThrow
import rop.jobsboard.core.{Auth, Jobs, LiveAuth, LiveJobs, LiveUsers}
import cats.effect.*
import cats.implicits.*
import doobie.*
import doobie.hikari.HikariTransactor
import doobie.implicits.*
import doobie.util.*
import rop.jobsboard.domain.Job.JobInfo
import org.typelevel.log4cats.Logger
import rop.jobsboard.config.SecurityConfig

final class Core[F[_]] private (val jobs: Jobs[F], val auth: Auth[F])

// modules spin-up order: postgres -> jobs -> core -> httpApi -> Application
object Core {

  def apply[F[_]: Async: Logger](xa: Transactor[F])(securityConfig: SecurityConfig): Resource[F, Core[F]] = {
    val coreF = for {
      jobs  <- LiveJobs[F](xa)
      users <- LiveUsers[F](xa)
      auth  <- LiveAuth[F](users)(securityConfig)
    } yield new Core(jobs, auth)

    Resource.eval(coreF)
  }
}
