package rop.jobsboard.modules

import cats.effect.MonadCancelThrow
import rop.jobsboard.core.{Auth, Jobs, LiveAuth, LiveEmails, LiveJobs, LiveTokens, LiveUsers, Users}
import cats.effect.*
import cats.implicits.*
import doobie.*
import doobie.hikari.HikariTransactor
import doobie.implicits.*
import doobie.util.*
import rop.jobsboard.domain.Job.JobInfo
import org.typelevel.log4cats.Logger
import rop.jobsboard.config.{EmailServiceConfig, SecurityConfig, TokenConfig}

final class Core[F[_]] private (val jobs: Jobs[F], val users: Users[F], val auth: Auth[F])

// modules spin-up order: postgres -> jobs -> core -> httpApi -> Application
object Core {

  def apply[F[_]: Async: Logger](
      xa: Transactor[F],
      tokenConfig: TokenConfig,
      emailServiceConfig: EmailServiceConfig
  ): Resource[F, Core[F]] = {
    val coreF = for {
      jobs   <- LiveJobs[F](xa)
      users  <- LiveUsers[F](xa)
      tokens <- LiveTokens[F](users)(xa, tokenConfig)
      emails <- LiveEmails[F](emailServiceConfig)
      auth   <- LiveAuth[F](users, tokens, emails)
    } yield new Core(jobs, users, auth)

    Resource.eval(coreF)
  }
}
