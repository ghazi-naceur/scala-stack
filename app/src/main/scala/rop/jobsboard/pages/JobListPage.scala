package rop.jobsboard.pages
import cats.effect.IO
import rop.jobsboard.App
import rop.jobsboard.common.{Constants, Endpoint}
import rop.jobsboard.domain.Job.{Job, JobFilter}
import rop.jobsboard.pages.JobListPage.Commands
import io.circe.parser.*
import io.circe.generic.auto.*
import rop.jobsboard.components.FilterPanel
import tyrian.Html.*
import tyrian.http.Method.Post
import tyrian.http.{HttpError, Method, Response, Status}
import tyrian.{Cmd, Html}

final case class JobListPage(
    filterPanel: FilterPanel = FilterPanel(),
    jobs: List[Job] = List(),
    canLoadMore: Boolean = true,
    status: Option[Page.Status] = Some(Page.Status("Loading", Page.StatusKind.LOADING))
) extends Page {

  import JobListPage.*
  override def initCmd: Cmd[IO, App.Msg] =
    filterPanel.initCmd |+| Commands.getJobs()

  override def update(msg: App.Msg): (Page, Cmd[IO, App.Msg]) = msg match {
    case AddJobs(list, canLoadMore) =>
      (setSuccessStatus("Loaded").copy(jobs = this.jobs ++ list, canLoadMore = canLoadMore), Cmd.None)
    case SetErrorStatus(error) => (setErrorStatus(error), Cmd.None)
    case LoadMoreJobs          => (this, Commands.getJobs(offset = jobs.length))
    case msg: FilterPanel.Msg => // delegating filter panel messages to FilterPanel
      val (newFilterPanel, cmd) = filterPanel.update(msg)
      (this.copy(filterPanel = newFilterPanel), cmd)
    case _ => (this, Cmd.None)
  }

  override def view(): Html[App.Msg] = {
    div(`class` := "jobs-list-page")(
      filterPanel.view(),
      div(`class` := "jobs-container")(
        jobs.map(job => renderJob(job)) ++ maybeRenderLoadMore
      )
    )
  }

  private def renderJob(job: Job) =
    div(`class` := "job-card")(
      div(`class` := "job-card-img")(
        img(
          `class` := "job-logo",
          src     := job.jobInfo.image.getOrElse(""),
          alt     := job.jobInfo.title
        )
      ),
      div(`class` := "job-card-content")(
        h4(s"${job.jobInfo.company} - ${job.jobInfo.title}")
      ),
      div(`class` := "job-card-apply")(
        a(href := job.jobInfo.externalUrl, target := "blank")("Apply")
      )
    )

  private def maybeRenderLoadMore: Option[Html[App.Msg]] = status.map { s =>
    div(`class` := "load-more-action")(
      s match {
        case Page.Status(_, Page.StatusKind.LOADING)   => div("Loading...")
        case Page.Status(error, Page.StatusKind.ERROR) => div(error)
        case Page.Status(_, Page.StatusKind.SUCCESS) =>
          if (canLoadMore)
            button(`type` := "button", onClick(LoadMoreJobs))("Load more")
          else div("All jobs were loaded")
      }
    )
  }

  // removing the return type 'Page' to get rid off the reassignment error in the 'update' method
  private def setErrorStatus(message: String) =
    this.copy(status = Some(Page.Status(message, Page.StatusKind.ERROR)))

  // removing the return type 'Page' to get rid off the reassignment error in the 'update' method
  private def setSuccessStatus(message: String) =
    this.copy(status = Some(Page.Status(message, Page.StatusKind.SUCCESS)))
}

object JobListPage {

  trait Msg extends App.Msg

  // state
  case class SetErrorStatus(error: String)                  extends Msg
  case class AddJobs(list: List[Job], canLoadMore: Boolean) extends Msg

  // actions
  case object LoadMoreJobs extends Msg

  object Endpoints {
    def getJobs(limit: Int = Constants.defaultPageSize, offset: Int = 0) = new Endpoint[Msg] {

      override val location: String = Constants.Endpoints.jobs + s"?limit=$limit&offset=$offset"
      override val method: Method   = Post
      override val onResponse: Response => Msg = Endpoint.onResponse[List[Job], Msg](
        list => AddJobs(list, canLoadMore = offset == 0 || list.nonEmpty),
        SetErrorStatus(_)
      )
      override val onError: HttpError => Msg = e => SetErrorStatus(e.toString)
    }
  }
  object Commands {
    def getJobs(filter: JobFilter = JobFilter(), limit: Int = Constants.defaultPageSize, offset: Int = 0): Cmd[IO, Msg] =
      Endpoints.getJobs(limit, offset).call(filter)
  }
}
