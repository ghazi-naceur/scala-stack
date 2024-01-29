package rop.jobsboard.pages
import cats.effect.IO
import rop.jobsboard.App
import rop.jobsboard.common.{Constants, Endpoint}
import rop.jobsboard.domain.Job.{Job, JobFilter}
import rop.jobsboard.pages.JobListPage.Commands
import io.circe.parser.*
import io.circe.generic.auto.*
import rop.jobsboard.components.{Anchors, FilterPanel, JobComponents}
import tyrian.Html.*
import tyrian.http.Method.Post
import tyrian.http.{HttpError, Method, Response, Status}
import tyrian.{Cmd, Html}

final case class JobListPage(
    filterPanel: FilterPanel = FilterPanel(filterAction = FilterJobs(_)),
    jobFilter: JobFilter = JobFilter(),
    jobs: List[Job] = List(),
    canLoadMore: Boolean = true,
    status: Option[Page.Status] = Some(Page.Status.LOADING)
) extends Page {

  import JobListPage.*
  override def initCmd: Cmd[IO, App.Msg] =
    filterPanel.initCmd |+| Commands.getJobs()

  override def update(msg: App.Msg): (Page, Cmd[IO, App.Msg]) = msg match {
    case AddJobs(list, canLoadMore) =>
      (setSuccessStatus("Loaded").copy(jobs = this.jobs ++ list, canLoadMore = canLoadMore), Cmd.None)
    case SetErrorStatus(error) => (setErrorStatus(error), Cmd.None)
    case LoadMoreJobs          => (this, Commands.getJobs(filter = jobFilter, offset = jobs.length))
    case msg: FilterPanel.Msg => // delegating filter panel messages to FilterPanel
      val (newFilterPanel, cmd) = filterPanel.update(msg)
      (this.copy(filterPanel = newFilterPanel), cmd)
    case FilterJobs(selectedFilters) =>
      val newJobFilter = createJobFilter(selectedFilters)
      (this.copy(jobs = List(), jobFilter = newJobFilter), Commands.getJobs(filter = newJobFilter))
    case _ => (this, Cmd.None)
  }

  override def view(): Html[App.Msg] =
    section(`class` := "section-1")(
      div(`class` := "container job-list-hero")(
        h1(`class` := "job-list-title")("ROP Jobs board")
      ),
      div(`class` := "container")(
        div(`class` := "row jvm-recent-jobs-body")(
          div(`class` := "col-lg-4")(
            filterPanel.view()
          ),
          div(`class` := "col-lg-8")(
            jobs.map(job => JobComponents.card(job)) ++ maybeRenderLoadMore
          )
        )
      )
    )

  private def maybeRenderLoadMore: Option[Html[App.Msg]] = status.map { s =>
    div(`class` := "load-more-action")(
      s match {
        case Page.Status(_, Page.StatusKind.LOADING)   => div(`class` := "page-status-loading")("Loading...")
        case Page.Status(error, Page.StatusKind.ERROR) => div(`class` := "page-status-errors")(error)
        case Page.Status(_, Page.StatusKind.SUCCESS) =>
          if (canLoadMore)
            button(`type` := "button", `class` := "load-more-btn", onClick(LoadMoreJobs))("Load more")
          else div("All jobs were loaded")
      }
    )
  }

  private def createJobFilter(selectedFilters: Map[String, Set[String]]) =
    JobFilter(
      companies = selectedFilters.getOrElse("Companies", Set()).toList,
      locations = selectedFilters.getOrElse("Locations", Set()).toList,
      countries = selectedFilters.getOrElse("Countries", Set()).toList,
      seniorities = selectedFilters.getOrElse("Seniorities", Set()).toList,
      tags = selectedFilters.getOrElse("Tags", Set()).toList,
      maxSalary = Some(filterPanel.maxSalary).filter(_ > 0), // maxSalary has a default value: 0
      remote = filterPanel.remote
    )

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
  case object LoadMoreJobs                                         extends Msg
  case class FilterJobs(selectedFilters: Map[String, Set[String]]) extends Msg

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
