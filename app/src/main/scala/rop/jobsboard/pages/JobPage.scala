package rop.jobsboard.pages

import cats.effect.IO
import rop.jobsboard.App
import rop.jobsboard.common.{Constants, Endpoint}
import rop.jobsboard.domain.Job.Job
import io.circe.generic.auto.*
import rop.jobsboard.pages.Page.StatusKind
import tyrian.Html.*
import tyrian.http.Method.Get
import tyrian.http.{HttpError, Method, Response, Status}
import tyrian.{Cmd, Html}
import laika.api.*
import laika.format.*

final case class JobPage(
    id: String,
    maybeJob: Option[Job] = None,
    status: Page.Status = Page.Status.LOADING
) extends Page {

  import JobPage.*

  val markdownTransformer: Transformer = Transformer
    .from(Markdown)
    .to(HTML)
    .build

  override def initCmd: Cmd[IO, App.Msg] =
    Commands.getJob(id)

  override def update(msg: App.Msg): (Page, Cmd[IO, App.Msg]) = msg match {
    case SetError(error) => (setErrorStatus(error), Cmd.None)
    case SetJob(job)     => (setSuccessStatus("Job loaded successfully").copy(maybeJob = Some(job)), Cmd.None)
    case _               => (this, Cmd.None)
  }

  override def view(): Html[App.Msg] = maybeJob match {
    case Some(job) => renderJobPage(job)
    case None      => renderNoJobPage
  }

  private def renderJobPage(job: Job) =
    div(`class` := "job-page")(
      div(`class` := "job-hero")(
        img(
          `class` := "job-logo",
          src     := job.jobInfo.image.getOrElse(""),
          alt     := job.jobInfo.title
        ),
        h1(s"${job.jobInfo.company} - ${job.jobInfo.title}")
      ),
      div(`class` := "job-overview")(
        renderJobDetails(job)
      ),
      renderJobDescription(job),
      // 'target := "blank"' to open the link in a new tab
      a(href := job.jobInfo.externalUrl, `class` := "job-apply-action", target := "blank")("Apply")
    )
  private def renderNoJobPage = status.kind match {
    case StatusKind.LOADING => div("Loading...")
    case StatusKind.ERROR   => div("This job doesn't exist")
    case StatusKind.SUCCESS => div("This should never happen. Server is running, but no job is displayed...")
  }

  private def renderJobDetails(job: Job) = {

    val fullLocationString = job.jobInfo.country match {
      case Some(country) => s"${job.jobInfo.location}, $country"
      case None          => job.jobInfo.location
    }

    val currency = job.jobInfo.currency.getOrElse("")

    val fullSalaryString = (job.jobInfo.salaryLo, job.jobInfo.salaryHi) match {
      case (Some(lo), Some(hi)) => s"$currency $lo-$hi"
      case (Some(lo), None)     => s"> $currency $lo"
      case (None, Some(hi))     => s"Up to $currency $hi"
      case (None, None)         => "Unspecified salary"
    }

    def renderDetail(value: String) = {
      if (value.isEmpty) div()
      else li(`class` := "job-detail-value")(value)
    }

    div(`class` := "job-details")(
      ul(`class` := "job-detail")(
        renderDetail(fullLocationString),
        renderDetail(fullSalaryString),
        renderDetail(job.jobInfo.seniority.getOrElse("All levels")),
        renderDetail(job.jobInfo.tags.getOrElse(List()).mkString(", "))
      )
    )
  }

  private def renderJobDescription(job: Job) = {
    val descriptionHtml = markdownTransformer.transform(job.jobInfo.description) match {
      case Right(html) => html
      case Left(error) =>
        """
          | Error when trying to show markdown for this job description.
          | Just hit the apply button (your application will be processed), and let the recruiter about the markdown issue
          |""".stripMargin

    }
    div(`class` := "job-description")().innerHtml(descriptionHtml) // 'innerHtml' makes sure to interpret the code as HTML
    // by overriding the actual html content inside this div
  }

  // removing the return type 'Page' to get rid off the reassignment error in the 'update' method
  private def setErrorStatus(message: String) =
    this.copy(status = Page.Status(message, Page.StatusKind.ERROR))

  // removing the return type 'Page' to get rid off the reassignment error in the 'update' method
  private def setSuccessStatus(message: String) =
    this.copy(status = Page.Status(message, Page.StatusKind.SUCCESS))
}

object JobPage {

  trait Msg                          extends App.Msg
  case class SetError(error: String) extends Msg
  case class SetJob(job: Job)        extends Msg

  object Endpoints {
    def getJob(id: String): Endpoint[Msg] = new Endpoint[Msg] {
      override val location: String = Constants.Endpoints.jobs + s"/$id"
      override val method: Method   = Get
      override val onResponse: Response => Msg =
        Endpoint.onResponse[Job, Msg](SetJob(_), SetError(_))
      override val onError: HttpError => Msg = e => SetError(e.toString)
    }
  }

  object Commands {
    def getJob(id: String): Cmd[IO, Msg] = Endpoints.getJob(id).call()
  }
}
