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
import rop.jobsboard.components.JobComponents

import scala.scalajs.*
import scala.scalajs.js.*            // can be used to refer to native js functions: js.native
import scala.scalajs.js.annotation.* // annotation to be able to use any JS lib

@js.native
@JSGlobal() // This will enable 'js.native'
class Moment extends js.Object {
  // This class allows to call the moment lib functions from ScalaJS
  def format(): String = js.native // This actually invokes the 'format()' function of moment lib in JS
  // 'js.native' maps between the 'format()' function from the 'moment' lib and this 'format()' method from ScalaJS.
  // => Surface out any JS function as a new Scala method in this class

  // See documentation: https://momentjs.com/docs/#/displaying/fromnow/
  def fromNow(): String = js.native
}

@js.native
@JSImport("moment", JSImport.Default) // runs the JS statement: "import moment from 'moment';"
object MomentLib extends js.Object {

  // This 'apply' method is the equivalent to the JS instantiation of moment lib: "moment()" (See documentation: https://momentjs.com/docs/#/use-it/)
  def apply(): Moment = js.native

  // See documentation: https://momentjs.com/docs/#/parsing/unix-timestamp/
  // Be aware that 'unix' function does not need an instance of Moment, so it can be invoked. In other terms:
  // in JS: 'moment.unix(some long value)' and it's not 'moment().unix(some long value)'... that's why the unix function
  // in defined here in the object 'MomentLib', not in the Class 'Moment'.. because 'unix' is in the same level as the
  // apply "()" function
  def unix(date: Long): Moment = js.native
}

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
    div(`class` := "container-fluid the-rock")(
      div(`class` := "row jvm-jobs-details-top-card")(
        div(`class` := "col-md-12 p-0")(
          div(`class` := "jvm-jobs-details-card-profile-img")(
            JobComponents.renderJobPicture(job)
          ),
          div(`class` := "jvm-jobs-details-card-profile-title")(
            h1(s"${job.jobInfo.company} - ${job.jobInfo.title}"),
            div(`class` := "jvm-jobs-details-card-profile-job-details-company-and-location")(
              JobComponents.renderJobSummary(job)
            )
          ),
          div(`class` := "jvm-jobs-details-card-apply-now-btn")(
            a(href := job.jobInfo.externalUrl, target := "blank")(
              button(`type` := "button", `class` := "btn btn-warning")("Apply now")
            ),
            p(
//              MomentLib().format() // or MomentLib.apply().format()
              MomentLib.unix(job.date / 1000).fromNow() // 'unix' is accepting seconds, that why we're dividing by 1000
            )
          )
        )
      ),
      div(`class` := "container-fluid")(
        div(`class` := "container")(
          div(`class` := "markdown-body overview-section")(
            renderJobDescription(job)
          )
        ),
        div(`class` := "container")(
          div(`class` := "rok-last")(
            div(`class` := "row")(
              div(`class` := "col-md-6 col-sm-6 col-6")(
                span(`class` := "rock-apply")("Apply for this job.")
              ),
              div(`class` := "col-md-6 col-sm-6 col-6")(
                a(href := job.jobInfo.externalUrl, target := "blank")(
                  button(`type` := "button", `class` := "rock-apply-btn")("Apply now")
                )
              )
            )
          )
        )
      )
    )

  private def renderNoJobPage = {
    div(`class` := "container-fluid the-rock")(
      div(`class` := "row jvm-jobs-details-top-card")(
        status.kind match {
          case StatusKind.LOADING => h1("Loading...")
          case StatusKind.ERROR   => h1("This job doesn't exist")
          case StatusKind.SUCCESS => h1("This should never happen. Server is running, but no job is displayed...")
        }
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
