package rop.jobsboard.pages
import cats.effect.IO
import cats.syntax.traverse.*
import rop.jobsboard.App
import rop.jobsboard.common.{Constants, Endpoint}
import rop.jobsboard.core.Session
import rop.jobsboard.domain.Job.JobInfo
import org.scalajs.dom.{File, FileReader}
import io.circe.generic.auto.*
import io.circe.parser.*
import tyrian.*
import tyrian.Html.*
import tyrian.cmds.Logger
import tyrian.http.*
import tyrian.http.Method.Post

import scala.util.Try

case class PostJobPage(
    company: String = "",
    title: String = "",
    description: String = "",
    externalUrl: String = "",
    remote: Boolean = false,
    location: String = "",
    salaryLo: Option[Int] = None,
    salaryHi: Option[Int] = None,
    currency: Option[String] = None,
    country: Option[String] = None,
    tags: Option[String] = None,
    image: Option[String] = None,
    seniority: Option[String] = None,
    other: Option[String] = None,
    status: Option[Page.Status] = None
) extends FormPage("Post Job", status) {

  import PostJobPage.*

  override def view(): Html[App.Msg] = {
    if (Session.isActive) super.view()
    else renderInvalidPage
  }

  override def update(msg: App.Msg): (Page, Cmd[IO, App.Msg]) = msg match {
    case UpdateCompany(c)               => (this.copy(company = c), Cmd.None)
    case UpdateTitle(t)                 => (this.copy(title = t), Cmd.None)
    case UpdateDescription(d)           => (this.copy(description = d), Cmd.None)
    case UpdateExternalUrl(e)           => (this.copy(externalUrl = e), Cmd.None)
    case ToggleRemote                   => (this.copy(remote = !this.remote), Cmd.None)
    case UpdateLocation(l)              => (this.copy(location = l), Cmd.None)
    case UpdateSalaryLo(s)              => (this.copy(salaryLo = Some(s)), Cmd.None)
    case UpdateSalaryHi(s)              => (this.copy(salaryHi = Some(s)), Cmd.None)
    case UpdateCurrency(c)              => (this.copy(currency = Some(c)), Cmd.None)
    case UpdateCountry(c)               => (this.copy(country = Some(c)), Cmd.None)
    case UpdateImageFile(potentialFile) => (this, Commands.loadFile(potentialFile))
    case UpdateImage(p)                 => (this.copy(image = p), Logger.consoleLog[IO](s"This is the image content: $p"))
    case UpdateTags(t)                  => (this.copy(tags = Some(t)), Cmd.None)
    case UpdateSeniority(s)             => (this.copy(seniority = Some(s)), Cmd.None)
    case UpdateOther(o)                 => (this.copy(other = Some(o)), Cmd.None)
    case AttemptPostJob =>
      (
        this,
        Commands.postJob(
          company,
          title,
          description,
          externalUrl,
          remote,
          location,
          salaryLo,
          salaryHi,
          currency,
          country,
          tags,
          image,
          seniority,
          other
        )
      )
    case PostJobError(error)   => (setErrorStatus(error), Cmd.None)
    case PostJobSuccess(jobId) => (setSuccessStatus("Success!"), Logger.consoleLog[IO](s"Job $jobId is posted"))
    case _                     => (this, Cmd.None)
  }

  override protected def renderFormContent(): List[Html[App.Msg]] = List(
    renderInput("Company", "company", "text", isRequired = true, UpdateCompany(_)),
    renderInput("Title", "title", "text", isRequired = true, UpdateTitle(_)),
    renderTextArea("Description", "description", isRequired = true, UpdateDescription(_)),
    renderInput("ExternalUrl", "externalUrl", "text", isRequired = true, UpdateExternalUrl(_)),
    renderInput("Remote", "remote", "checkbox", isRequired = true, _ => ToggleRemote),
    renderInput("Location", "location", "text", isRequired = true, UpdateLocation(_)),
    renderInput("Salary low", "salaryLo", "number", isRequired = false, value => UpdateSalaryLo(parseNumber(value))),
    renderInput("Salary high", "salaryHi", "number", isRequired = false, value => UpdateSalaryHi(parseNumber(value))),
    renderInput("Currency", "currency", "text", isRequired = false, UpdateCurrency(_)),
    renderInput("Country", "country", "text", isRequired = false, UpdateCountry(_)),
    renderImageUploadInput("Logo", "logo", image, UpdateImageFile(_)),
    renderInput("Tags", "tags", "text", isRequired = false, UpdateTags(_)),
    renderInput("Seniority", "seniority", "text", isRequired = false, UpdateSeniority(_)),
    renderInput("Other", "other", "text", isRequired = false, UpdateOther(_)),
    button(`type` := "button", onClick(AttemptPostJob))("Post job")
  )

  private def renderInvalidPage =
    div(
      h1("Post job"),
      div("You need to be logged in to post a job")
    )

  private def setErrorStatus(message: String): Page =
    this.copy(status = Some(Page.Status(message, Page.StatusKind.ERROR)))

  private def setSuccessStatus(message: String): Page =
    this.copy(status = Some(Page.Status(message, Page.StatusKind.SUCCESS)))

  private def parseNumber(value: String) =
    Try(value.toInt).getOrElse(0)

}

object PostJobPage {

  trait Msg extends App.Msg

  case class UpdateCompany(company: String)                    extends Msg
  case class UpdateTitle(title: String)                        extends Msg
  case class UpdateDescription(description: String)            extends Msg
  case class UpdateExternalUrl(externalUrl: String)            extends Msg
  case object ToggleRemote                                     extends Msg
  case class UpdateLocation(location: String)                  extends Msg
  case class UpdateSalaryLo(salaryLo: Int)                     extends Msg
  case class UpdateSalaryHi(salaryHi: Int)                     extends Msg
  case class UpdateCurrency(currency: String)                  extends Msg
  case class UpdateCountry(country: String)                    extends Msg
  case class UpdateImageFile(potentialFile: Option[File])      extends Msg
  case class UpdateImage(potentialFileContent: Option[String]) extends Msg
  case class UpdateTags(tags: String)                          extends Msg
  case class UpdateSeniority(seniority: String)                extends Msg
  case class UpdateOther(other: String)                        extends Msg

  // actions
  case object AttemptPostJob extends Msg

  // statuses
  case class PostJobError(error: String)   extends Msg
  case class PostJobSuccess(jobId: String) extends Msg

  object Endpoints {
    val postJob: Endpoint[Msg] = new Endpoint[Msg] {
      override val location: String = Constants.Endpoints.postJob
      override val method: Method   = Post
      override val onResponse: Response => Msg = response =>
        response.status match {
          case Status(s, _) if s >= 200 && s < 300 =>
            val jobId = response.body
            PostJobSuccess(jobId)
          case Status(401, _) =>
            PostJobError("You are not authorized to post a job.")
          case Status(s, _) if s >= 400 && s < 500 =>
            val jsonError = response.body
            val parsed    = parse(jsonError).flatMap(_.hcursor.get[String]("error"))
            parsed match {
              case Right(errorFromServer) => PostJobError(errorFromServer)
              case Left(error)            => PostJobError(s"Error $error.")
            }
          case _ => PostJobError("Unknown reply from server.")
        }
      override val onError: HttpError => Msg = e => PostJobError(e.toString)
    }
  }

  object Commands {
    def postJob(
        company: String,
        title: String,
        description: String,
        externalUrl: String,
        remote: Boolean,
        location: String,
        salaryLo: Option[Int],
        salaryHi: Option[Int],
        currency: Option[String],
        country: Option[String],
        tags: Option[String],
        image: Option[String],
        seniority: Option[String],
        other: Option[String]
    ): Cmd[IO, Msg] = Endpoints.postJob.callAuthorized(
      JobInfo(
        company = company,
        title = title,
        description = description,
        externalUrl = externalUrl,
        remote = remote,
        location = location,
        salaryLo = salaryLo,
        salaryHi = salaryHi,
        currency = currency,
        country = country,
        tags = tags.map(_.split(",").map(_.trim).toList),
        image = image,
        seniority = seniority,
        other = other
      )
    )

    def loadFile(potentialFile: Option[File]) = {
      Cmd.Run[IO, Option[String], Msg](
        /*
        For the logo upload feature, and since reading a file using JS is asynchronous, we need to use 'IO.async_' in Cats Effect
        that will allow us to surface an asynchronous computation based on callback (which the read file method from JS) into Cats Effect
         */
        // run the effect here that return an Option[String]
        // Option[File] => Option[String] => Msg
        // Option[File].traverse(file => IO[String]) => IO[Option[String]] => Msg
        potentialFile.traverse { file =>
          IO.async_ { callback =>
            // create a reader
            val reader = new FileReader
            // set the onload method
            reader.onload = _ => callback(Right(reader.result.toString))
            // trigger the reader
            reader.readAsDataURL(file)
          }
        }
      )(UpdateImage(_))
    }
  }
}
