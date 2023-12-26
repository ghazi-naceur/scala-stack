package rop.jobsboard.pages

import cats.effect.IO
import rop.jobsboard.common.{Constants, Endpoint}
import rop.jobsboard.domain.auth.*
import rop.jobsboard.pages.Page.Status
import rop.jobsboard.pages.Page.StatusKind.{ERROR, SUCCESS}
import tyrian.Html.*
import tyrian.cmds.Logger
import tyrian.http.*
import tyrian.http.Method.Post
import tyrian.http.{Body, Http, Request}
import tyrian.{Cmd, Html, http}
import io.circe.syntax.*
import io.circe.parser.*
import io.circe.generic.auto.*

/*
  NewUserInfo form:
    input:
      - email
      - password
      - confirm password
      - first name
      - last name
      - company
    button - trigger the signup
    The signup needs to contain its own state by knowing the values stored in the input as the user types them
 */
final case class SignUpPage(
    email: String = "",
    password: String = "",
    confirmPassword: String = "",
    firstName: String = "",
    lastName: String = "",
    company: String = "",
    status: Option[Status] = None
) extends Page {

  import SignUpPage.*
  override def initCmd: Cmd[IO, Page.Msg] = Cmd.None

  override def update(msg: Page.Msg): (Page, Cmd[IO, Page.Msg]) = msg match {
    // We need to send messages as the user is filling data in the input
//    case UpdateEmail(email) => (this, Logger.consoleLog[IO]("Changing email to: " + email))
    case UpdateEmail(e)            => (this.copy(email = e), Cmd.None)
    case UpdatePassword(p)         => (this.copy(password = p), Cmd.None)
    case UpdateConfirmPassword(cp) => (this.copy(confirmPassword = cp), Cmd.None)
    case UpdateFirstName(f)        => (this.copy(firstName = f), Cmd.None)
    case UpdateLastName(l)         => (this.copy(lastName = l), Cmd.None)
    case UpdateCompany(c)          => (this.copy(company = c), Cmd.None)
    case AttemptSignUp =>
      if (!email.matches(Constants.emailRegex))
        (setErrorStatus("Email is invalid"), Cmd.None)
      else if (password.isEmpty)
        (setErrorStatus("Enter a password"), Cmd.None)
      else if (password != confirmPassword)
        (setErrorStatus("Password fields do not match"), Cmd.None)
      else
        (
          this,
          Commands.signup(
            NewUserInfo(
              email,
              password,
              Option(firstName).filter(_.nonEmpty),
              Option(lastName).filter(_.nonEmpty),
              Option(company).filter(_.nonEmpty)
            )
          )
        )
    case SignUpError(message)   => (setErrorStatus(message), Cmd.None)
    case SignUpSuccess(message) => (setSuccessStatus(message), Cmd.None)
    case _                      => (this, Cmd.None)
  }

  override def view(): Html[Page.Msg] =
    div(`class` := "form-section")(
      div(`class` := "top-section")(
        h1("Sign up")
      ),
      // 'preventDefault()' is used to prevent refreshing the page after submitting a form, to avoid loosing state
      form(
        name    := "signin",
        `class` := "form",
        onEvent(
          "submit",
          e => {
            e.preventDefault()
            NoOp // won't change the state of the page.
            // So everytime the form is submitted, we're going to send 'NoOp' which is not going to change the state of the page
          }
        )
      )(
        // 6 inputs
        // 'UpdateEmail(_)' this is the function to change the value of the email in this state
        renderInput("Email", "email", "text", isRequired = true, UpdateEmail(_)),
        renderInput("Password", "password", "password", isRequired = true, UpdatePassword(_)),
        renderInput("Confirm password", "cPassword", "password", isRequired = true, UpdateConfirmPassword(_)),
        renderInput("First name", "firstName", "text", isRequired = false, UpdateFirstName(_)),
        renderInput("Last name", "lastName", "text", isRequired = false, UpdateLastName(_)),
        renderInput("Company", "company", "text", isRequired = false, UpdateCompany(_)),
        // button
        button(`type` := "button", onClick(AttemptSignUp))("Sign up")
      ),
      status.map(s => div(s.message)).getOrElse(div())
    )

  private def renderInput(name: String, uid: String, kind: String, isRequired: Boolean, onChange: String => Msg) =
    div(`class` := "form-input")(
      label(`for` := name, `class` := "form-label")(
        if (isRequired) span("*") else span(),
        text(name)
      ),
      input(`type` := kind, `class` := "form-control", id := uid, onInput(onChange))
    )

  private def setErrorStatus(message: String): Page =
    this.copy(status = Some(Status(message, ERROR)))

  private def setSuccessStatus(message: String): Page =
    this.copy(status = Some(Status(message, SUCCESS)))

}

object SignUpPage {
  trait Msg extends Page.Msg

  case class UpdateEmail(email: String)                     extends Msg
  case class UpdatePassword(password: String)               extends Msg
  case class UpdateConfirmPassword(confirmPassword: String) extends Msg
  case class UpdateFirstName(firstName: String)             extends Msg
  case class UpdateLastName(lastName: String)               extends Msg
  case class UpdateCompany(company: String)                 extends Msg

  // Actions
  case object AttemptSignUp extends Msg
  case object NoOp          extends Msg

  // Statuses
  case class SignUpError(message: String)   extends Msg
  case class SignUpSuccess(message: String) extends Msg

  object Endpoints {
    val signup: Endpoint[Msg] = new Endpoint[Msg] {
      override val location: String = Constants.Endpoints.signup
      override val method: Method   = Post
      override val onSuccess: Response => Msg =
        response =>
          response.status match {
            case tyrian.http.Status(201, _) => SignUpSuccess("User has signed up in successfully.")
            case tyrian.http.Status(s, _) if s >= 400 && s < 500 =>
              val json   = response.body
              val parsed = parse(json).flatMap(_.hcursor.get[String]("error"))
              // 'error' is the field name from 'FailureResponse.scala'
              parsed match
                case Right(thr) => SignUpError(thr)
                case Left(thr)  => SignUpError(s"Error: ${thr.getMessage}")
          }
      override val onError: HttpError => Msg = e => SignUpError(e.toString)
    }
  }

  object Commands {
    def signup(newUserInfo: NewUserInfo): Cmd[IO, Msg] =
      Endpoints.signup.call(newUserInfo)

  }
}
