package rop.jobsboard.pages

import cats.effect.IO
import rop.jobsboard.pages.Page.{Status, StatusKind}
import tyrian.Html.*
import tyrian.cmds.Logger
import tyrian.http.*
import tyrian.http.Method.Post
import tyrian.http.{Body, Http, Request}
import tyrian.{Cmd, Html, http}
import io.circe.syntax.*
import io.circe.parser.*
import io.circe.generic.auto.*
import rop.jobsboard.App
import rop.jobsboard.common.*
import rop.jobsboard.core.Session
import rop.jobsboard.domain.auth.LoginInfo

/*
  Login form:
    - email
    - password
    - button

  Status (success or failure)
 */

final case class LoginPage(email: String = "", password: String = "", status: Option[Page.Status] = None) extends Page {

  import LoginPage.*
  override def initCmd: Cmd[IO, App.Msg] = Cmd.None

  override def update(msg: App.Msg): (Page, Cmd[IO, App.Msg]) = msg match {
    case UpdateEmail(e)    => (this.copy(email = e), Cmd.None)
    case UpdatePassword(p) => (this.copy(password = p), Cmd.None)
    case AttemptLogin =>
      if (!email.matches(Constants.emailRegex))
        (setErrorStatus("Invalid email"), Cmd.None)
      else if (password.isEmpty)
        (setErrorStatus("Enter a password"), Cmd.None)
      else (this, Commands.login(LoginInfo(email, password)))
    case LoginError(error) => (setErrorStatus(error), Cmd.None)
    case LoginSuccess(token) =>
      (setSuccessStatus("You have logged in successfully"), Cmd.Emit(Session.SetToken(email, token, isNewUser = true)))
    //                                                               ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ should be a App.Msg
    // The command will propagate the token through the entire app. The message issued by this command will be intercepted
    // by 'update' method of App.scala and it will change the model of the app to store the user session token
    case _ => (this, Cmd.None)
  }

  override def view(): Html[App.Msg] =
    div(`class` := "form-section")(
      div(`class` := "top-section")(
        h1("Log in")
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
        renderInput("Email", "email", "text", isRequired = true, UpdateEmail(_)),
        renderInput("Password", "password", "password", isRequired = true, UpdatePassword(_)),
        button(`type` := "button", onClick(AttemptLogin))("Login")
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
    this.copy(status = Some(Status(message, StatusKind.ERROR)))

  private def setSuccessStatus(message: String): Page =
    this.copy(status = Some(Status(message, StatusKind.SUCCESS)))

}

object LoginPage {
  trait Msg extends App.Msg

  case class UpdateEmail(email: String)       extends Msg
  case class UpdatePassword(password: String) extends Msg

  case object AttemptLogin extends Msg
  case object NoOp         extends Msg

  case class LoginError(error: String)   extends Msg
  case class LoginSuccess(token: String) extends Msg

  object Endpoints {
    val login: Endpoint[Msg] = new Endpoint[Msg] {
      override val location: String = Constants.Endpoints.login
      override val method: Method   = Post
      override val onSuccess: Response => Msg = response => {
        val potentialToken = response.headers.get("authorization")
        potentialToken match {
          case Some(token) => LoginSuccess(token)
          case None        => LoginError("Invalid username or password")
        }
      }
      override val onError: HttpError => Msg = e => LoginError(e.toString)
    }
  }

  object Commands {
    def login(loginInfo: LoginInfo): Cmd[IO, Msg] =
      Endpoints.login.call(loginInfo)
  }
}
