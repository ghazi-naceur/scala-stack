package rop.jobsboard.pages

import cats.effect.IO
import rop.jobsboard.App
import rop.jobsboard.common.{Constants, Endpoint}
import tyrian.Html.*
import tyrian.http.Method.Post
import tyrian.http.{HttpError, Method, Response, Status}
import tyrian.{Cmd, Html}
import io.circe.parser.*
import io.circe.generic.auto.*
import rop.jobsboard.components.Anchors
import rop.jobsboard.domain.auth.RecoverPasswordInfo
import rop.jobsboard.pages.Page.StatusKind

final case class ResetPasswordPage(
    email: String = "",
    token: String = "",
    password: String = "",
    status: Option[Page.Status] = None
) extends FormPage("Reset password", status) {

  import ResetPasswordPage.*

  override def update(msg: App.Msg): (Page, Cmd[IO, App.Msg]) = msg match {
    case UpdateEmail(e)    => (this.copy(email = e), Cmd.None)
    case UpdateToken(t)    => (this.copy(token = t), Cmd.None)
    case UpdatePassword(p) => (this.copy(password = p), Cmd.None)
    case AttemptResetPassword =>
      if (!email.matches(Constants.emailRegex))
        (setErrorStatus("Insert a valid email."), Cmd.None)
      else if (token.isEmpty)
        (setErrorStatus("Add a token."), Cmd.None)
      else if (password.isEmpty)
        (setErrorStatus("Add a password."), Cmd.None)
      else (this, Commands.resetPassword(email, token, password))
    case ResetPasswordFailure(error) => (setErrorStatus(error), Cmd.None)
    case ResetPasswordSuccess        => (setSuccessStatus("Success! You can log in now."), Cmd.None)
    case _                           => (this, Cmd.None)
  }

  override def renderFormContent(): List[Html[App.Msg]] = List(
    renderInput("Email", "email", "text", isRequired = true, UpdateEmail(_)),
    renderInput("Token", "token", "text", isRequired = true, UpdateToken(_)),
    renderInput("Password", "password", "password", isRequired = true, UpdatePassword(_)),
    button(`type` := "button", onClick(AttemptResetPassword))("Set password"),
    Anchors.renderSimpleNavLink("Don't have a token yet?", Page.Urls.FORGOT_PASSWORD, "auth-link")
  )

  private def setErrorStatus(message: String): Page =
    this.copy(status = Some(Page.Status(message, StatusKind.ERROR)))

  private def setSuccessStatus(message: String): Page =
    this.copy(status = Some(Page.Status(message, StatusKind.SUCCESS)))
}

object ResetPasswordPage {

  trait Msg                                      extends App.Msg
  case class UpdateEmail(email: String)          extends Msg
  case class UpdateToken(token: String)          extends Msg
  case class UpdatePassword(password: String)    extends Msg
  case object AttemptResetPassword               extends Msg
  case class ResetPasswordFailure(error: String) extends Msg
  case object ResetPasswordSuccess               extends Msg

  object Endpoints {
    val resetPassword: Endpoint[Msg] = new Endpoint[Msg] {
      override val location: String = Constants.Endpoints.resetPassword
      override val method: Method   = Post
      override val onResponse: Response => Msg = response =>
        response.status match {
          case Status(200, _) => ResetPasswordSuccess
          case Status(s, _) if s >= 400 && s < 500 =>
            val json   = response.body
            val parsed = parse(json).flatMap(_.hcursor.get[String]("error"))
            parsed match {
              case Right(errorFromServer) => ResetPasswordFailure(errorFromServer)
              case Left(error)            => ResetPasswordFailure(s"Response error: ${error.getMessage}")
            }
          case _ => ResetPasswordFailure("Unknown reply from server.")
        }
      override val onError: HttpError => Msg = e => ResetPasswordFailure(e.toString)
    }
  }

  object Commands {

    def resetPassword(email: String, token: String, password: String): Cmd[IO, Msg] =
      Endpoints.resetPassword.call(RecoverPasswordInfo(Constants.ADMIN_EMAIL, email, token, password))
  }
}
