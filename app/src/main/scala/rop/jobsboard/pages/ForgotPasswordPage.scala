package rop.jobsboard.pages

import cats.effect.IO
import rop.jobsboard.App
import rop.jobsboard.common.{Constants, Endpoint}
import rop.jobsboard.domain.auth.ForgotPasswordInfo
import rop.jobsboard.pages.Page.{Status, StatusKind}
import io.circe.generic.auto.*
import tyrian.Html.*
import tyrian.http.Method.Post
import tyrian.http.{HttpError, Method, Response}
import tyrian.{Cmd, Html}

final case class ForgotPasswordPage(email: String = "", status: Option[Page.Status] = None)
    extends FormPage("Reset password", status) {

  import ForgotPasswordPage.*
  override def update(msg: App.Msg): (Page, Cmd[IO, App.Msg]) = msg match {
    case UpdateEmail(e) => (this.copy(email = e), Cmd.None)
    case AttemptResetPassword =>
      if (!email.matches(Constants.emailRegex))
        (setErrorStatus("Insert a valid email."), Cmd.None)
      else (this, Commands.resetPassword(email))
    case ResetSuccess        => (setSuccessStatus("Check your email!"), Cmd.None)
    case ResetFailure(error) => (setErrorStatus(error), Cmd.None)
    case _                   => (this, Cmd.None)
  }

  override protected def renderFormContent(): List[Html[App.Msg]] = List(
    renderInput("Email", "email", "text", isRequired = true, UpdateEmail(_)),
    button(`type` := "button", onClick(AttemptResetPassword))("Send email")
  )

  private def setErrorStatus(message: String): Page =
    this.copy(status = Some(Status(message, StatusKind.ERROR)))

  private def setSuccessStatus(message: String): Page =
    this.copy(status = Some(Status(message, StatusKind.SUCCESS)))

}

object ForgotPasswordPage {
  trait Msg extends App.Msg

  case class UpdateEmail(email: String)  extends Msg
  case object AttemptResetPassword       extends Msg
  case class ResetFailure(error: String) extends Msg
  case object ResetSuccess               extends Msg

  object Endpoints {
    val resetPassword: Endpoint[Msg] = new Endpoint[Msg] {
      override val location: String            = Constants.Endpoints.forgotPassword
      override val method: Method              = Post
      override val onResponse: Response => Msg = _ => ResetSuccess
      override val onError: HttpError => Msg   = e => ResetFailure(e.toString)
    }
  }

  object Commands {
    def resetPassword(email: String) =
      Endpoints.resetPassword.call(ForgotPasswordInfo("admin@gmail.com", email))
  }
}
