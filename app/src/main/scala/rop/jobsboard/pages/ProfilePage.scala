package rop.jobsboard.pages

import cats.effect.IO
import tyrian.*
import tyrian.Html.*
import tyrian.http.*
import io.circe.generic.auto.*
import rop.jobsboard.App
import rop.jobsboard.common.{Constants, Endpoint}
import rop.jobsboard.core.Session
import rop.jobsboard.domain.auth.NewPasswordInfo
import tyrian.http.Method.Put

final case class ProfilePage(oldPassword: String = "", newPassword: String = "", status: Option[Page.Status] = None)
    extends FormPage("Profile", status) {

  import ProfilePage.*

  override def update(msg: App.Msg): (Page, Cmd[IO, App.Msg]) = msg match {
    case UpdateOldPassword(p)       => (this.copy(oldPassword = p), Cmd.None)
    case UpdateNewPassword(p)       => (this.copy(newPassword = p), Cmd.None)
    case AttemptChangePassword      => (this, Commands.changePassword(oldPassword, newPassword))
    case ChangePasswordError(error) => (setErrorStatus(error), Cmd.None)
    case ChangePasswordSuccess      => (setSuccessStatus("Success!"), Cmd.None)
    case _                          => (this, Cmd.None)
  }

  override protected def renderFormContent(): List[Html[App.Msg]] = List(
    renderInput("Old password", "oldPassword", "password", isRequired = true, UpdateOldPassword(_)),
    renderInput("New password", "newPassword", "password", isRequired = true, UpdateNewPassword(_)),
    button(`type` := "button", onClick(AttemptChangePassword))("Change password")
  )

  // This parent method needs to be overridden to prevent not logged users from accessing this page, just by typing the
  // url '/profile'
  override def view(): Html[App.Msg] =
    if (Session.isActive) super.view()
    else renderInvalidPage

  private def renderInvalidPage: Html[App.Msg] =
    div(
      h1("Profile "),
      div("It seems that you're not logged in yet.")
    )

  private def setErrorStatus(message: String): Page =
    this.copy(status = Some(Page.Status(message, Page.StatusKind.ERROR)))

  private def setSuccessStatus(message: String): Page =
    this.copy(status = Some(Page.Status(message, Page.StatusKind.SUCCESS)))
}

object ProfilePage {

  trait Msg extends App.Msg

  case class UpdateOldPassword(oldPassword: String) extends Msg
  case class UpdateNewPassword(newPassword: String) extends Msg
  case object AttemptChangePassword                 extends Msg
  case class ChangePasswordError(error: String)     extends Msg
  case object ChangePasswordSuccess                 extends Msg

  object Endpoints {
    val changePassword = new Endpoint[Msg] {

      override val location: String = Constants.Endpoints.changePassword
      override val method: Method   = Put
      override val onResponse: Response => Msg = _.status match {
        case Status(200, _)                      => ChangePasswordSuccess
        case Status(404, _)                      => ChangePasswordError("User doesn't exist.")
        case Status(s, _) if s >= 400 && s < 500 => ChangePasswordError("Invalid credentials.")
        case _                                   => ChangePasswordError("Unknown reply from server.")
      }
      override val onError: HttpError => Msg = e => ChangePasswordError(e.toString)
    }
  }

  object Commands {
    def changePassword(oldPassword: String, newPassword: String) =
      Endpoints.changePassword.callAuthorized(NewPasswordInfo(oldPassword, newPassword))
  }
}
