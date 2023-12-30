package rop.jobsboard.core

import tyrian.*
import cats.effect.IO
import tyrian.cmds.Logger
import rop.jobsboard.App
import org.scalajs.dom.document
import rop.jobsboard.common.Constants.Cookies
import rop.jobsboard.common.Constants.Cookies.*
import rop.jobsboard.common.{Constants, Endpoint}
import rop.jobsboard.pages.Page
import tyrian.http.Method.Post
import tyrian.http.{HttpError, Method, Response}

import scala.scalajs.js.Date

final case class Session(email: Option[String] = None, token: Option[String] = None) {

  import Session.*
  def update(msg: Msg): (Session, Cmd[IO, App.Msg]) = msg match {
    case SetToken(e, t, isNewUser) =>
      val cookieCmd = Commands.setAllSessionCookies(e, t, isFresh = isNewUser)
      val routingCmd =
        if (isNewUser) Cmd.Emit(Router.ChangeLocation(Page.Urls.HOME)) // re-routing to home when the user is logged in
        else Cmd.None                                                  // no re-routing to home if the user is not logged in
      (
        this.copy(email = Some(e), token = Some(t)),
        cookieCmd |+| routingCmd
      )

    case Logout =>
      // trigger an authorized http to the backend
      val cmd = token.map(_ => Commands.logout).getOrElse(Cmd.None)
      (this, cmd)

    case LogoutSuccess =>
      (
        this.copy(email = None, token = None),
        Commands.clearAllSessionCookies() |+| Cmd.Emit(Router.ChangeLocation(Page.Urls.HOME))
      )
  }

  def initCmd: Cmd[IO, Msg] = {
    val potentialCommand = for {
      email <- getCookie(Cookies.email)
      token <- getCookie(Cookies.token)
    } yield Cmd.Emit(SetToken(email, token, isNewUser = false)) // at startup, no user is logged in, so 'isNewUser = false'

    potentialCommand.getOrElse(Cmd.None)
  }

}

object Session {
  trait Msg extends App.Msg

  case class SetToken(email: String, token: String, isNewUser: Boolean = false) extends Msg
  case object Logout                                                            extends Msg
  case object LogoutSuccess                                                     extends Msg
  case object LogoutFailure                                                     extends Msg

  def isActive: Boolean = getUserToken.nonEmpty

  def getUserToken: Option[String] = getCookie(Cookies.token)

  object Endpoints {
    val logout: Endpoint[Msg] = new Endpoint[Msg] {
      val location: String           = Constants.Endpoints.logout
      val method: Method             = Post
      val onSuccess: Response => Msg = _ => LogoutSuccess
      val onError: HttpError => Msg  = _ => LogoutFailure
    }
  }

  object Commands {

    def logout: Cmd[IO, Msg] =
      Endpoints.logout.callAuthorized()

    def setSessionCookie(name: String, value: String, isFresh: Boolean): Cmd[IO, Msg] =
      Cmd.SideEffect[IO] {
        if (getCookie(name).isEmpty || isFresh)
          document.cookie = s"$name=$value;expires=${new Date(Date.now() + Cookies.duration)};path=/"
      }

    def setAllSessionCookies(email: String, token: String, isFresh: Boolean = false): Cmd[IO, Msg] = {
      setSessionCookie(Cookies.email, email, isFresh) |+| setSessionCookie(Cookies.token, token, isFresh)
    }

    def clearSessionCookie(name: String): Cmd[IO, Msg] =
      Cmd.SideEffect[IO] {
        document.cookie = s"$name=;expires=${new Date(0)};path=/"
      }

    def clearAllSessionCookies(): Cmd[IO, Msg] = {
      clearSessionCookie(Cookies.email) |+| clearSessionCookie(Cookies.token)
    }
  }

  private def getCookie(name: String): Option[String] =
    document.cookie // key1=value1; key2=value2;key3=value3
      .split(";")
      .map(_.trim)
      .find(_.startsWith(s"$name=")) // option of "key1=value1"
      .map(_.split("="))
      .map(_(1)) // "value1"

}
