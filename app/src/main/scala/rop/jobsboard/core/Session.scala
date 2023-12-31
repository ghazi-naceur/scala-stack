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
import tyrian.http.Method.{Get, Post}
import tyrian.http.{HttpError, Method, Response, Status}

import scala.scalajs.js.Date

final case class Session(email: Option[String] = None, token: Option[String] = None) {

  import Session.*
  def update(msg: Msg): (Session, Cmd[IO, App.Msg]) = msg match {
    case SetToken(e, t, isNewUser) =>
      val cookieCmd = Commands.setAllSessionCookies(e, t, isFresh = isNewUser)
      val routingCmd =
        if (isNewUser) Cmd.Emit(Router.ChangeLocation(Page.Urls.HOME)) // new authenticated user == token is still fresh
        else Commands.checkToken                                       // check whether token is still valid on the server
      (
        this.copy(email = Some(e), token = Some(t)),
        cookieCmd |+| routingCmd
      )

    case CheckToken      => (this, Commands.checkToken)
    case KeepToken       => (this, Cmd.None)

    case Logout =>
      // trigger an authorized http to the backend
      val cmd = token.map(_ => Commands.logout).getOrElse(Cmd.None)
      (this, cmd)

    case LogoutSuccess | InvalidateToken =>
      (
        this.copy(email = None, token = None),
        Commands.clearAllSessionCookies() |+| Cmd.Emit(Router.ChangeLocation(Page.Urls.HOME))
      )

    // todo add case LogoutFailure
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

  // Checking the token
  case object CheckToken      extends Msg
  case object KeepToken       extends Msg
  case object InvalidateToken extends Msg

  // Logout actions
  case object Logout        extends Msg
  case object LogoutSuccess extends Msg
  case object LogoutFailure extends Msg

  def isActive: Boolean = getUserToken.nonEmpty

  def getUserToken: Option[String] = getCookie(Cookies.token)

  object Endpoints {
    val logout: Endpoint[Msg] = new Endpoint[Msg] {
      override val location: String           = Constants.Endpoints.logout
      override val method: Method             = Post
      override val onSuccess: Response => Msg = _ => LogoutSuccess
      override val onError: HttpError => Msg  = _ => LogoutFailure
    }

    val checkToken: Endpoint[Msg] = new Endpoint[Msg] {
      override val location: String = Constants.Endpoints.checkToken
      override val method: Method   = Get
      override val onSuccess: Response => Msg = response =>
        response.status match {
          case Status(200, _) => KeepToken
          case _              => InvalidateToken
        }
      override val onError: HttpError => Msg = _ => InvalidateToken
    }
  }

  object Commands {

    def logout: Cmd[IO, Msg] =
      Endpoints.logout.callAuthorized()

    def checkToken: Cmd[IO, Msg] =
      Endpoints.checkToken.callAuthorized() // 'callAuthorized' will embed automatically whatever we have in the cookie
      // as an authorization token

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
