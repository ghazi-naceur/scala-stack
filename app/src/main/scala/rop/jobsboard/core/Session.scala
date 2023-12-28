package rop.jobsboard.core

import tyrian.*
import cats.effect.IO
import tyrian.cmds.Logger
import rop.jobsboard.App
import org.scalajs.dom.document
import rop.jobsboard.common.Constants.Cookies
import rop.jobsboard.common.Constants.Cookies.*

import scala.scalajs.js.Date

final case class Session(email: Option[String] = None, token: Option[String] = None) {

  import Session.*
  def update(msg: Msg): (Session, Cmd[IO, Msg]) = msg match {
    case SetToken(e, t, isNewUser) =>
      (
        this.copy(email = Some(e), token = Some(t)),
        Commands.setAllSessionCookies(e, t, isFresh = isNewUser)
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

  object Commands {
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
