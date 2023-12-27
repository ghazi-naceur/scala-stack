package rop.jobsboard.core

import cats.effect.IO
import rop.jobsboard.App
import rop.jobsboard.core.Router.*
import tyrian.Cmd
import fs2.dom.History

// location is the sub-url after the base path:
// jobsboard.com/'login'
// jobsboard.com/'user/info'
// jobsboard.com/'contact'
case class Router private (location: String, history: History[IO, String]) {
  def update(msg: Msg): (Router, Cmd[IO, Msg]) = msg match {
    case ChangeLocation(newLocation, browserTriggered) =>
      if (location == newLocation) (this, Cmd.None)
      else {
        val historyCommand =
          if (browserTriggered) Cmd.None // this is a browser action, so no need to push location in history
          else goto(newLocation)         // this is a manual action, so we need to push location in history

        (this.copy(location = newLocation), historyCommand)
      }
    case _ => (this, Cmd.None) // todo to check external redirects
  }

  def goto[M](location: String): Cmd[IO, M] =
    Cmd.SideEffect[IO] {
      history.pushState(location, location)
    }
}

object Router {

  trait Msg extends App.Msg

  case class ChangeLocation(location: String, browserTriggered: Boolean = false) extends Msg
  case class ExternalRedirect(location: String)                                  extends Msg

  def startAt[M](initialLocation: String): (Router, Cmd[IO, M]) =
    val router = Router(initialLocation, History[IO, String])
    (router, router.goto(initialLocation))
}

/*
  To change the url in the browser manually, we can use the JS api 'history.pushState("new page", "Login", "/login");'.
  We can simulate this behavior in our code using 'fs2.dom.History', which is a wrapper over the native JS api 'history'.
 */
