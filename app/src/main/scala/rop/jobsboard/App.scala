package rop.jobsboard

import cats.effect.*
import org.scalajs.dom.{console, document, window}
import rop.jobsboard.App.{Decrement, Increment, Model, Msg}
import rop.jobsboard.components.Header
import rop.jobsboard.core.Router
import rop.jobsboard.core.Router.{ChangeLocation, ExternalRedirect}
import tyrian.*
import tyrian.Html.*
import tyrian.cmds.Logger

import scala.concurrent.duration.*
import scala.language.postfixOps
import scala.scalajs.js.annotation.*

object App {
  type Msg = Router.Msg
  case class Increment(amount: Int) extends Msg
  case class Decrement(amount: Int) extends Msg
  case class Model(router: Router)

}

@JSExportTopLevel("JobsBoardFE")
class App extends TyrianApp[Msg, Model] {

  override def init(flags: Map[String, String]): (Model, Cmd[IO, Msg]) = {
    // 'window' is used to keep track of the current location the app is at
    val (router, cmd) = Router.startAt(window.location.pathname)
    (Model(router), cmd)
  }

  override def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = { case msg: Msg =>
    val (newRouter, command) = model.router.update(msg)
    (model.copy(router = newRouter), command)
  }

  override def view(model: Model): Html[Msg] = {
    div(
      Header.view(),
      div(s"You are now at: ${model.router.location}")
    )
  }

  override def subscriptions(model: Model): Sub[IO, Msg] = {
//    'state' is an fs2 signal that can emit new elements whenever the signal changes
//    'discrete' is an fs2 stream
//    '_.get' getting an optional location from 'history'. This optional always has a value, so we can call '.get' method
    Sub.make(
      "urlChange",
      model.router.history.state.discrete // stream of locations
        .map(_.get)
        .map(newLocation => Router.ChangeLocation(newLocation, browserTriggered = true)) // a listener for browser history changes

      // it's useful to change the url, when clicking the 'go back' browser button for example.
      // Whenever we click the 'go back' button, the 'history' changes, which triggers 'Router.ChangeLocation(newLocation)'
      // which triggers the 'update' method, specifically 'model.router.update(msg)', which triggers the 'goto' method
      // that executes 'history.pushState(location, location)' for the location I've just pushed the 'back' button for.
      // So essentially what the browser is doing is pushing that location that I'm navigating to 1 more time to history,
      // so I have to hit the back button twice to get out of there. So I need to differentiate the 'ChangeLocation' message
      // between the situation when I'm hitting a link and the situation where I'm hitting the back or forward browser buttons,
      // so we can add a flag for that: 'browserTriggered'
    )
  }

}
