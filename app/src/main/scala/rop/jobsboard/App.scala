package rop.jobsboard

import cats.effect.*
import org.scalajs.dom.{console, document, window}
import rop.jobsboard.core.Session
import rop.jobsboard.components.Header
import rop.jobsboard.core.Router
import rop.jobsboard.core.Router.{ChangeLocation, ExternalRedirect}
import rop.jobsboard.pages.Page
import tyrian.*
import tyrian.Html.*
import tyrian.cmds.Logger

import scala.concurrent.duration.*
import scala.language.postfixOps
import scala.scalajs.js.annotation.*

object App {
  trait Msg
  case class Model(router: Router, session: Session, page: Page)
}

@JSExportTopLevel("JobsBoardFE")
class App extends TyrianApp[App.Msg, App.Model] {

  import App.*

  override def init(flags: Map[String, String]): (Model, Cmd[IO, Msg]) = {
    // 'window' is used to keep track of the current location the app is at
    val location            = window.location.pathname
    val page                = Page.get(location)
    val pageCmd             = page.initCmd
    val (router, routerCmd) = Router.startAt(location)
    val session             = Session()
    val sessionCmd          = session.initCmd
    (Model(router, session, page), routerCmd |+| sessionCmd |+| pageCmd)
  }

  override def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case msg: Router.Msg =>
      val (newRouter, routerCmd) = model.router.update(msg)
      if (model.router == newRouter) (model, Cmd.None) // no page change is necessary
      else { // router has changes == the location has changed => a need to re-render the appropriate page
        val newPage    = Page.get(newRouter.location)
        val newPageCmd = newPage.initCmd
        (model.copy(router = newRouter, page = newPage), routerCmd |+| newPageCmd)
      }

    case msg: Session.Msg =>
      val (newSession, cmd) = model.session.update(msg)
      (model.copy(session = newSession), cmd)

    case msg: App.Msg =>
      val (newPage, command) = model.page.update(msg)
      (model.copy(page = newPage), command)
  }

  override def view(model: Model): Html[Msg] = {
    div(
      Header.view(),
      model.page.view()
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
