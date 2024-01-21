package rop.jobsboard.components

import rop.jobsboard.App
import rop.jobsboard.core.Router
import tyrian.*
import tyrian.Html.*
object Anchors {

  def renderSimpleNavLink(text: String, location: String, cssClass: String = "") =
    renderNavLink(text, location, cssClass)(Router.ChangeLocation(_))

  def renderNavLink(text: String, location: String, cssClass: String = "")(location2msg: String => App.Msg) = {
    div(`class` := "nav-item")(
      a(
        href    := location,
        `class` := cssClass,
        onEvent(
          "click",
          e => {
            e.preventDefault() // native JS to prevent reloading the page
            location2msg(location)
          }
        )
      )(text)
    )
  }
}
