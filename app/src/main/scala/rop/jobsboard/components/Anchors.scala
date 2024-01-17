package rop.jobsboard.components

import rop.jobsboard.App
import rop.jobsboard.core.Router
import tyrian.*
import tyrian.Html.*
object Anchors {

  def renderSimpleNavLink(text: String, location: String) =
    renderNavLink(text, location)(Router.ChangeLocation(_))

  def renderNavLink(text: String, location: String)(location2msg: String => App.Msg) = {
    li(`class` := "nav-item")(
      a(
        href    := location,
        `class` := "nav-link",
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
