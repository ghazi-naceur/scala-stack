package rop.jobsboard.components

import rop.jobsboard.core.Router
import rop.jobsboard.pages.Page.Urls.*
import tyrian.Html.*

import scala.scalajs.js
import scala.scalajs.js.annotation.*

object Header {

  def view() = {
    div(`class` := "header-container")(
      renderLogo(),
      div(`class` := "header-nav")(
        ul(`class` := "header-links")(
          renderNavLink("Jobs", JOBS),
          renderNavLink("Login", LOGIN),
          renderNavLink("Sign up", SIGNUP)
        )
      )
    )
  }

  // For logos, we need an image tag, which needs to source, and the source needs to be something that JS can refer to.
  // The problem here if we store Scala strings, they will not be exposed to JS in any way, so we somehow add a path to an
  // image file that we can store in static assets, that path will not be exposed to JS, until we do a trickery, which using
  // JS Native.

  // 'logoImage': this val will be exposed to JS as a native JS string.
  @js.native
  @JSImport("/static/img/konoha.png", JSImport.Default) // adding a path to be referred to in JS
  private val logoImage: String = js.native

  /*
    When encountering the following error for 'js.native': " js.native may only be used as stub implementation in facade types"
    It means that 'logoImage' string does not compile unless it's used in a DOM component (html tag), and that's we're going to do in
    'renderLogo'.
   */
  private def renderLogo() = {
    li(`class` := "nav-item")(
      a(
        href := "/",
        onEvent(
          "click",
          e => {
            e.preventDefault()
            Router.ChangeLocation("/")
          }
        )
      )(
        img(
          `class` := "home-logo",
          src     := logoImage,
          alt     := "Konoha Corp"
        )
      )
    )
  }

  private def renderNavLink(text: String, location: String) = {
    li(`class` := "nav-item")(
      a(
        href    := location,
        `class` := "nav-link",
        onEvent(
          "click",
          e => {
            e.preventDefault() // native JS to prevent reloading the page
            Router.ChangeLocation(location)
          }
        )
      )(text)
    )
  }
}
