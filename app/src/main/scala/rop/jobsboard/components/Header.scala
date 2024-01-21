package rop.jobsboard.components

import rop.jobsboard.App
import rop.jobsboard.core.{Router, Session}
import rop.jobsboard.pages.Page.Urls.*
import tyrian.Html
import tyrian.Html.*

import scala.scalajs.js
import scala.scalajs.js.annotation.*

object Header {

//                    <li class="nav-item">
//                      <a class="nav-link jvm-item Home active-item" href="index.html">Home</a>
//
//                    </li>
//                    <li class="nav-item">
//                      <a class="nav-link jvm-item job" href="jobs.html">Jobs</a>
//                    </li>
//                    <li class="nav-item">
//                      <a class="nav-link jvm-item contect-us" href="Contact-us.html">Contact Us</a>
//                    </li>
//                    <li class="nav-item">
//                      <a class="nav-link jvm-iteme" href="#">|</a>
//                    </li>
//                    <li class="nav-item">
//                      <a class="nav-link jvm-item post-a-job" href="Post-a-Job.html">Post A Job</a>
//                    </li>
//                    <!-- start-button   -->
//
//                    <li class="nav-item">
//                      <a class="nav-link " href="signin.html">
//                        <button class="btn-register" type="text">Register</button>
//                      </a>
//                    </li>
//                    <li class="nav-item">
//                      <a class="nav-link " href="login.html">
//                        <button class="btn-logout" type="text">Login</button>
//                      </a>
//                    </li>
//
//                    <!-- End-button   -->
//
//                  </ul>
//                </div>
//              </div>
//            </nav>
//          </div>
//        </div>
//      </div>

  def view() = {
    div(`class` := "container-fluid p-0")(
      div(`class` := "jvm-nav")(
        div(`class` := "container")(
          nav(`class` := "navbar navbar-expand-lg navbar-light JVM-nav")(
            div(`class` := "container")(
              renderLogo(),
              button(
                `class` := "navbar-toggler",
                `type`  := "button",
                attribute("data-bs-toggle", "collapse"),
                attribute("data-bs-target", "#navbarNav"),
                attribute("aria-controls", "navbarNav"),
                attribute("aria-expanded", "false"),
                attribute("aria-label", "Toggle navigation")
              )(
                span(`class` := "navbar-toggler-icon")()
              ),
              div(`class` := "collapse navbar-collapse", id := "navbarNav")(
                ul(`class` := "navbar-nav ms-auto menu align-center expanded text-center SMN_effect-3")(
                  renderNavLinks()
                )
              )
            )
          )
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
    a(
      href    := "/",
      `class` := "navbar-brand",
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
  }

  private def renderNavLinks(): List[Html[App.Msg]] = {
    val constantLinks = List(
      renderSimpleNavLink("Jobs", JOBS),
      renderSimpleNavLink("Post job", POST_JOB)
    )

    val unauthedLinks = List(
      renderSimpleNavLink("Login", LOGIN),
      renderSimpleNavLink("Sign up", SIGNUP)
    )

    val authedLinks = List(
      renderSimpleNavLink("Profile", PROFILE),
      renderNavLink("Log out", HASH)(_ => Session.Logout)
    )

    constantLinks ++ (
      if (Session.isActive) authedLinks
      else unauthedLinks
    )
  }

  private def renderSimpleNavLink(text: String, location: String) =
    renderNavLink(text, location)(Router.ChangeLocation(_))

  private def renderNavLink(text: String, location: String)(location2msg: String => App.Msg) =
    li(`class` := "nav-item")(
      Anchors.renderNavLink(text, location, "nav-link jvm-item Home active-item")(location2msg)
    )

}
