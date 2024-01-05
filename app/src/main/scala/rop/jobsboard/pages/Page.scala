package rop.jobsboard.pages

import tyrian.*
import cats.effect.*
import rop.jobsboard.App
import rop.jobsboard.pages.Page.Msg

object Page {
  trait Msg

  enum StatusKind {
    case SUCCESS, ERROR, LOADING
  }
  final case class Status(message: String, kind: StatusKind)

  object Urls {
    val LOGIN           = "/login"
    val SIGNUP          = "/signup"
    val FORGOT_PASSWORD = "/forgotpassword"
    val RESET_PASSWORD  = "/resetpassword"
    val PROFILE         = "/profile"
    val JOBS            = "/jobs"
    val EMPTY           = ""
    val HOME            = "/"
    val HASH            = "#"
  }

  import Urls.*
  def get(location: String): Page = location match {
    case `LOGIN`                   => LoginPage()
    case `SIGNUP`                  => SignUpPage()
    case `FORGOT_PASSWORD`         => ForgotPasswordPage()
    case `RESET_PASSWORD`          => ResetPasswordPage()
    case `PROFILE`                 => ProfilePage()
    case `EMPTY` | `HOME` | `JOBS` => JobListPage()
    case s"/jobs/$id"              => JobPage(id)
    // don't use 's"/${`JOBS`}/$id"' because it's not going to work, as string interpolation here won't work
    case _ => NotFoundPage()
  }
}

abstract class Page {

  // send a command upon instantiating
  def initCmd: Cmd[IO, App.Msg]

  // update content of the page
  def update(msg: App.Msg): (Page, Cmd[IO, App.Msg])

  // render the page
  def view(): Html[App.Msg]
}

// login page
// signup page
// forgot password page
// recover password page
// job list page == home page
// individual job page
// not found page
