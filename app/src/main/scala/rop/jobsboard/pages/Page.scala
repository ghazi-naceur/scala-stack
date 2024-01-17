package rop.jobsboard.pages

import tyrian.*
import cats.effect.*
import rop.jobsboard.App
import rop.jobsboard.components.Component
import rop.jobsboard.pages.Page.Msg

object Page {
  trait Msg

  enum StatusKind {
    case SUCCESS, ERROR, LOADING
  }
  final case class Status(message: String, kind: StatusKind)

  object Status {
    val LOADING: Status = Status("Loading", StatusKind.LOADING)
  }

  object Urls {
    val LOGIN           = "/login"
    val SIGNUP          = "/signup"
    val FORGOT_PASSWORD = "/forgotpassword"
    val RESET_PASSWORD  = "/resetpassword"
    val PROFILE         = "/profile"
    val POST_JOB        = "/postjob"
    val JOBS            = "/jobs"
    val EMPTY           = ""
    val HOME            = "/"
    val HASH            = "#"
    def JOB(id: String) = JOBS + s"/$id"
  }

  import Urls.*
  def get(location: String): Page = location match {
    case `LOGIN`                   => LoginPage()
    case `SIGNUP`                  => SignUpPage()
    case `FORGOT_PASSWORD`         => ForgotPasswordPage()
    case `RESET_PASSWORD`          => ResetPasswordPage()
    case `PROFILE`                 => ProfilePage()
    case `POST_JOB`                => PostJobPage()
    case `EMPTY` | `HOME` | `JOBS` => JobListPage()
    case s"/jobs/$id"              => JobPage(id)
    // don't use 's"/${`JOBS`}/$id"' because it's not going to work, as string interpolation here won't work
    case _ => NotFoundPage()
  }
}

abstract class Page extends Component[App.Msg, Page]
