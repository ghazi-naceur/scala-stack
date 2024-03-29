package rop.jobsboard.common

import org.scalajs.dom.window

import scala.scalajs.{LinkingInfo, js}
import scala.scalajs.js.annotation.*
import scala.util.matching.Regex

object Constants {

  @js.native
  @JSImport("/static/img/konoha.png", JSImport.Default) // adding a path to be referred to in JS
  val logoImage: String = js.native

  @js.native
  @JSImport("/static/img/briefcase.png", JSImport.Default) // adding a path to be referred to in JS
  val jobImagePlaceholder: String = js.native

  val emailRegex: String =
    """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"""

  val ADMIN_EMAIL = "admin@gmail.com"

  val defaultPageSize   = 20
  val jobAdvertPriceEUR = 99

  object Endpoints {
    val root =
      if (LinkingInfo.developmentMode) "http://localhost:4041"
      else window.location.origin // to get the fully qualified domain of the host (or ip)
    val signup          = s"$root/api/auth/users"
    val login           = s"$root/api/auth/login"
    val logout          = s"$root/api/auth/logout"
    val checkToken      = s"$root/api/auth/checkToken"
    val forgotPassword  = s"$root/api/auth/reset"
    val resetPassword   = s"$root/api/auth/recover"
    val changePassword  = s"$root/api/auth/users/password"
    val postJob         = s"$root/api/jobs/create"
    val postJobPromoted = s"$root/api/jobs/promoted"
    val jobs            = s"$root/api/jobs"
    val filters         = s"$root/api/jobs/filters"
  }

  object Cookies {
    val duration: Int = 10 * 24 * 3600 * 1000
    val email: String = "email"
    val token: String = "token"
  }
}
