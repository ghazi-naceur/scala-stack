package rop.jobsboard.common

import scala.util.matching.Regex

object Constants {

  val emailRegex: String =
    """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"""

  val ADMIN_EMAIL = "admin@gmail.com"

  val defaultPageSize = 20

  object Endpoints {
    val root           = "http://localhost:4041"
    val signup         = s"$root/api/auth/users"
    val login          = s"$root/api/auth/login"
    val logout         = s"$root/api/auth/logout"
    val checkToken     = s"$root/api/auth/checkToken"
    val forgotPassword = s"$root/api/auth/reset"
    val resetPassword  = s"$root/api/auth/recover"
    val changePassword = s"$root/api/auth/users/password"
    val postJob        = s"$root/api/jobs/create"
    val jobs           = s"$root/api/jobs"
  }

  object Cookies {
    val duration: Int = 10 * 24 * 3600 * 1000
    val email: String = "email"
    val token: String = "token"
  }
}
