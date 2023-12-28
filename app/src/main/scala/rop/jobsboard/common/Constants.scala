package rop.jobsboard.common

import scala.util.matching.Regex

object Constants {

  val emailRegex: String =
    """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"""

  object Endpoints {
    val root   = "http://localhost:4041"
    val signup = s"$root/api/auth/users"
    val login  = s"$root/api/auth/login"
  }

  object Cookies {
    val duration: Int = 10 * 24 * 3600 * 1000
    val email: String = "email"
    val token: String = "token"
  }
}
