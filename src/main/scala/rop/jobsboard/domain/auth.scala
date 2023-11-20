package rop.jobsboard.domain

// user-facing
object auth {

  final case class LoginInfo(email: String, password: String)

  final case class NewPasswordInfo(oldPassword: String, newPassword: String)
}
