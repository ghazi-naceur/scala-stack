package rop.jobsboard.domain

// user-facing
object auth {

  final case class LoginInfo(email: String, password: String)

  case class NewUserInfo(
      email: String,
      password: String,
      firstName: Option[String],
      lastName: Option[String],
      company: Option[String]
  )

  final case class NewPasswordInfo(oldPassword: String, newPassword: String)

  final case class ForgotPasswordInfo(from: String, to: String)

  final case class RecoverPasswordInfo(from: String, to: String, token: String, newPassword: String)
}
