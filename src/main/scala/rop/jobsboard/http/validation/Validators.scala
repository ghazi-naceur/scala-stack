package rop.jobsboard.http.validation

import cats.*
import cats.implicits.*
import cats.data.*
import cats.data.Validated.*
import rop.jobsboard.domain.Job.JobInfo
import rop.jobsboard.domain.auth.*
import rop.jobsboard.domain.user.*

import java.net.URL
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}
object Validators {

  sealed trait ValidationFailure(val errorMessage: String)
  case class EmptyField(fieldName: String)   extends ValidationFailure(s"'$fieldName' is empty")
  case class InvalidUrl(fieldName: String)   extends ValidationFailure(s"Url '$fieldName' is an invalid url")
  case class InvalidEmail(fieldName: String) extends ValidationFailure(s"Email '$fieldName' is an invalid email")

  type ValidationResult[A] = ValidatedNel[ValidationFailure, A]

  trait Validator[A] {
    def validate(value: A): ValidationResult[A]
  }

  val emailRegex: Regex =
    """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r

  def validateRequired[A](field: A, fieldName: String)(required: A => Boolean): ValidationResult[A] =
    if (required(field)) field.validNel
    else EmptyField(fieldName).invalidNel

  def validateUrl(field: String, fieldName: String): ValidationResult[String] = Try {
    URL(field).toURI
  } match {
    case Success(_) => field.validNel
    case Failure(_) => InvalidUrl(fieldName).invalidNel
  }

  def validateEmail(field: String, fieldName: String): ValidationResult[String] = {
    if (emailRegex.findFirstMatchIn(field).isDefined) field.validNel
    else InvalidEmail(fieldName).invalidNel
  }

  given jobInfoValidator: Validator[JobInfo] = (jobInfo: JobInfo) => {
    val JobInfo(
      company,
      title,
      description,
      externalUrl,
      remote,
      location,
      salaryLo,
      salaryHi,
      currency,
      country,
      tags,
      image,
      seniority,
      other
    ) = jobInfo

    val validCompany     = validateRequired(company, "company")(_.nonEmpty)
    val validTitle       = validateRequired(title, "title")(_.nonEmpty)
    val validDescription = validateRequired(description, "description")(_.nonEmpty)
    val validExternalUrl = validateUrl(externalUrl, "externalUrl")
    val validLocation    = validateRequired(location, "location")(_.nonEmpty)

    (
      validCompany,
      validTitle,
      validDescription,
      validExternalUrl,
      remote.validNel,
      validLocation,
      salaryLo.validNel,
      salaryHi.validNel,
      currency.validNel,
      country.validNel,
      tags.validNel,
      image.validNel,
      seniority.validNel,
      other.validNel
    ).mapN(JobInfo.apply) // ValidatedNel[ValidationFailure, JobInfo]
  }

  given loginInfoValidator: Validator[LoginInfo] = (loginInfo: LoginInfo) => {
    val validUserEmail = validateRequired(loginInfo.email, "email")(_.nonEmpty)
      .andThen(email => validateEmail(email, "email"))
    val validUserPassword = validateRequired(loginInfo.password, "password")(_.nonEmpty)
    (validUserEmail, validUserPassword).mapN(LoginInfo.apply)
  }

  given newUserInfoValidator: Validator[NewUserInfo] = (newUserInfo: NewUserInfo) => {
    val validEUserEmail = validateRequired(newUserInfo.email, "email")(_.nonEmpty)
      .andThen(email => validateEmail(email, "email"))
    val validUserPassword = validateRequired(newUserInfo.password, "password")(_.nonEmpty)

    (
      validEUserEmail,
      validUserPassword,
      newUserInfo.firstName.validNel,
      newUserInfo.lastName.validNel,
      newUserInfo.company.validNel
    ).mapN(NewUserInfo.apply)
  }

  given newPasswordInfo: Validator[NewPasswordInfo] = (newPasswordInfo: NewPasswordInfo) => {
    val validOldPassword = validateRequired(newPasswordInfo.oldPassword, "oldPassword")(_.nonEmpty)
    val validNewPassword = validateRequired(newPasswordInfo.newPassword, "newPassword")(_.nonEmpty)

    (validOldPassword, validNewPassword).mapN(NewPasswordInfo.apply)
  }
}
