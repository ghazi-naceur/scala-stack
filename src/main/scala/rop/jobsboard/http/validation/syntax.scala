package rop.jobsboard.http.validation

import io.circe.generic.auto.*
import org.http4s.circe.CirceEntityCodec.*
import cats.*
import cats.implicits.*
import cats.data.*
import cats.data.Validated.*
import org.http4s.*
import org.http4s.dsl.*
import org.http4s.dsl.impl.*
import org.http4s.implicits.*
import org.typelevel.log4cats.Logger
import rop.jobsboard.http.responses.FailureResponse
import rop.jobsboard.http.validation.Validators.*
import rop.jobsboard.logging.syntax.*

object syntax {

  def validateEntity[A](entity: A)(using validator: Validator[A]): ValidationResult[A] =
    validator.validate(entity)

  trait HttpValidationDsl[F[_]: MonadThrow: Logger] extends Http4sDsl[F] {
    extension (req: Request[F])
      def validate[A: Validator](serverLogicIfValid: A => F[Response[F]])(using EntityDecoder[F, A]): F[Response[F]] = {
        req
          .as[A]
          .logError(error => s"Parsing payload failed: $error")
          .map(validateEntity) // F[ValidationResult[A]]
          .flatMap {
            case Valid(entity)   => serverLogicIfValid(entity) // F[Response[F]]
            case Invalid(errors) => BadRequest(FailureResponse(errors.toList.map(_.errorMessage).mkString(", ")))
            // 'BadRequest' need to be identified inside a trait extending 'Http4sDsl'
          }
      }
  }

}
