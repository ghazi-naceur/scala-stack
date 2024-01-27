package rop.jobsboard.common

import cats.effect.IO
import tyrian.*
import tyrian.http.*
import io.circe.Encoder
import io.circe.parser.parse
import io.circe.syntax.*
import rop.jobsboard.core.Session

trait Endpoint[M] {

  val location: String
  val method: Method
  val onResponse: Response => M
  val onError: HttpError => M

  // Public API
  def call[A: Encoder](payload: A): Cmd[IO, M] = internalCall(payload, None)
  def call(): Cmd[IO, M]                       = internalCall(None)

  def callAuthorized[A: Encoder](payload: A): Cmd[IO, M] = internalCall(payload, Session.getUserToken)
  def callAuthorized(): Cmd[IO, M]                       = internalCall(Session.getUserToken)

  // Private API
  private def internalCall[A: Encoder](payload: A, authorization: Option[String]): Cmd[IO, M] =
    Http.send(
      Request(
        url = location,
        method = method,
        headers = authorization.map(token => Header("Authorization", token)).toList,
        body = Body.json(payload.asJson.toString),
        timeout = Request.DefaultTimeOut,
        withCredentials = false
      ),
      Decoder[M](onResponse, onError) // The decoder is the response parser. It emits a message based on the error
      // state of the request (onError) or the response that gets back from the server (onSuccess).
    )

  private def internalCall(authorization: Option[String]): Cmd[IO, M] =
    Http.send(
      Request(
        url = location,
        method = method,
        headers = authorization.map(token => Header("Authorization", token)).toList,
        body = Body.Empty,
        timeout = Request.DefaultTimeOut,
        withCredentials = false
      ),
      Decoder[M](onResponse, onError) // The decoder is the response parser. It emits a message based on the error
      // state of the request (onError) or the response that gets back from the server (onSuccess).
    )
}

object Endpoint {

  def onResponse[A: io.circe.Decoder, Msg](valueCallback: A => Msg, errorCallback: String => Msg): Response => Msg =
    response =>
      response.status match {
        case Status(s, _) if s >= 200 && s < 300 =>
          val json   = response.body
          val parsed = parse(json).flatMap(_.as[A])
          parsed match {
            case Right(value)       => valueCallback(value)
            case Left(parsingError) => errorCallback(s"Parsing error: $parsingError")
          }
        case Status(code, message) if code >= 400 && code < 600 =>
          errorCallback(s"Error: $message")
      }

  def onResponseText[Msg](
      valueCallback: String => Msg,
      errorCallback: String => Msg
  ): Response => Msg = response =>
    response.status match {
      case Status(s, _) if s >= 200 && s < 300 =>
        valueCallback(response.body)
      case Status(s, _) if s >= 400 && s < 500 =>
        val jsonError = response.body
        val parsed    = parse(jsonError).flatMap(_.hcursor.get[String]("error"))
        parsed match {
          case Right(errorFromServer) => errorCallback(errorFromServer)
          case Left(error)            => errorCallback(s"Error $error.")
        }
      case _ => errorCallback("Unknown reply from server.")
    }
}
