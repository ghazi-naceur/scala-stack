package rop.jobsboard.common

import cats.effect.IO
import tyrian.*
import tyrian.http.*
import io.circe.Encoder
import io.circe.syntax.*
import rop.jobsboard.core.Session

trait Endpoint[M] {

  val location: String
  val method: Method
  val onSuccess: Response => M
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
      Decoder[M](onSuccess, onError) // The decoder is the response parser. It emits a message based on the error
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
      Decoder[M](onSuccess, onError) // The decoder is the response parser. It emits a message based on the error
      // state of the request (onError) or the response that gets back from the server (onSuccess).
    )
}
