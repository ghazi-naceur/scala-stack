package rop.jobsboard.common

import cats.effect.IO
import tyrian.*
import tyrian.http.*
import io.circe.Encoder
import io.circe.syntax.*

trait Endpoint[M] {

  val location: String
  val method: Method
  val onSuccess: Response => M
  val onError: HttpError => M

  def call[A: Encoder](payload: A): Cmd[IO, M] =
    Http.send(
      Request(
        url = location,
        method = method,
        headers = List(),
        body = Body.json(payload.asJson.toString),
        timeout = Request.DefaultTimeOut,
        withCredentials = false
      ),
      Decoder[M](onSuccess, onError) // The decoder is the response parser. It emits a message based on the error
      // state of the request (onError) or the response that gets back from the server (onSuccess).
    )
}
