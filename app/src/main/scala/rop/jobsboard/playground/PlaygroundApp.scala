package rop.jobsboard.playground

import cats.effect.*
import org.scalajs.dom.{console, document}
import rop.jobsboard.playground.PlaygroundApp.{Decrement, Increment, Model, Msg}
import tyrian.*
import tyrian.Html.*
import tyrian.cmds.Logger

import scala.concurrent.duration.*
import scala.language.postfixOps
import scala.scalajs.js.annotation.*

object PlaygroundApp {
  sealed trait Msg
  case class Increment(amount: Int) extends Msg
  case class Decrement(amount: Int) extends Msg
  case class Model(count: Int)

}

/*
  1- Tyrian concepts:

  - Model: the state of the entire app
  - Message: piece of data handled to update the model, that can be sent by:
_____* a command: an action that results in a message. Cmd is essentially a wrapper over IO.
                    + emitting a pure message: (Cmd.Emit ...etc)
                    + performing a side effect
                    + async computation, e.g. HTTP
                    + combining multiple commands
_____* a subscription: (Sub.every ...etc)
_____* an event: (OnClick ...etc)

  2- Tyrian functions:

  - init:
      (+) creates the initial model
      (+) triggers the first command
  - update:
      (+) model + message => new model
      (+) may emit a command
      (+) usually handled with PFs
  - subscriptions:
      (+) returns a stream of messages
  - view:
      (+) renders the content based on model
      (+) triggered automatically every time model is changed

  3- Tyrian App Scaling:
    - Message types:
      (+) keep them in companions
      (+) use sealed traits, enums, union types
    - One App, many parts
      (+) each part messages its own state
      (+) app model built out of all parts' models
      (+) each message propagated "down" to the responsible component
      (+) each view call propagated "down" to render components first
 */

//@JSExportTopLevel("PlaygroundApp")
class PlaygroundApp extends TyrianApp[Msg, PlaygroundApp.Model] {

  override def init(flags: Map[String, String]): (PlaygroundApp.Model, Cmd[IO, Msg]) = {
    (PlaygroundApp.Model(0), Cmd.None)
  }

  override def update(model: PlaygroundApp.Model): Msg => (PlaygroundApp.Model, Cmd[IO, Msg]) = {
    case Increment(amount) =>
      console.log(s"Logging with ScalaJS- Changing count by: $amount")
      (model.copy(count = model.count + amount), Logger.consoleLog[IO](s"Logging with Tyrian- Changing count by: $amount"))
    case Decrement(amount) =>
      console.log(s"Logging with ScalaJS- Changing count by: -$amount")
      (model.copy(count = model.count - amount), Logger.consoleLog[IO](s"Logging with Tyrian- Changing count by: -$amount"))
  }

  override def view(model: PlaygroundApp.Model): Html[Msg] = {
    div(
      button(onClick(Increment(1)))("increase"),
      button(onClick(Increment(-2)))("minus 2"),
      button(onClick(Decrement(1)))("decrease"),
      div(s"Tyrian is running.. This is the new model '${model.count}'")
    )
  }

  override def subscriptions(model: PlaygroundApp.Model): Sub[IO, Msg] = {
    Sub.every[IO](1 second).map(_ => Increment(1))
  }
}
