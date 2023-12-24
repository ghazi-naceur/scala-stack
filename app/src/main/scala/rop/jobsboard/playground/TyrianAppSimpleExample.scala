package rop.jobsboard.playground

import cats.effect.*
import org.scalajs.dom.document
import tyrian.*
import tyrian.Html.*

import scala.concurrent.duration.*
import scala.language.postfixOps
import scala.scalajs.js.annotation.*

// this class will be compiled to a regular javascript and it will be referred to from javascript as module called "JobsBoardFE"
//@JSExportTopLevel("TyrianAppSimpleExample")
class TyrianAppSimpleExample extends TyrianApp[Int, String] {
  /*
  TyrianApp[Int, String]:
    - Int represents the message
    - String represents the model or the state of the entire application (nb of jobs, history of navigation, which user is logged in ...etc)

  How to send a message:
    - trigger a command (Cmd[IO, Int]). Tyrian runtime will automatically take out this data structure, take the message
      and shave it through the TyrianApp internal logic, which will internally call the 'update' function, and then if the
      model changes, the 'view' function will be triggered
    - create a subscription
    - listen for an event

  As a programmer, we are never the ones to call these 4 functions (init, update, view and subscriptions) manually. It's the
    'launch' function provided by Tyrian, who's calling these 4 functions.
  The 'launch' function spin up the TyrianApp, by running the 'init' function.
   */

  override def init(flags: Map[String, String]): (String, Cmd[IO, Int]) = {
    // The 'init' function takes flags as arguments.
    // The 'init' function returns the initial state of the app (String) and the 1st command that ever gets to run

//    ("", Cmd.None)
    //  We have message/state "" which will be rendered in the 'view', then we have Cmd.None means no message,
    //  means no 'update' will triggered, means no 'view' will triggered => as a result this will be displayed:
    // "Tyrian is running.. This is the new ''" in view function.

    ("", Cmd.Emit(1))
  }

  override def update(model: String): Int => (String, Cmd[IO, Int]) = {
    // The model (String) can change by receiving messages (Int)
    // The signature of this function is: model => message => (new model, new command)
    // => This is a state transition: so if we have a model and a message, then we will automatically obtain a new model
    // This function is triggered whenever we get a new message ==> and when the model changes, the 'view' function will
    // also be triggered
    message => (model + ", " + message, Cmd.None) // Since no command is being sent, the view/rendering won't change
//    message => (model + ", " + message, Cmd.Emit(1)) // Since a command is being sent, the view/rendering will change
  }

  override def view(model: String): Html[Int] = {
    // Based on the "model" or "state", which in our case a 'String', we can render HTML content using 'view' function
    // Html[Int] is a virtual DOM
    // 'view' is triggered every single time the model (or the state (a String in our case)) changes
    // The 'model' (or 'state') is changed via messages (which are of type 'Int' in our case)
    // Messages are being set and handled in the 'update' function
    div(
      button(onClick(1))("Click button"),
      div(s"Tyrian is running.. This is the new model '$model'")
    )
  }

  override def subscriptions(model: String): Sub[IO, Int] = {
    // A potentially endless stream of messages of type Int in our case
    // You can listen for updates for a particular value, or listen for time updates or listen for clicks, or listen for
    // other subscriptions or even FS streams (powerful.. Tyrian is based on FS2)
//    Sub.every[IO](1 second).map(_ => 1) // emits 1 every second => 'update' function will called every second
    // => 'view' will be triggered every second as well
    Sub.None
  }
}
