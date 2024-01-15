package rop.jobsboard.components

import cats.effect.*
import tyrian.*
import tyrian.Html.*

trait Component[Msg, +Model] {
// Making "+Model" covariant because the model is contained within the component

  // send a command upon instantiating
  def initCmd: Cmd[IO, Msg]

  // update content of the page
  def update(msg: Msg): (Model, Cmd[IO, Msg])

  // render the page
  def view(): Html[Msg]
}
