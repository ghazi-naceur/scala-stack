package rop.jobsboard.pages

import cats.effect.IO
import rop.jobsboard.App
import tyrian.Html.div
import tyrian.{Cmd, Html}

final case class JobPage(id: String) extends Page {
  override def initCmd: Cmd[IO, App.Msg] = Cmd.None // todo

  override def update(msg: App.Msg): (Page, Cmd[IO, App.Msg]) = (this, Cmd.None) // todo

  override def view(): Html[App.Msg] = div(s"Individual Job page for id $id - todo")
}
