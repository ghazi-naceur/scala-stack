package rop.jobsboard.pages

import cats.effect.IO
import tyrian.Html.div
import tyrian.{Cmd, Html}

final case class JobPage(id: String) extends Page {
  override def initCmd: Cmd[IO, Page.Msg] = Cmd.None // todo

  override def update(msg: Page.Msg): (Page, Cmd[IO, Page.Msg]) = (this, Cmd.None) // todo

  override def view(): Html[Page.Msg] = div(s"Individual Job page for id $id - todo")
}
