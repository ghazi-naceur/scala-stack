package rop.jobsboard.pages
import cats.effect.IO
import tyrian.Html.div
import tyrian.{Cmd, Html}

final case class JobListPage() extends Page {
  override def initCmd: Cmd[IO, Page.Msg] = Cmd.None // todo

  override def update(msg: Page.Msg): (Page, Cmd[IO, Page.Msg]) = (this, Cmd.None) // todo

  override def view(): Html[Page.Msg] = div("Job list page - todo")
}
