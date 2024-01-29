package rop.jobsboard.pages

import cats.effect.IO
import rop.jobsboard.App
import rop.jobsboard.common.Constants
import tyrian.Html.*
import tyrian.{Cmd, Html}

final case class NotFoundPage() extends Page {
  override def initCmd: Cmd[IO, App.Msg] = Cmd.None

  override def update(msg: App.Msg): (Page, Cmd[IO, App.Msg]) = (this, Cmd.None)

  override def view(): Html[App.Msg] =
    div(`class` := "row")(
      div(`class` := "col-md-5 p-0")(
        // left side
        div(`class` := "logo")(
          img(src   := Constants.logoImage)
        )
      ),
      div(`class` := "col-md-7")(
        // right side
        div(`class` := "form-section")(
          div(`class` := "top-section")(
            h1(span("\uD83D\uDD75\uFE0F")),
            div("This page does not exist.")
          )
        )
      )
    )
}
