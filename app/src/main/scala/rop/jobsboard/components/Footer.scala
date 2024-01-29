package rop.jobsboard.components

import rop.jobsboard.App
import tyrian.*
import tyrian.Html.*
import scala.scalajs.js.Date

object Footer {

  def view(): Html[App.Msg] =
    div(`class` := "footer")(
      p(
        text("Written in "),
        a(href := "https://scala-lang.org", target := "blank")("Scala"),
        text(" with \uD83E\uDD1D at "),
        a(href := "https://ropthecorp.org", target := "blank")("ROP")
      ),
      p(s"© Corp ${new Date().getFullYear()}")
    )
}
