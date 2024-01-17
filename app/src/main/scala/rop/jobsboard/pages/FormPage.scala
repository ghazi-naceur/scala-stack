package rop.jobsboard.pages
import cats.effect.IO
import org.scalajs.dom.{File, HTMLFormElement, HTMLInputElement, document}
import rop.jobsboard.App
import rop.jobsboard.core.Router
import rop.jobsboard.pages.Page.Status
import tyrian.*
import tyrian.Html.*

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

abstract class FormPage(title: String, status: Option[Status]) extends Page {

  val pageFormId = "form"

  // Public API
  override def initCmd: Cmd[IO, App.Msg] = {
    // Browser saves values in form between pages. That data is corrupted moving from 1 page to another, so we need too
    // clear the form when we load it
    clearForm()
  }

  override def update(msg: App.Msg): (Page, Cmd[IO, App.Msg]) = ???

  override def view(): Html[App.Msg] = renderForm()

  // Abstract API
  protected def renderFormContent(): List[Html[App.Msg]] = ??? // for every page to override

  // Protected API
  protected def renderForm(): Html[App.Msg] =
    div(`class` := "form-section")(
      div(`class` := "top-section")(
        h1(title)
      ),
      // 'preventDefault()' is used to prevent refreshing the page after submitting a form, to avoid loosing state
      form(
        name    := "signin",
        `class` := "form",
        id      := pageFormId,
        onEvent(
          "submit",
          e => {
            e.preventDefault()
            App.NoOp // won't change the state of the page.
            // So everytime the form is submitted, we're going to send 'NoOp' which is not going to change the state of the page
          }
        )
      )(renderFormContent()),
      status.map(s => div(s.message)).getOrElse(div())
    )

  protected def renderInput(name: String, uid: String, kind: String, isRequired: Boolean, onChange: String => App.Msg) =
    div(`class` := "form-input")(
      label(`for` := uid, `class` := "form-label")(
        if (isRequired) span("*") else span(),
        text(name)
      ),
      input(`type` := kind, `class` := "form-control", id := uid, onInput(onChange))
    )

  protected def renderImageUploadInput(
      name: String,
      uid: String,
      imageSource: Option[String],
      onChange: Option[File] => App.Msg
  ) =
    div(`class` := "form-input")(
      label(`for` := uid, `class` := "form-label")(name),
      input(
        `type`  := "file",
        `class` := "form-control",
        id      := uid,
        accept  := "image/*",
        onEvent(
          "change",
          e => {
            val imageInput = e.target.asInstanceOf[HTMLInputElement]
            val fileList   = imageInput.files
            if (fileList.length > 0)
              onChange(Some(fileList(0)))
            else onChange(None)
          }
        )
      ),
      img(
        id     := "preview",
        src    := imageSource.getOrElse(""),
        alt    := "Preview",
        width  := "100",
        height := "100"
      )
    )

  protected def renderTextArea(name: String, uid: String, isRequired: Boolean, onChange: String => App.Msg) =
    div(`class` := "form-input")(
      label(`for` := uid, `class` := "form-label")(
        if (isRequired) span("*") else span(),
        text(name)
      ),
      textarea(`class` := "form-control", id := uid, onInput(onChange))("")
    )

    /*
      Check if the form has loaded (if it's present on the page) by invoking 'document.getElementById()'. If the previous
      statement didn't return anything, we need to check again, if the element is null.. and we keep rechecking while the
      element is null.. This recheck needs to be periodic (like every 100 millis): The issue here is that Javascript is
      single-threaded, so if we block the page 100 ms, until we get the form element => This is a bad solution.
      In order to fix this issue, we can perform the period (100 ms) in terms of 'IO.sleep', and that's because Cats Effect
      maintains its power of concurrency even on single-threaded machines: So the solution is 'Cmd.Run'
     */
  private def clearForm() =
    Cmd.Run[IO, Unit, App.Msg] {
      // Cmd.Run will take the IO effect. Its argument will be a function that accepts the value returned by the IO effect
      // to return App.Msg that will be sent to the main application
      def effect: IO[Option[HTMLFormElement]] = for {
        potentialForm <- IO(Option(document.getElementById(pageFormId).asInstanceOf[HTMLFormElement]))
        finalForm <-
          if (potentialForm.isEmpty) IO.sleep(FiniteDuration(100, "millis")) *> effect
          else IO(potentialForm)
      } yield finalForm

      effect.map(_.foreach(_.reset()))

    }(_ => App.NoOp)
}
