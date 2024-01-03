package rop.jobsboard.pages
import cats.effect.IO
import rop.jobsboard.App
import rop.jobsboard.core.Router
import rop.jobsboard.pages.Page.Status
import tyrian.*
import tyrian.Html.*

abstract class FormPage(title: String, status: Option[Status]) extends Page {

  // Public API
  override def initCmd: Cmd[IO, App.Msg] = Cmd.None

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
      label(`for` := name, `class` := "form-label")(
        if (isRequired) span("*") else span(),
        text(name)
      ),
      input(`type` := kind, `class` := "form-control", id := uid, onInput(onChange))
    )

  protected def renderAuxLink(location: String, text: String): Html[App.Msg] =
    a(
      href    := location,
      `class` := "aux-link",
      onEvent(
        "click",
        e => {
          e.preventDefault() // native JS to prevent reloading the page
          Router.ChangeLocation(location)
        }
      )
    )(text)
}
