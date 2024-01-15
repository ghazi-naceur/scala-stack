package rop.jobsboard.components

import cats.effect.IO
import rop.jobsboard.App
import rop.jobsboard.domain.Job.JobFilter
import rop.jobsboard.common.{Constants, Endpoint}
import io.circe.generic.auto.*
import rop.jobsboard.components.FilterPanel.Commands
import tyrian.*
import tyrian.Html.*
import tyrian.http.*
import tyrian.http.Method.Get

final case class FilterPanel(possibleFilters: JobFilter = JobFilter(), maybeError: Option[String] = None)
    extends Component[App.Msg, FilterPanel] {

  import FilterPanel.*

  override def initCmd: Cmd[IO, App.Msg] = Commands.getFilters

  override def update(msg: App.Msg): (FilterPanel, Cmd[IO, App.Msg]) = msg match {
    case SetPossibleFilters(possibleFilters) => (this.copy(possibleFilters = possibleFilters), Cmd.None)
    case FilterPanelError(error)             => (this.copy(maybeError = Some(error)), Cmd.None)
    case _                                   => (this, Cmd.None)
  }

  override def view(): Html[App.Msg] =
    div(`class` := "filter-panel-container")(
      maybeRenderError(),
      div(possibleFilters.toString)
    )

  private def maybeRenderError() =
    maybeError
      .map { e =>
        div(`class` := "filter-panel-error")(e)
      }
      .getOrElse(div())
}

object FilterPanel {

  trait Msg                                                 extends App.Msg
  case class FilterPanelError(error: String)                extends Msg
  case class SetPossibleFilters(possibleFilters: JobFilter) extends Msg

  object Endpoints {
    val getFilters: Endpoint[Msg] = new Endpoint[Msg] {
      override val location: String = Constants.Endpoints.filters
      override val method: Method   = Get
      override val onResponse: Response => Msg =
        Endpoint.onResponse[JobFilter, Msg](
          SetPossibleFilters(_),
          FilterPanelError(_)
        )
      override val onError: HttpError => Msg = e => FilterPanelError(e.toString)
    }
  }
  object Commands {
    def getFilters: Cmd[IO, Msg] = Endpoints.getFilters.call()
  }
}
