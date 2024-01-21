package rop.jobsboard.components

import tyrian.*
import tyrian.Html.*
import rop.jobsboard.App
import rop.jobsboard.domain.Job.Job
import rop.jobsboard.pages.Page

object JobComponents {

  def card(job: Job): Html[App.Msg] =
    div(`class` := "jvm-recent-jobs-cards")(
      div(`class` := "jvm-recent-jobs-card-img")(
        img(
          `class` := "img-fluid",
          src     := job.jobInfo.image.getOrElse(""),
          alt     := job.jobInfo.title
        )
      ),
      div(`class` := "jvm-recent-jobs-card-contents")(
        h5(
          Anchors.renderSimpleNavLink(
            s"${job.jobInfo.company} - ${job.jobInfo.title}",
            Page.Urls.JOB(job.id.toString),
            "job-title-link"
          )
        ),
        renderJobSummary(job)
      ),
      div(`class` := "jvm-recent-jobs-card-btn-apply")(
        a(href := job.jobInfo.externalUrl, target := "blank")(
          button(`type` := "button", `class` := "btn btn-danger")("Apply")
        )
      )
    )

  def renderJobSummary(job: Job): Html[App.Msg] =
    div(`class` := "job-summary")(
      renderDetail("euro", fullSalaryString(job)),
      renderDetail("location-dot", fullLocationString(job)),
      maybeRenderDetail("ranking-star", job.jobInfo.seniority),
      maybeRenderDetail("tags", job.jobInfo.tags.map(_.mkString(", ")))
    )

  def maybeRenderDetail(icon: String, potentialValue: Option[String]): Html[App.Msg] =
    potentialValue.map(value => renderDetail(icon, value)).getOrElse(div())

  def renderDetail(icon: String, value: String): Html[App.Msg] =
    div(`class` := "job-detail")(
      i(`class` := s"fa fa-$icon job-detail-icon")(),
      p(`class` := "job-detail-value")(value)
    )

  private def fullSalaryString(job: Job) = {
    val currency = job.jobInfo.currency.getOrElse("")
    (job.jobInfo.salaryLo, job.jobInfo.salaryHi) match {
      case (Some(lo), Some(hi)) => s"$currency $lo-$hi"
      case (Some(lo), None)     => s"> $currency $lo"
      case (None, Some(hi))     => s"Up to $currency $hi"
      case (None, None)         => "Unspecified salary = ∞"
    }
  }

  private def fullLocationString(job: Job) = job.jobInfo.country match {
    case Some(country) => s"${job.jobInfo.location}, $country"
    case None          => job.jobInfo.location
  }
}
