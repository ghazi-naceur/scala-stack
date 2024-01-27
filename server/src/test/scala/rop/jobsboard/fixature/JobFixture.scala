package rop.jobsboard.fixature

import cats.syntax.all.*
import rop.jobsboard.domain.Job.{Job, JobInfo}

import java.util.UUID
trait JobFixture {

  val NotFoundJobUuid: UUID = UUID.fromString("6ea79557-3112-4c84-a8f5-1d1e2c300948")

  val TestJobUuid: UUID = UUID.fromString("843df718-ec6e-4d49-9289-f799c0f40064")

  val TestJob: Job = Job(
    TestJobUuid,
    1659186086L,
    "someone@google.com",
    JobInfo(
      "Some Company",
      "Tech Lead",
      "A job in Amsterdam",
      "https://google.com/somejob",
      false,
      "Amsterdam",
      2000.some,
      3000.some,
      "EUR".some,
      "Netherlands".some,
      Some(List("scala", "scala-3", "cats")),
      None,
      "Senior".some,
      None
    ),
    active = true
  )

  val InvalidJob: Job = Job(
    null,
    42L,
    "nothing@gmail.com",
    JobInfo.empty
  )

  val UpdatedTestJob: Job = Job(
    TestJobUuid,
    1659186086L,
    "someone@google.com",
    JobInfo(
      "Some Company (Spain Branch)",
      "Engineering Manager",
      "An some job in Barcelona",
      "http://www.some.com",
      false,
      "Barcelona",
      2200.some,
      3200.some,
      "USD".some,
      "Spain".some,
      Some(List("scala", "scala-3", "cats effect")),
      "http://www.some.com/logo.png".some,
      "Highest".some,
      "Some additional info".some
    ),
    active = true
  )

  val CorpNewJob: JobInfo = JobInfo(
    "Corp",
    "Technical Author",
    "Some Corp",
    "https://google.com/",
    true,
    "From remote",
    2000.some,
    3500.some,
    "EUR".some,
    "Romania".some,
    Some(List("scala", "scala-3", "cats", "akka", "spark", "flink", "cats effect")),
    None,
    "High".some,
    None
  )

  val CorpJobWithNotFoundId: Job = TestJob.copy(id = NotFoundJobUuid)

  val AnotherTestJobUuid: UUID = UUID.fromString("19a941d0-aa19-477b-9ab0-a7033ae65c2b")
  val AnotherTestJob: Job      = TestJob.copy(id = AnotherTestJobUuid)

  val CorpTestJob: Job =
    TestJob.copy(jobInfo = TestJob.jobInfo.copy(company = "Corp"))

  val NewJobUuid: UUID = UUID.fromString("efcd2a64-4463-453a-ada8-b1bae1db4377")
  val TestNewJobInfo: JobInfo = JobInfo(
    "Some Company",
    "Tech Lead",
    "A job in Amsterdam",
    "example.com",
    false,
    "Amsterdam",
    2000.some,
    3000.some,
    "EUR".some,
    "Netherlands".some,
    Some(List("scala", "scala-3", "cats")),
    None,
    "High".some,
    None
  )
}
