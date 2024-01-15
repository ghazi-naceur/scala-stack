package rop.jobsboard.core

import cats.*
import cats.effect.*
import cats.implicits.*
import rop.jobsboard.domain.Job.{Job, JobFilter, JobInfo}
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import doobie.util.*
import org.typelevel.log4cats.Logger
import rop.jobsboard.domain.pagination.Pagination
import rop.jobsboard.logging.syntax.*

import java.util.UUID

trait Jobs[F[_]] {

  def create(ownerEmail: String, jobInfo: JobInfo): F[UUID]
  def all(): F[List[Job]] // todo to be improved
  def all(filter: JobFilter, pagination: Pagination): F[List[Job]]
  def find(id: UUID): F[Option[Job]]
  def update(id: UUID, jobInfo: JobInfo): F[Option[Job]]
  def delete(id: UUID): F[Int]
  def possibleFilters(): F[JobFilter]
}

class LiveJobs[F[_]: MonadCancelThrow: Logger] private (xa: Transactor[F]) extends Jobs[F] {
  override def create(ownerEmail: String, jobInfo: JobInfo): F[UUID] =
    sql"""
         INSERT INTO jobs(
            date,
            ownerEmail,
            company,
            title,
            description,
            externalUrl,
            remote,
            location,
            salaryLo,
            salaryHi,
            currency,
            country,
            tags,
            image,
            seniority,
            other,
            active
         ) VALUES (
            ${System.currentTimeMillis()},
            $ownerEmail,
            ${jobInfo.company},
            ${jobInfo.title},
            ${jobInfo.description},
            ${jobInfo.externalUrl},
            ${jobInfo.remote},
            ${jobInfo.location},
            ${jobInfo.salaryLo},
            ${jobInfo.salaryHi},
            ${jobInfo.currency},
            ${jobInfo.country},
            ${jobInfo.tags},
            ${jobInfo.image},
            ${jobInfo.seniority},
            ${jobInfo.other},
            false
         )
       """.update
      .withUniqueGeneratedKeys[UUID]("id")
      .transact(xa)

  override def all(): F[List[Job]] =
    sql"""
      SELECT
        id,
        date,
        ownerEmail,
        company,
        title,
        description,
        externalUrl,
        remote,
        location,
        salaryLo,
        salaryHi,
        currency,
        country,
        tags,
        image,
        seniority,
        other,
        active
      FROM jobs
    """
      .query[Job]
      .to[List]
      .transact(xa)

  override def all(filter: JobFilter, pagination: Pagination): F[List[Job]] = {

    val selectFragment: Fragment =
      fr"""
        SELECT
             id,
             date,
             ownerEmail,
             company,
             title,
             description,
             externalUrl,
             remote,
             location,
             salaryLo,
             salaryHi,
             currency,
             country,
             tags,
             image,
             seniority,
             other,
             active
        """

    val fromFragment: Fragment =
      fr"FROM jobs"

    /*
        WHERE company in [filter.companies]
        AND  company in [filter.companies]
        AND  company in [filter.companies]
        AND  company in [filter.companies]
        AND (
          tag1=any(tags)
          OR tag2=any(tags)
          OR tag3=any(tags)
          OR ... (for every tag in filter.tags)
        )
        AND salaryHi > [filter.salary]
        AND remote = [filter.remote]
     */

    val whereFragment: Fragment = Fragments.whereAndOpt(
      // company in [filter.companies]
      filter.companies.toNel // Option[NonEmptyList] => Option[Fragment]
        .map(companies => Fragments.in(fr"company", companies)),
      filter.locations.toNel // Option[NonEmptyList] => Option[Fragment]
        .map(locations => Fragments.in(fr"location", locations)),
      filter.countries.toNel // Option[NonEmptyList] => Option[Fragment]
        .map(countries => Fragments.in(fr"country", countries)),
      filter.seniorities.toNel // Option[NonEmptyList] => Option[Fragment]
        .map(seniorities => Fragments.in(fr"seniority", seniorities)),
      filter.tags.toNel.map(tags => // intersection between filter.tags and row's tags
        Fragments.or(tags.toList.map(tag => fr"$tag=any(tags)"): _*)
      ),
      filter.maxSalary.map(salary => fr"salaryHi > $salary"),
      filter.remote.some.filter(identity).map(remote => fr"remote=$remote")
      // .filter(identity) == only remote jobs are being filtered
    )

    val paginationFragment: Fragment =
      fr"ORDER BY id LIMIT ${pagination.limit} OFFSET ${pagination.offset}"

    val statement = selectFragment |+| fromFragment |+| whereFragment |+| paginationFragment

    Logger[F].info(statement.toString) *>
      statement
        .query[Job]
        .to[List]
        .transact(xa)
        .logError(thr => s"Failed query: ${thr.getMessage}")
  }

  override def find(id: UUID): F[Option[Job]] =
    sql"""
         SELECT
            id,
            date,
            ownerEmail,
            company,
            title,
            description,
            externalUrl,
            remote,
            location,
            salaryLo,
            salaryHi,
            currency,
            country,
            tags,
            image,
            seniority,
            other,
            active
         FROM jobs
         WHERE id = $id
       """
      .query[Job]
      .option
      .transact(xa)

  override def update(id: UUID, jobInfo: JobInfo): F[Option[Job]] =
    sql"""
         UPDATE jobs
         SET
            company = ${jobInfo.company},
            title = ${jobInfo.title},
            description = ${jobInfo.description},
            externalUrl = ${jobInfo.externalUrl},
            remote = ${jobInfo.remote},
            location = ${jobInfo.location},
            salaryLo = ${jobInfo.salaryLo},
            salaryHi = ${jobInfo.salaryHi},
            currency = ${jobInfo.currency},
            country = ${jobInfo.country},
            tags = ${jobInfo.tags},
            image = ${jobInfo.image},
            seniority = ${jobInfo.seniority},
            other = ${jobInfo.other}
         WHERE id = $id
       """.update.run
      .transact(xa)
      .flatMap(_ => find(id)) // return the update job

  override def delete(id: UUID): F[Int] =
    sql"""
         DELETE FROM jobs
         WHERE id = $id
       """.update.run
      .transact(xa)

  // select all unique values for companies, locations, countries, seniorities, tags from all rows from the DB
  override def possibleFilters(): F[JobFilter] = {
// Yolo is useful to debug SQL queries
//    val yolo = xa.yolo
//    import yolo.*

    val query = sql"""
       SELECT
         ARRAY(SELECT DISTINCT(company) FROM jobs) AS companies,
         ARRAY(SELECT DISTINCT(location) FROM jobs) AS locations,
         ARRAY(SELECT DISTINCT(country) FROM jobs WHERE country IS NOT NULL) AS countries,
         ARRAY(SELECT DISTINCT(seniority) FROM jobs WHERE seniority IS NOT NULL) AS seniorities,
         ARRAY(SELECT DISTINCT(UNNEST(tags)) FROM jobs) AS tags,
         MAX(salaryHi),
         false
       FROM jobs
       """
      .query[JobFilter]

//    query.check() *>
    query.option
      .transact(xa)
      .map(_.getOrElse(JobFilter()))
  }

}
object LiveJobs {

  // This given is not mandatory, as Doobie can derive 'Read[JJobFilter]' type class instance automatically
  given jobFilterRead: Read[JobFilter] = Read[
    (
        List[String],
        List[String],
        List[String],
        List[String],
        List[String],
        Option[Int],
        Boolean
    )
  ].map { case (companies, locations, countries, seniorities, tags, maxSalary, remote) =>
    JobFilter(companies, locations, countries, seniorities, tags, maxSalary, remote)
  }

  given jobRead: Read[Job] = Read[
    (
        UUID,                 // id
        Long,                 // date
        String,               // ownerEmail
        String,               // company
        String,               // title
        String,               // description
        String,               // externalUrl
        Boolean,              // remote
        String,               // location
        Option[Int],          // salaryLo
        Option[Int],          // salaryHi
        Option[String],       // currency
        Option[String],       // country
        Option[List[String]], // tags
        Option[String],       // image
        Option[String],       // seniority
        Option[String],       // other
        Boolean               // active
    )
  ].map {
    case (
          id: UUID,
          date: Long,
          ownerEmail: String,
          company: String,
          title: String,
          description: String,
          externalUrl: String,
          remote: Boolean,
          location: String,
          salaryLo: Option[Int] @unchecked,
          salaryHi: Option[Int] @unchecked,
          currency: Option[String] @unchecked,
          country: Option[String] @unchecked,
          tags: Option[List[String]] @unchecked,
          image: Option[String] @unchecked,
          seniority: Option[String] @unchecked,
          other: Option[String] @unchecked,
          active: Boolean
        ) =>
      Job(
        id = id,
        date = date,
        ownerEmail = ownerEmail,
        jobInfo = JobInfo(
          company = company,
          title = title,
          description = description,
          externalUrl = externalUrl,
          remote = remote,
          location = location,
          salaryLo = salaryLo,
          salaryHi = salaryHi,
          currency = currency,
          country = country,
          tags = tags,
          image = image,
          seniority = seniority,
          other = other
        ),
        active = active
      )
  }

  def apply[F[_]: MonadCancelThrow: Logger](xa: Transactor[F]): F[LiveJobs[F]] = new LiveJobs[F](xa).pure[F]
}
