package rop.jobsboard.core

import cats.effect.*
import cats.effect.implicits.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import rop.jobsboard.domain.Job.JobFilter
import rop.jobsboard.domain.pagination.Pagination
import rop.jobsboard.fixature.JobFixture

class JobsSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers with DoobieSpec with JobFixture {

  override val initScript: String = "sql/jobs.sql"

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  "Jobs 'algebra'" - {
    "should return no job if the given UUID does not exist" in {
      transactor.use { xa =>
        val program = for {
          jobs      <- LiveJobs[IO](xa)
          retrieved <- jobs.find(NotFoundJobUuid)
        } yield retrieved

        program.asserting(_ shouldBe None)
      }
    }

    "should retrieve job by id" in {
      transactor.use { xa =>
        val program = for {
          jobs      <- LiveJobs[IO](xa)
          retrieved <- jobs.find(TestJobUuid)
        } yield retrieved

        program.asserting(_ shouldBe Some(TestJob))
      }
    }

    "should retrieve None when trying to retrieve a job that doesn't exist" in {
      transactor.use { xa =>
        val program = for {
          jobs      <- LiveJobs[IO](xa)
          retrieved <- jobs.find(NotFoundJobUuid)
        } yield retrieved

        program.asserting(_ shouldBe None)
      }
    }

    "should retrieve all jobs" in {
      transactor.use { xa =>
        val program = for {
          jobs      <- LiveJobs[IO](xa)
          retrieved <- jobs.all()
        } yield retrieved

        program.asserting(_ shouldBe List(TestJob))
      }
    }

    "should create a new job" in {
      transactor.use { xa =>
        val program = for {
          jobs      <- LiveJobs[IO](xa)
          jobId     <- jobs.create("someone@gmail.com", TestNewJobInfo)
          retrieved <- jobs.find(jobId)
        } yield retrieved

        program.asserting(_.map(_.jobInfo) shouldBe Some(TestNewJobInfo))
      }
    }

    "should update a job if it exists" in {
      transactor.use { xa =>
        val program = for {
          jobs      <- LiveJobs[IO](xa)
          retrieved <- jobs.update(TestJobUuid, UpdatedTestJob.jobInfo)

        } yield retrieved

        program.asserting(_ shouldBe Some(UpdatedTestJob))
      }
    }

    "should return a None when trying to update a non-existent job" in {
      transactor.use { xa =>
        val program = for {
          jobs      <- LiveJobs[IO](xa)
          retrieved <- jobs.update(NotFoundJobUuid, UpdatedTestJob.jobInfo)

        } yield retrieved

        program.asserting(_ shouldBe None)
      }
    }

    "should delete a job if it exists" in {
      transactor.use { xa =>
        val program = for {
          jobs      <- LiveJobs[IO](xa)
          retrieved <- jobs.delete(TestJobUuid)
        } yield retrieved

        program.asserting(_ shouldBe 1)
      }
    }

    "should delete an existing job" in {
      transactor.use { xa =>
        val program = for {
          jobs            <- LiveJobs[IO](xa)
          nbOfDeletedJobs <- jobs.delete(TestJobUuid)
          countOfJobs     <- sql"SELECT COUNT(*) FROM jobs WHERE id = $TestJobUuid".query[Int].unique.transact(xa)
        } yield (nbOfDeletedJobs, countOfJobs)

        program.asserting { case (nbOfDeletedJobs, countOfJobs) =>
          nbOfDeletedJobs shouldBe 1
          countOfJobs shouldBe 0
        }
      }
    }

    "should return 0 deleted jobs when trying to delete a non-existent job" in {
      transactor.use { xa =>
        val program = for {
          jobs            <- LiveJobs[IO](xa)
          nbOfDeletedJobs <- jobs.delete(NotFoundJobUuid)
        } yield nbOfDeletedJobs

        program.asserting(_ shouldBe 0)
      }
    }

    "should filter remote jobs" in {
      transactor.use { xa =>
        val program = for {
          jobs <- LiveJobs[IO](xa)
          jobs <- jobs.all(JobFilter(remote = true), Pagination.default)
        } yield jobs

        program.asserting(_ shouldBe List())
      }
    }

    "should filter jobs by tags" in {
      transactor.use { xa =>
        val program = for {
          jobs <- LiveJobs[IO](xa)
          jobs <- jobs.all(JobFilter(tags = List("scala", "cats", "not-found")), Pagination.default)
        } yield jobs

        program.asserting(_ shouldBe List(TestJob))
      }
    }

    "should surface a comprehensive filter out of all jobs contained" in {
      transactor.use { xa =>
        val program = for {
          jobs   <- LiveJobs[IO](xa)
          filter <- jobs.possibleFilters()
        } yield filter

        program.asserting { case JobFilter(companies, locations, countries, seniorities, tags, maxSalary, remote) =>
          companies shouldBe List("Some Company")
          locations shouldBe List("Amsterdam")
          countries shouldBe List("Netherlands")
          seniorities shouldBe List("Senior")
          tags.toSet shouldBe Set("scala", "scala-3", "cats")
          maxSalary shouldBe Some(3000)
          remote shouldBe false
        }
      }
    }
  }
}
