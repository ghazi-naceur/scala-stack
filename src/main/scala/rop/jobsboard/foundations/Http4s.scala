package rop.jobsboard.foundations

import cats.{Applicative, Monad}
import cats.effect.{IO, IOApp, MonadCancel}
import cats.*
import cats.data.Kleisli
import cats.implicits.*
import io.circe.generic.auto.*
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.headers.*
import org.http4s.{Header, HttpRoutes}
import org.http4s.dsl.*
import org.http4s.dsl.impl.*
import org.http4s.server.*
import org.http4s.ember.server.EmberServerBuilder
import org.typelevel.ci.CIString

import java.util.UUID

// 4
object Http4s extends IOApp.Simple {

  // Simulate HTTP server with students and courses
  type Student = String
  case class Instructor(firstName: String, lastName: String)
  case class Course(
      id: String,
      title: String,
      year: Int,
      students: List[Student],
      instructorName: String
  )

  object CourseRepository {
    // a "database"
    val hunterCourse: Course = Course(
      "be259bdc-3676-4283-862e-40ac72ba0c1a",
      "Nin course",
      2011,
      List("Zoldik", "Freecss"),
      "Isaac Netero"
    )

    private val courses: Map[String, Course] = Map(hunterCourse.id -> hunterCourse)

    // API
    def findCoursesById(courseId: UUID): Option[Course] =
      courses.get(courseId.toString)
    def findCoursesByInstructor(instructorName: String): List[Course] =
      courses.values.filter(_.instructorName == instructorName).toList
  }

  // REST endpoints
  // http GET 'localhost:8080/courses?instructor=Isaac%20Netero'
  // http GET 'localhost:8080/courses?instructor=Isaac%20Netero&year=2011'
  // http GET 'localhost:8080/courses?instructor=Isaac%20Netero&year=2005'
  // http GET 'localhost:8080/courses?instructor=Isaac%20Netero&year=abc'
  // http GET 'localhost:8080/courses/be259bdc-3676-4283-862e-40ac72ba0c1a/students'
  // http GET 'localhost:8080/courses/notfound-uuid/students'
  object InstructorQueryParamMatcher extends QueryParamDecoderMatcher[String]("instructor")
  object YearQueryParamMatcher       extends OptionalValidatingQueryParamDecoderMatcher[Int]("year")

  def courseRoutes[F[_]: Monad]: HttpRoutes[F] = {
    val dsl = Http4sDsl[F]
    import dsl.*

    HttpRoutes.of[F] {
      case GET -> Root / "courses" :? InstructorQueryParamMatcher(instructor)
          +& YearQueryParamMatcher(optionYear) =>
        val courses = CourseRepository.findCoursesByInstructor(instructor)
        optionYear match {
          case Some(value) =>
            value.fold(
              _ => BadRequest("Parameter 'year' is in valid"),
              year => Ok(courses.filter(_.year == year).asJson)
            )
          case None => Ok(courses.asJson)
        }

      case GET -> Root / "courses" / UUIDVar(courseId) / "students" =>
        CourseRepository.findCoursesById(courseId).map(_.students) match
          case Some(students) =>
            Ok(students.asJson, Header.Raw(CIString("custom-header"), "somevalue"))
          case None => NotFound(s"No course with '$courseId' was found")
    }
  }

  def healthEndpoint[F[_]: Monad]: HttpRoutes[F] = {
    val dsl = Http4sDsl[F]
    import dsl.*
    HttpRoutes.of[F] { case GET -> Root / "health" =>
      Ok("great")
    }
  }

  def allRoutes[F[_]: Monad]: HttpRoutes[F] = courseRoutes[F] <+> healthEndpoint[F]

  def routerWithPathPrefixes: Kleisli[IO, Request[IO], Response[IO]] = Router(
    "/api" -> courseRoutes[IO],
    // "/api" will be a path prefix for the starting point (Root) for all courseRoutes endpoints
    "/private" -> healthEndpoint[IO]
    // "/private" will be a path prefix for the starting point (Root) for all healthEndpoint endpoints
  ).orNotFound

  override def run: IO[Unit] = {
//    EmberServerBuilder
//      .default[IO]
//      .withHttpApp(courseRoutes[IO].orNotFound)
//      .build
//      .use(_ => IO.println("Server ready") *> IO.never)

    // REST endpoints
    // http GET 'localhost:8080/courses?instructor=Isaac%20Netero'
    // http GET 'localhost:8080/courses?instructor=Isaac%20Netero&year=2011'
    // http GET 'localhost:8080/courses?instructor=Isaac%20Netero&year=2005'
    // http GET 'localhost:8080/courses?instructor=Isaac%20Netero&year=abc'
    // http GET 'localhost:8080/courses/be259bdc-3676-4283-862e-40ac72ba0c1a/students'
    // http GET 'localhost:8080/courses/notfound-uuid/students'

    /* ***** */

    EmberServerBuilder
      .default[IO]
      .withHttpApp(routerWithPathPrefixes)
      .build
      .use(_ => IO.println("Server ready") *> IO.never)

    // REST endpoints
    // http GET 'localhost:8080/api/courses?instructor=Isaac%20Netero'
    // http GET 'localhost:8080/api/courses?instructor=Isaac%20Netero&year=2011'
    // http GET 'localhost:8080/api/courses?instructor=Isaac%20Netero&year=2005'
    // http GET 'localhost:8080/api/courses?instructor=Isaac%20Netero&year=abc'
    // http GET 'localhost:8080/api/courses/be259bdc-3676-4283-862e-40ac72ba0c1a/students'
    // http GET 'localhost:8080/api/courses/notfound-uuid/students'
    // http GET 'localhost:8080/private/health'
    // http GET 'localhost:8080/private/something'
  }
}
