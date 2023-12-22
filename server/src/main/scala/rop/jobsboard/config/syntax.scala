package rop.jobsboard.config

import cats.MonadThrow
import cats.implicits.*
import pureconfig.error.ConfigReaderException
import pureconfig.{ConfigReader, ConfigSource}

import scala.reflect.ClassTag

object syntax {
  extension (source: ConfigSource)
    def loadF[F[_], A](using reader: ConfigReader[A], F: MonadThrow[F], tag: ClassTag[A]): F[A] =
      F.pure(source.load[A]) // F[Either[Errors, A]]
        .flatMap {
          case Right(value) => F.pure(value)
          case Left(errors) => F.raiseError[A](ConfigReaderException(errors))
        }
}
