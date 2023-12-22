package rop.foundations

// 1
object Cats {

  /*
  Type classes:
    - Applicative
    - Functor
    - FlatMap
    - Monad
    - ApplicativeError/MonadError
   */

  // 1- Functor: mappable structures
  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  import cats.Functor
  import cats.instances.list.*
  val listFunctor: Functor[List] = Functor[List]
  val mappedList: List[Int]      = listFunctor.map(List(1, 2, 3))(_ + 1)
  // functor TC is used for generalizable mappable APIs
  def increment[F[_]](container: F[Int])(using functor: Functor[F]): F[Int] =
    functor.map(container)(_ + 1)

  import cats.syntax.functor.*
  def increment_v2[F[_]](container: F[Int])(using functor: Functor[F]): F[Int] =
    container.map(_ + 1)

  // 2- Applicative: pure method to wrap existing values into "wrapper" values
  trait MyApplicative[F[_]] extends MyFunctor[F] {
    def pure[A](value: A): F[A]
  }

  import cats.Applicative
  val applicativeList: Applicative[List] = Applicative[List]
  val aSimpleList: List[Int]             = applicativeList.pure(42)

  import cats.syntax.applicative.*
  val aSimpleList_v2: List[Int] = 42.pure[List]

  // 3- FlatMap:
  trait MyFlatMap[F[_]] extends Functor[F] {
    def flatMap[A, B](initialValue: F[A])(function: A => F[B]): F[B]
  }

  import cats.FlatMap
  val flatMapList: FlatMap[List]        = FlatMap[List]
  private val flatMappedList: List[Int] = flatMapList.flatMap(List(1, 2, 3))(x => List(x, x + 1))

  // The combination of Functor and FlatMap gives the for-comprehension syntax
  import cats.syntax.flatMap.*
  def crossProduct[F[_]: FlatMap, A, B](containerA: F[A], containerB: F[B]): F[(A, B)] =
    containerA.flatMap(a => containerB.map(b => (a, b)))

  // with for-comprehension
  def crossProduct_v2[F[_]: FlatMap, A, B](containerA: F[A], containerB: F[B]): F[(A, B)] =
    for {
      a <- containerA
      b <- containerB
    } yield (a, b)

  // 4- Monad: Applicative + FlatMap
  trait MyMonad[F[_]] extends Applicative[F] with FlatMap[F] {
    override def map[A, B](fa: F[A])(f: A => B): F[B] =
      flatMap(fa)(a => pure(f(a))) // pure(f(a)) == F[B]
  }

  import cats.Monad
  val monadList: Monad[List] = Monad[List]

  // with for-comprehension
  def monadComputation_v2[F[_]: Monad, A, B](containerA: F[A], containerB: F[B]): F[(A, B)] =
    for {
      a <- containerA
      b <- containerB
    } yield (a, b)

  // 5- ApplicativeError: Computations that can fail
  trait MyApplicativeError[F[_], E] extends Applicative[F] {
    def raiseError[A](error: E): F[A]
  }

  import cats.ApplicativeError
  type ErrorOr[A] = Either[String, A]

  val applicativeEither: ApplicativeError[ErrorOr, String] = ApplicativeError[ErrorOr, String]
  val desiredValue: ErrorOr[Int]                           = applicativeEither.pure(42)
  val failedValue: ErrorOr[Int] = applicativeEither.raiseError("Wrong value")

  import cats.syntax.applicativeError.*
  val failedValue_v2: ErrorOr[Int] = "Wrong value".raiseError

  // 6- MonadError: ApplicativeError + Monad + computations that can fail
  trait MyMonadError[F[_], E] extends ApplicativeError[F, E] with Monad[F]

  import cats.MonadError
  val monadErrorEither: MonadError[ErrorOr, String] = MonadError[ErrorOr, String]

  def main(args: Array[String]): Unit = {}
}
