package rop.jobsboard.foundations

import cats.{Defer, MonadError}
import cats.effect.kernel.{Fiber, Spawn}
import cats.effect.{Concurrent, Deferred, GenSpawn, IO, IOApp, MonadCancel, Ref, Resource, Sync, Temporal}

import java.io.{File, FileWriter, PrintWriter}
import scala.concurrent.ExecutionContext
import scala.io.{BufferedSource, Source, StdIn}
import scala.concurrent.duration.*
import scala.util.Random

// 2
object CatsEffect extends IOApp.Simple {

  /*
     - Describing computations as values in purely functional way
   */
//  - IO: data structure that describes arbitrary computations, including those performing side effects
  val firstIO: IO[Int] = IO.pure(42)
  val delayedIO: IO[Int] = IO { // or 'IO.apply'
    // '.apply' take an 'expression by name' that won't be evaluated until the IO itself is being force evaluated at the end of the app
    println("This is a side effect") // this won't be printed until the IO is being evaluated
    42
  }

  def evaluateIO[A](io: IO[A]): Unit = {
    import cats.effect.unsafe.implicits.global // platform to evaluate IOs
    val value = io.unsafeRunSync()
    println(s"The result of the effect is '$value'")
  }

  // Transformations:
  // map + flatMap
  val improvedValue: IO[Int] = firstIO.map(_ * 2)
  val printedValue: IO[Unit] = firstIO.flatMap(value => IO(println(value)))

  // for-comprehensions
  def smallProgram(): IO[Unit] = for {
    line1 <- IO(StdIn.readLine())
    line2 <- IO(StdIn.readLine())
    _     <- IO(println(line1 + line2))
  } yield ()

  // Using '.unsafeRunSync()' is not the ideal way to evaluate an IO, which is why apps in Cats effect extend a trait
  // called 'IOApp' (a data type from Cats effect) or we can use 'IOApp.Simple', which enforce using a new main method
  // called 'run'. This method will evaluate IOs internally, so we don't need to call '.unsafeRunSync()' explicitly

//  The old style of standard of Scala apps
//  def main(args: Array[String]): Unit = {
//    evaluateIO(delayedIO)
//    evaluateIO(smallProgram())
//  }

  // IOs can raise/"catch" errors
  val failure: IO[Int] = IO.raiseError(new RuntimeException("Error occurred"))
  private val handledFailure = failure.handleErrorWith { case _: RuntimeException =>
    IO(println("Recovered from error"))
  }

  // Fibers: lightweight threads. An internal scheduler that will schedule few OS (native) threads on many of these fibers
  val delayedPrint: IO[Unit] =
    IO.sleep(1.second) *> IO(println(Random.nextInt(100))) // '*>' is equivalent to 'flatMap'
  val manyPrints: IO[Unit] = for {
    _ <- delayedPrint
    _ <- delayedPrint
  } yield ()
  // We can run these delayed prints in parallel using fibers:
  val manyPrintsWithFibers: IO[Unit] = for {
    fib1 <- delayedPrint.start
    fib2 <- delayedPrint.start
    // these 2 previous expressions will be run in parallel
    _ <- fib1.join // joining by blocking the calling fiber until fib1 joins
    _ <- fib2.join // joining by blocking the calling fiber until fib2 join
  } yield ()

  // Cancelling fibers
  val cancelledFiber: IO[Unit] = for {
    fib <- delayedPrint.onCancel(IO(println("This fiber is cancelled"))).start
    _   <- IO.sleep(500.millis) *> IO(println("cancelling fiber...")) *> fib.cancel
    _   <- fib.join
  } yield ()

  // Uncancelling fibers
  val ignoredCancellation: IO[Unit] = for {
    fib <- IO.uncancelable(_ => delayedPrint.onCancel(IO(println("This fiber is cancelled")))).start
    _   <- IO.sleep(500.millis) *> IO(println("cancelling fiber...")) *> fib.cancel
    // cancellation will be ignored since this fiber is uncancellable
    _ <- fib.join
  } yield ()

  // Resources:
  val readingResource: Resource[IO, BufferedSource] = Resource.make(
    IO(Source.fromFile("src/main/scala/rop/jobsboard/foundations/Cats.scala"))
  )(source => IO(println("closing source")) *> IO(source.close()))

  val readingEffect: IO[Unit] =
    readingResource.use(source => IO(source.getLines().foreach(println)))

    // Composing resources:
  val copiedFileResource: Resource[IO, PrintWriter] = Resource.make(
    IO(new PrintWriter(new FileWriter(new File("src/main/resources/dumpedFile.scala"))))
  )(writer => IO(println("closing duplicated file")) *> IO(writer.close()))

  val compositeResource: Resource[IO, (BufferedSource, PrintWriter)] = for {
    source      <- readingResource
    destination <- copiedFileResource
  } yield (source, destination)

  val copyFileEffect: IO[Unit] = compositeResource.use { case (source, destination) =>
    IO(source.getLines().foreach(destination.println))
  }

  // Abstract kinds of computations
  // MonadCancel is for cancelable computations, regarding if those computations end up executing on fibers or on
  // separate threads, or directly in the calling thread.. that doesn't matter as MonadCancel will describe cancelable
  // computations
  trait MyMonadCancel[F[_], E] extends MonadError[F, E] {

    // Marking certain code pieces as cancellable and some not to be cancellable
    trait CancellationFlagResetter {
      def apply[A](fa: F[A]): F[A] // with the cancellation flag reset
    }
    def canceled: F[Unit]
    def uncancelable[A](poll: CancellationFlagResetter => F[A]): F[A]
  }

  // MonadCancel for IO
  val monadCancelIO: MonadCancel[IO, Throwable] = MonadCancel[IO]
  val uncancellableIO: IO[Int]                  = monadCancelIO.uncancelable(_ => IO(42))
  // same as IO.uncancelable(...)

  // Spawn: the ability to create fibers
  trait MyGenSpawn[F[_], E] extends MyMonadCancel[F, E] {
    def start[A](fa: F[A]): Fiber[F, E, A] // creates a fiber
    // never, cede, racePair
  }

  trait MySpawn[F[_]] extends GenSpawn[F, Throwable]

  private val spawnIO = Spawn[IO] // this will fetch any implicit instance in scope for Spawn IO
  private val fiber =
    spawnIO.start(delayedPrint) // creates a fiber .. it's the same as: 'delayedPrint.start'

  // Concurrent: creates 2 fundamental concurrency primitives: 'Atomic References' and 'Promises'
  trait MyConcurrent[F[_]] extends Spawn[F] {
    def ref[A](a: A): F[Ref[F, A]]     // for 'atomic references'
    def deferred[A]: F[Deferred[F, A]] // for 'promises'
  }

  // Temporal: the ability to suspend computations for a given time
  trait MyTemporal[F[_]] extends Concurrent[F] {
    def sleep(time: FiniteDuration): F[Unit]
  }

  // Sync: the ability to suspend synchronous arbitrary expressions or computations in an effect
  trait MySync[F[_]] extends MonadCancel[F, Throwable] with Defer[F] {
    def delay[A](expression: => A): F[A] // 'by name' (: => A) lazy expression
    def blocking[A](expression: => A): F[A]
    // same expression as 'delay', but it runs on a dedicated thread pool
  }

  // Async: the ability to suspend asynchronous computations (running on other thread pools, for example an execution
  // context that you define outside of Cats effect) into an effect managed by Cats Effect
  trait MyAsync[F[_]] extends Sync[F] with Temporal[F]{
    def executionContext: F[ExecutionContext]
    def async[A](callBack: (Either[Throwable, A] => Unit) => F[Option[F[Unit]]]): F[A]
  }

  override def run: IO[Unit] = {
//    smallProgram()
//    manyPrints
//    manyPrintsWithFibers
//    cancelledFiber
//    ignoredCancellation
//    readingEffect
    copyFileEffect
  }
}
