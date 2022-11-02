package io.github.sagifogel.freermonad.core

final case class IO[A](unsafeRun: () => A)

object IO extends IOInstances {
  def delay[A](a: => A): IO[A] = IO(() => a)
}

abstract class IOInstances {
  implicit def ioMonad: Monad[IO] = new Monad[IO] {
    override def pure[A](a: A): IO[A] = IO.delay[A](a)

    override def flatMap[A, B](ma: IO[A])(f: A => IO[B]): IO[B] =
      IO.delay[B](f(ma.unsafeRun()).unsafeRun())
  }
}