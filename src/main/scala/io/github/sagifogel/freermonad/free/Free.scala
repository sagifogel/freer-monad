package io.github.sagifogel.freermonad.free

import io.github.sagifogel.freermonad.core.{Monad, ~>}

trait Free[M[_], A] {

  import Free._

  def map[B](f: A => B): Free[M, B] = flatMap(a => pure(f(a)))

  def flatMap[B](f: A => Free[M, B]): Free[M, B] = FlatMap(this, f)

  def foldMap[G[_] : Monad](nat: M ~> G): G[A] = this match {
    case Pure(a) => Monad[G].pure(a)
    case FlatMap(fa, f) =>
      Monad[G].flatMap(fa.foldMap(nat))(a => f(a).foldMap(nat))
    case Suspend(ma) => nat(ma)
  }
}

object Free {
  def pure[M[_], A](a: A): Free[M, A] = Pure(a)

  def liftM[M[_], A](ma: M[A]): Free[M, A] = Suspend(ma)

  case class Pure[M[_], A](a: A) extends Free[M, A]

  case class FlatMap[M[_], A, B](fa: Free[M, A], f: A => Free[M, B]) extends Free[M, B]

  case class Suspend[M[_], A](ma: M[A]) extends Free[M, A]
}