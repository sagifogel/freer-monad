package io.github.sagifogel.freermonad.core

trait Monad[M[_]] {
  def pure[A](a: A): M[A]

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => pure(f(a)))
}

object Monad {
  def apply[M[_]](implicit fa: Monad[M]): Monad[M] = fa
}