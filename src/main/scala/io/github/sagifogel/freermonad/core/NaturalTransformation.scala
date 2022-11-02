package io.github.sagifogel.freermonad.core

trait ~>[F[_], G[_]] {
  def apply[A](fa: F[A]): G[A]
}