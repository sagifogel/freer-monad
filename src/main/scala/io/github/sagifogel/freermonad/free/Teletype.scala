package io.github.sagifogel.freermonad.free

sealed trait Teletype[_] extends Product with Serializable

final case class WriteLine(line: String) extends Teletype[Unit]

final case class ReadLine(prompt: String) extends Teletype[String]

final case class Log(line: String) extends Teletype[Unit]

object Teletype {
  type FreeTeletype[A] = Free[Teletype, A]

  final def writeLine(line: String): FreeTeletype[Unit] =
    Free.liftM[Teletype, Unit](WriteLine(line))

  final def readLine(prompt: String): FreeTeletype[String] =
    Free.liftM[Teletype, String](ReadLine(prompt))

  final def log(line: String): FreeTeletype[Unit] =
    Free.liftM[Teletype, Unit](Log(line))
}