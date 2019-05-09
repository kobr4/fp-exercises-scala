package example

import scala.annotation.tailrec

object Hello extends Greeting with App {
  println(greeting)
}




trait Greeting {
  lazy val greeting: String = "hello"
}
