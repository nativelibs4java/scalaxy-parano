package scalaxy.parano
package test

import org.junit._
import org.junit.Assert._

class ParanoTest {
  // TODO: embed parser and do proper tests.
  // Uncomment to test
  // scalaxy.parano.verify()

  case class Foo(a: Int, b: Foo)
  case class Foo2(a: Int)(b: Foo)
  case class Bar(a: Int, b: Int)

  case class StreamOutput(
    a: List[String] = Nil,
    b: List[String] = Nil,
    c: List[String] = Nil)

  val f = Foo(1, Foo(2, null))
  val f2 = Foo2(1)(Foo(2, null))

  Bar(1, 2)
  Bar(a = 1, 2)
  Bar(1, b = 2)
  Bar(a = 1, b = 2)

  StreamOutput(a = List(""))

  @Test
  def simple {
    {
      val Foo(b, a) = f
      val Foo2(b2: Int) = f2
    }

    {
      val Foo(b, aaa @ Foo(bb, aa)) = f
    }
  }
  @Test
  def longerNames {
    {
      case class Bar(firstField: Int, secondField: Int, third: Int, fourth: Int)

      val b = Bar(1, 2, 3, 4)
      val Bar(theFourth, first, theThird, theSecond) = b
    }
  }
  @Test
  def ambiguousParams {
    case class Bar(a: Int, b: Int)
    val a = 10
    val b = 12
    Bar(b, a)
    Bar(a = 1, b = 2)
    Bar(a = 1, 2)
    Bar(1, 2)
  }
}
