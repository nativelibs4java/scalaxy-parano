package scalaxy.parano
package test

class ParanoChecksTest extends BaseTest with ParanoChecks {
  import global._
  import Message._

  behavior of "ParanoChecks"

  it should "detect ambiguous case class construction" in {
  
    val List(Message(Error, q"1", _)) = check(parse("""
      case class Foo(bar: Int, baz: Int)
      Foo(1, 2)
    """))

    val List() = check(parse("""
      case class Foo(bar: Int, baz: Int)
      Foo(bar = 1, 2)
      Foo(1, baz = 2)
      Foo(bar = 1, baz = 2)
    """))
  }

  ignore should "detect possible argument swaps" in {
    
    // TODO error here: second warning is... unexpected
    val List(Message(Error, q"baz", _), Message(Error, q"baz", _)) = check(parse("""
      def f(bar: Int, baz: Int) = ???
      val baz = 10
      f(baz, 2)
    """))
  }
}
