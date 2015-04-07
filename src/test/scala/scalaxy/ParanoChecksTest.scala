package scalaxy.parano
package test

class ParanoChecksTest extends BaseTest with ParanoChecks {
  import global._
  import Message._

  behavior of "ParanoChecks"

/*
_: IdentUsedAtWrongPosition
_: IdentMatchingOtherParams
_: UnnamedParamConfusingWithOthersWithSameType
_: ExtractorAliasSoundsLikeOtherField
*/
  it should "error on ambiguous params with same type" in {
    val List(msg) = check(parse("""
      case class Foo(bar: Int, baz: Int)
      Foo(1, 2)
    """))
    val Message(Error, q"1", _: UnnamedParamConfusingWithOthersWithSameType) = msg
  }

  it should "accept named params with same type" in {
    val List() = check(parse("""
      case class Foo(bar: Int, baz: Int)
      Foo(bar = 1, 2)
      Foo(1, baz = 2)
      Foo(bar = 1, baz = 2)
    """))
  }

  it should "detect possible argument swaps" in {
    // TODO error here: second warning is... unexpected
    val List(msg1, msg2) = check(parse("""
      def f(bar: Int, baz: Int) = ???
      val baz = 10
      f(baz, 2)
    """))
    val Message(Error, q"baz", _: IdentUsedAtWrongPosition) = msg1
    val Message(Error, q"baz", _: UnnamedParamConfusingWithOthersWithSameType) = msg2
  }
}
