package scalaxy.parano
package test

class SyntaxUtilsTest extends BaseTest with SyntaxUtils {
  import global._

  behavior of "isArgSpecified"

  def areArgsSpecified(tree: Apply): List[Boolean] =
    tree.args.map(isArgSpecified(tree.fun, _))

  it should "detect args specified to simple method" in {
    val Block(List(_), f12: Apply) = parse("""
      def f(a: Int, b: Int) = ???
      f(1, 2)
    """)
    val List(true, true) = areArgsSpecified(f12)
  }

  it should "detect args specified to method with some default params" in {
    val Block(List(_, _, f12: Apply), f1: Apply) = parse("""
      def f(a: Int, b: Int = 0) = ???
      f(1, 2)
      f(1)
    """)
    val List(true, true) = areArgsSpecified(f12)
    val List(true, false) = areArgsSpecified(f1)
  }

  it should "detect args specified to method with only default params" in {
    val Block(List(_, _, _, f12: Apply, f1: Apply), f0: Apply) = parse("""
      def f(a: Int = 0, b: Int = 0) = ???
      f(1, 2)
      f(1)
      f()
    """)
    val List(true, true) = areArgsSpecified(f12)
    val List(true, false) = areArgsSpecified(f1)
    val List(false, false) = areArgsSpecified(f0)
  }

  behavior of "isNamedParam"

  def areParamsNamed(tree: Apply): List[Option[Boolean]] =
    tree.args.map(isNamedParam)

  it should "detect named params given to simple method" in {
    val Block(List(_, f12: Apply), fa12: Apply) = parse("""
      def f(a: Int, b: Int) = ???
      f(1, 2)
      f(a = 1, 2)
    """)
    val List(Some(false), Some(false)) = areParamsNamed(f12)
    val List(Some(true), Some(false)) = areParamsNamed(fa12)
  }
  it should "detect named params given to method with some default params" in {
    val Block(List(_, _, f12: Apply), f1: Apply) = parse("""
      def f(a: Int, b: Int = 0) = ???
      f(1, b = 2)
      f(a = 1)
    """)
    val List(Some(false), Some(true)) = areParamsNamed(f12)
    val List(Some(true), Some(false)) = areParamsNamed(f1)
  }
}
