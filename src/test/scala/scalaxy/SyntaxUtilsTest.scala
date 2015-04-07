package scalaxy.parano
package test

class SyntaxUtilsTest extends BaseTest with SyntaxUtils {
  import global._

  behavior of "SyntaxUtils"

  it should "detect args that are specified of left to default" in {
    {
      val Block(List(_), f12) = parse("""
        def f(a: Int, b: Int) = ???
        f(1, 2)
      """)
      val Apply(target, List(a, b)) = f12
      isArgSpecified(target, a) should be(true)
      isArgSpecified(target, b) should be(true)
    }

    {
      val Block(List(_, _, f12), f1) = parse("""
        def f(a: Int, b: Int = 0) = ???
        f(1, 2)
        f(1)
      """)

      { val Apply(target, List(a, b)) = f12
        isArgSpecified(target, a) should be(true)
        isArgSpecified(target, b) should be(true) }

      { val Apply(target, List(a, b)) = f1
        isArgSpecified(target, a) should be(true)
        isArgSpecified(target, b) should be(false) }
    }

    {
      val Block(List(_, _, _, f12, f1), f0) = parse("""
        def f(a: Int = 0, b: Int = 0) = ???
        f(1, 2)
        f(1)
        f()
      """)

      { val Apply(target, List(a, b)) = f12
        isArgSpecified(target, a) should be(true)
        isArgSpecified(target, b) should be(true) }

      { val Apply(target, List(a, b)) = f1
        isArgSpecified(target, a) should be(true)
        isArgSpecified(target, b) should be(false) }

      { val Apply(target, List(a, b)) = f0
        isArgSpecified(target, a) should be(false)
        isArgSpecified(target, b) should be(false) }
    }
  }
}
