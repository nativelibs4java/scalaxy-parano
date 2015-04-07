package scalaxy.parano
package test

class SyntaxUtilsTest extends BaseTest with SyntaxUtils {
  import global._

  behavior of "SyntaxUtils"

  object ArgsSpecified {
    def unapplySeq(tree: Apply) = Option(tree) collect {
      case Apply(target, args) => args.map(isArgSpecified(target, _))
    }
  }

  it should "detect args that are specified of left to default" in {
    {
      val Block(List(_), f12) = parse("""
        def f(a: Int, b: Int) = ???
        f(1, 2)
      """)
      val ArgsSpecified(true, true) = f12
    }

    {
      val Block(List(_, _, f12), f1) = parse("""
        def f(a: Int, b: Int = 0) = ???
        f(1, 2)
        f(1)
      """)
      val ArgsSpecified(true, true) = f12
      val ArgsSpecified(true, false) = f1
    }

    {
      val Block(List(_, _, _, f12, f1), f0) = parse("""
        def f(a: Int = 0, b: Int = 0) = ???
        f(1, 2)
        f(1)
        f()
      """)
      val ArgsSpecified(true, true) = f12
      val ArgsSpecified(true, false) = f1
      val ArgsSpecified(false, false) = f0
    }
  }
}
