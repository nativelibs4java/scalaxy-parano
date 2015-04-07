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

  it should "detect specified args (as opposed to default ones)" in {
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

  object ArgsNamed {
    def unapplySeq(tree: Apply) = Option(tree) collect {
      case Apply(_, args) => args.map(isNamedParam)
    }
  }

  it should "detect named params" in {
    {
      val Block(List(_, f12), fa12) = parse("""
        def f(a: Int, b: Int) = ???
        f(1, 2)
        f(a = 1, 2)
      """)
      val ArgsNamed(Some(false), Some(false)) = f12
      val ArgsNamed(Some(true), Some(false)) = fa12
    }

    {
      val Block(List(_, _, f12), f1) = parse("""
        def f(a: Int, b: Int = 0) = ???
        f(1, b = 2)
        f(a = 1)
      """)
      val ArgsNamed(Some(false), Some(true)) = f12
      val ArgsNamed(Some(true), Some(false)) = f1
    }
  }
}
