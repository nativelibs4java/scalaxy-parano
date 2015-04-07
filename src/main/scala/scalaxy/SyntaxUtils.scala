package scalaxy.parano

import scala.reflect.api.Universe
import scala.collection.mutable

trait SyntaxUtils {
  val global: Universe
  import global._

  def isArgSpecified(applicationTarget: Tree, arg: Tree): Boolean = {
    // println(s"""
    //   arg = $arg
    //   arg.pos = ${arg.pos}
    //   applicationTarget.pos = ${applicationTarget.pos}
    // """)
    arg.pos != NoPosition && arg.pos != applicationTarget.pos
  }

  // def isSynthetic(mods: Modifiers): Boolean
  def isSynthetic(mods: Modifiers): Boolean =
    mods.hasFlag(Flag.SYNTHETIC)

  /**
   * Figure whether a parameter value is given "by name".
   * There's no clean official way to do it, so the hack here is to do some simple back-parsing
   * of the source before the value's position to skip spaces and comments until a = (or any
   * other meaningful char) is met.
   */
  def isNamedParam(value: Tree): Option[Boolean] = {
    val pos = value.pos
    if (pos == NoPosition) None
    else {
      val src = pos.source.content
      var i = pos.point - 1
      var done = false
      var inComments = false
      while (i >= 0 && !done) {
        val c = src(i)
        if (inComments) {
          if (c == '*' && i > 0 && src(i - 1) == '/') {
            i -= 2
            inComments = false
          } else {
            i -= 1
          }
        } else {
          c match {
            case '/' if i > 0 && src(i - 1) == '*' =>
              i -= 2
              inComments = true
            case ' ' | '\t' | '\n' | '\r' =>
              i -= 1
            case _ =>
              done = true
          }
        }
      }
      Some(src(i) == '=')
    }
  }
}
