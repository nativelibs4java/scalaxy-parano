package scalaxy.parano
package test

import org.scalamock.scalatest.MockFactory
import org.scalatest.{ FlatSpecLike, Matchers }

import scala.tools.reflect.ToolBox
import scala.tools.reflect.FrontEnd

trait BaseTest extends FlatSpecLike with Matchers with MockFactory {

  val global = scala.reflect.runtime.universe
  import global._

  import scala.reflect.runtime.currentMirror

  private[this] lazy val toolbox = currentMirror.mkToolBox()

  def parse(s: String): Tree = 
    toolbox.typecheck(toolbox.parse(s)).asInstanceOf[Tree]
}
