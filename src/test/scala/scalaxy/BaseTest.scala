package scalaxy.parano
package test

import org.scalamock.scalatest.MockFactory
import org.scalatest.{ FlatSpecLike, Matchers }

import scala.tools.reflect.ToolBox
import scala.tools.reflect.FrontEnd

import scala.reflect.runtime.currentMirror

trait BaseTest extends FlatSpecLike with Matchers with MockFactory {

  val global = scala.reflect.runtime.universe
  import global._

  private[this] lazy val toolbox = currentMirror.mkToolBox()

  def parse(s: String): Tree =
    toolbox.typecheck(toolbox.parse(s)).asInstanceOf[Tree]
}
