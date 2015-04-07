package scalaxy.parano
package test

import scala.collection.mutable.ListBuffer

import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox
import scala.tools.reflect.FrontEnd

case class CompilerMessages(
  infos: List[String] = Nil,
  warnings: List[String] = Nil,
  errors: List[String] = Nil)

object ParanoTestUtils {

  private[this] class TestToolbox(options: String) {

    val infosBuilder, warningsBuilder, errorsBuilder =
      ListBuffer[String]()

    def reset() = {
      infosBuilder.clear()
      warningsBuilder.clear()
      errorsBuilder.clear()
      this
    }
    
    private[this] val frontEnd = new FrontEnd {
      override def display(info: Info) {
        val builder: ListBuffer[String] = info.severity match {
          case INFO => infosBuilder
          case WARNING => warningsBuilder
          case ERROR => errorsBuilder
        }

        builder += info.msg
      }
      override def interactive() {}
    }

    val toolbox = currentMirror.mkToolBox(frontEnd, options)
  }

  private[this] def threadLocal[A](a: => A) = new ThreadLocal[A] {
    override def initialValue() = a
  }

  private[this] val localTestToolbox =
    threadLocal(new TestToolbox("-Ystop-after:" + ParanoComponent.phaseName))

  private[this] def compile(source: String, opt: Boolean = false): (() => Any, CompilerMessages) = {
    
    val testToolbox = localTestToolbox.get.reset()
    import testToolbox.toolbox
    import toolbox.u._

    try {
      val tree = toolbox.parse(source);
      val compilation = toolbox.compile(tree)

      (
        compilation,
        CompilerMessages(
          infos = testToolbox.infosBuilder.result,
          warnings = testToolbox.warningsBuilder.result,
          errors = testToolbox.errorsBuilder.result)
      )
    } catch { case ex: Throwable =>
      throw new RuntimeException(s"Failed to compile:\n$source", ex)
    }
  }
}
