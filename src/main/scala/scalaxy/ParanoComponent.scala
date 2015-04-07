// Author: Olivier Chafik (http://ochafik.com)
package scalaxy.parano

import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.symtab.Flags

/**
 *  To understand / reproduce this, you should use paulp's :power mode in the scala console:
 *
 *  scala
 *  > :power
 *  > :phase parser // will show us ASTs just after parsing
 *  > val Some(List(ast)) = intp.parse("@scalaxy.extension[Int] def str = self.toString")
 *  > nodeToString(ast)
 *  > val DefDef(mods, name, tparams, vparamss, tpt, rhs) = ast // play with extractors to explore the tree and its properties.
 */
class ParanoComponent(
  val global: Global)
    extends PluginComponent
    with ParanoChecks {
  import global._
  import definitions._

  override val phaseName = ParanoComponent.phaseName

  override val runsAfter = List("typer")
  override val runsBefore = List("patmat")

  def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
    def apply(unit: CompilationUnit) {

      report(check(unit.body), reporter.info, reporter.warning, reporter.error)
    }
  }
}

object ParanoComponent {
  val phaseName = "scalaxy-parano"
}
