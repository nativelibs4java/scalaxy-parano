package scalaxy.parano

import scala.reflect.api.Universe
import scala.collection.mutable

/**
 * Perform extra compilation-time checks in compilation units where scalaxy.parano.verify() is called.
 * Checks:
 * - Confusing names in case class extractors
 * - Ambiguous unnamed arguments with same type
 * - Confusing names in method calls
 * - (TODO) Potential side-effect free statements (e.g. missing + between multiline concatenations)
 */
trait ParanoChecks extends SyntaxUtils with Messages {
  val global: Universe
  import global._

  import ParanoChecks._

  /** getFragments("this_isTheEnd") == Array("this", "is", "The", "End") */
  private def getFragments(name: String) =
    name.split("""\b|[_-]|(?<=[a-z])(?=[A-Z])""").filter(_.length >= 3)

  private case class RichName(name: String) {
    val fragments = getFragments(name).toSet

    def containsExactFragment(str: String): Boolean = {
      name.contains(str) ||
        fragments.exists(_.equalsIgnoreCase(str)) ||
        getFragments(str).exists(frag => fragments.exists(_.equalsIgnoreCase(frag)))
    }
  }

  private val methodParams = new mutable.HashMap[MethodSymbol, List[List[RichName]]]
  private def getMethodParams(sym: MethodSymbol): List[List[RichName]] = {
    methodParams synchronized {
      methodParams.getOrElseUpdate(sym, {
        // Case class extractors only care about the first params group.
        sym.paramLists.map(_.map(param => RichName(param.name.toString)))
      })
    }
  }
  private val methodParamTypeGroups = new mutable.HashMap[MethodSymbol, Map[Int, List[(String, Int)]]]
  private def getMethodParamTypeGroups(sym: MethodSymbol): Map[Int, List[(String, Int)]] = {
    methodParamTypeGroups synchronized {
      methodParamTypeGroups.getOrElseUpdate(sym, {
        // Case class extractors only care about the first params group.
        for (
          (tpe, params) <- sym.paramLists.flatten.zipWithIndex.groupBy(_._1.typeSignature);
          if params.size > 1
        ) yield {
          val paramNames = params.map { case (param, i) => param.name.toString -> i }
          for ((param, i) <- params) yield {
            i -> paramNames
          }
        }
      }.flatten.toMap)
    }
  }
  private val caseClassesFields = new mutable.HashMap[ClassSymbol, List[RichName]]
  private def getCaseClassFields(sym: ClassSymbol): List[RichName] = {
    caseClassesFields synchronized {
      caseClassesFields.getOrElseUpdate(sym, {
        val csym = sym.asClass
        val ctor = csym.typeSignature.member(termNames.CONSTRUCTOR).asMethod
        // Case class extractors only care about the first params group.
        ctor.paramLists.head.map(param => RichName(param.name.toString))
      })
    }
  }

  def check(tree: Tree): Seq[Message] = {
    val messages = Seq.newBuilder[Message]

    new Traverser {
      override def traverse(tree: Tree) {
        tree match {
          case DefDef(mods, _, _, _, _, _) if isSynthetic(mods) =>
            // Don't check synthetic methods
            // (created by the compiler itself, so better be good already)

          case tree @ Apply(target, _) if target.symbol.isMethod =>
            checkMethodApply(tree, messages)
            super.traverse(tree)

          case tree @ Match(_, _) =>
            checkMatch(tree, messages)
            super.traverse(tree)

          case _ =>
            super.traverse(tree)
        }
      }
    } traverse tree

    messages.result
  }

  def isWhitelistedMethod(sym: MethodSymbol): Boolean = {
    whitelistedMethods((sym.owner.fullName, sym.name.toString))
  }

  def checkMethodApply(tree: Apply, messages: MessagesSink) {
    val Apply(target, args) = tree
    val sym = target.symbol.asMethod

    if (!isWhitelistedMethod(sym)) {
      val groups = getMethodParamTypeGroups(sym)
      val params = getMethodParams(sym).flatten

      val decisiveIdents = args.zip(params).map({
        case (arg, param) if isArgSpecified(target, arg) =>
          arg match {
            case Ident(name) =>
              val n = name.toString
              val matches = params.filter(_.containsExactFragment(n))
              if (matches.contains(param)) {
                // Decisive if not matched by any other.
                matches.size == 1
              } else {
                // Not decisive.
                if (!matches.isEmpty) {
                  if (params.exists(_.name == n)) {
                    messages += Message(Message.Error, arg,
                      s"""Confusing name: $n not used for same-named param but for param ${param.name}""")
                  } else {
                    messages += Message(Message.Error, arg,
                      s"""Confusing name: $n sounds like ${matches.map(_.name).mkString(",")} but used for param ${param.name}""")
                  }
                }
                false
              }
            case _ =>
              false
          }
        case _ =>
          true
      })

      // println(s"""
      //   decisiveIdents = $decisiveIdents
      // """)
      for {
        (arg, i) <- args.zipWithIndex
        named <- isNamedParam(arg)
        if !named && isArgSpecified(target, arg)
      } {
        for {
          group <- groups.get(i)
          if i != group.last._2 /* Report in all per group */
        } {
          val (namedParams, unnamedParams) = group.partition({
            case (name, index) =>
              // TODO add unequivocal ident name check here.
              index < args.size && (
                isNamedParam(args(index)) == Some(true) ||
                decisiveIdents(index)
              )
          })
          if (namedParams.size < group.size - 1) {
            val param = sym.paramLists.flatten.apply(i)
            val paramName = param.name.toString
            val others = unnamedParams.map(_._1).filter(_ != paramName)
            messages += Message(Message.Error, arg,
              s"""Unnamed param $paramName can be confused with param${if (others.size == 1) "" else "s"} ${others.mkString(", ")} of same type ${param.typeSignature} (method: ${sym.owner.fullName}.${sym.name}""")
          }
        }
      }
    }
  }

  def checkMatch(tree: Match, messages: MessagesSink) {
    val Match(selector, cases) = tree
    val sym = selector.tpe.typeSymbol

    if (sym.isClass && sym.asClass.isCaseClass) {
      val fields = getCaseClassFields(sym.asClass)
      for (CaseDef(pat, guard, body) <- cases) {
        pat match {
          case Apply(target, args) =>
            for ((arg, field) <- args.zip(fields)) {
              arg match {
                case Bind(argName, _) if !field.containsExactFragment(argName.toString) =>
                  val an = argName.toString
                  for (clashingField <- fields.find(_.containsExactFragment(an))) {
                    messages += Message(Message.Error, arg,
                      s"Confusing name: $an sounds like ${sym.name}.${clashingField.name} but extracts ${sym.name}.${field.name}")
                  }
                case _ =>
              }
            }
          case _ =>
        }
      }
    }
  }
}

object ParanoChecks {
  // TODO(ochafik): Add flag.
  val whitelistedMethods = Set(
    ("java.lang.String", "replace")
  )
}
