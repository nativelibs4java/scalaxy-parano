package scalaxy.parano

import scala.reflect.api.Universe
import scala.collection.mutable

trait Messages extends SyntaxUtils {
  val global: Universe
  import global._

  case class Message(level: Message.Level, tree: Tree, msg: MessageType)
  object Message {
    sealed trait Level
    case object Info extends Level
    case object ForcedInfo extends Level
    case object Warning extends Level
    case object Error extends Level
  }

  // def info(pos: Position, msg: String, force: Boolean): Unit
  // def error(pos: Position, msg: String): Unit

  type MessagesSink = collection.mutable.Builder[Message, Seq[Message]]
 
  type InfoReporter = (Position, String, Boolean) => Unit
  type WarningReporter = (Position, String) => Unit
  type ErrorReporter = (Position, String) => Unit

  def report(messages: Seq[Message],
             info: InfoReporter,
             warning: WarningReporter,
             error: ErrorReporter) {
    import Message._

    for (Message(level, tree, msg) <- messages; pos = tree.pos) {
      level match {
        case Info | ForcedInfo =>
          info(pos, msg.toString, level == ForcedInfo)

        case Warning =>
          warning(pos, msg.toString)

        case Error =>
          error(pos, msg.toString)
      }
    }
  }

  sealed trait MessageType {
    def message: String
    override def toString = getClass.getName + ": " + message
  }

  case class IdentUsedAtWrongPosition(
    identName: Name,
    positionParam: Symbol)
      extends MessageType
  {
    override def message =
      s"""Confusing name: $identName not used for same-named param but for param ${positionParam.name}"""
  }

  case class IdentMatchingOtherParams(
    identName: Name,
    givenParam: Symbol,
    matchingParams: List[Symbol])
      extends MessageType
  {
    override def message =
      s"""Confusing name: $identName sounds like ${matchingParams.map(_.name).mkString(",")} but used for param ${givenParam.name}"""
  }

  case class UnnamedParamConfusingWithOthersWithSameType(
    param: Symbol,
    otherParams: List[Symbol],
    tpe: Type,
    sym: MethodSymbol)
      extends MessageType
  {
    override def message =
      s"""Unnamed param ${param.name} can be confused with param${if (otherParams.size == 1) "" else "s"} ${otherParams.map(_.name).mkString(", ")} of same type $tpe (method: ${sym.owner.fullName}.${sym.name}"""
  }

  case class ExtractorAliasSoundsLikeOtherField(
    aliasName: Name,
    extractedField: Symbol,
    clashingField: Symbol)
      extends MessageType
  {
    override def message =
      s"Confusing name: $aliasName sounds like ${clashingField.owner.name}.${clashingField.name} but extracts ${extractedField.owner.name}.${extractedField.name}"
  }
}
