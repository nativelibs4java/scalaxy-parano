package scalaxy.parano

import scala.reflect.api.Universe
import scala.collection.mutable

trait Messages extends SyntaxUtils {
  val global: Universe
  import global._

  case class Message(level: Message.Level, tree: Tree, msg: String)
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
        case Info | ForcedInfo => info(pos, msg, level == ForcedInfo)
        case Warning => warning(pos, msg)
        case Error => error(pos, msg)
      }
    }
  }
}
