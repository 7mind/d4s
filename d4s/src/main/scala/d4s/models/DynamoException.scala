package d4s.models

import scala.annotation.tailrec

abstract class DynamoException(val message: String, val cause: Throwable) extends RuntimeException(message, cause)

object DynamoException {
  @tailrec def unapply(arg: DynamoException): Option[(String, Throwable)] = shallow.unapply(arg) match {
    case Some((_, inner: DynamoException)) => unapply(inner)
    case None                              => None
    case res                               => res
  }

  object shallow {
    def unapply(arg: DynamoException): Option[(String, Throwable)] = Some((arg.message, arg.cause))
  }

  final case class InterpreterException(operation: String, tableName: Option[String], override val cause: Throwable)
    extends DynamoException(s"Got error during executing `$operation` for table `$tableName`. Cause: ${cause.getMessage}", cause)

  final case class QueryException private (override val message: String, override val cause: Throwable) extends DynamoException(message, cause)
  object QueryException {
    def apply(cause: Throwable): QueryException                    = from(None, cause)
    def apply(queryName: String, cause: Throwable): QueryException = from(Some(queryName), cause)
    private def from(queryName: Option[String], cause: Throwable) = {
      val mbName = queryName.fold(" ")(n => s" `$n` ")
      cause match {
        case DynamoException(message, cause) => new QueryException(s"Dynamo query${mbName}failed due to error: $message", cause)
        case cause                           => new QueryException(s"Dynamo query${mbName}failed due to error: ${cause.getMessage}", cause)
      }
    }
  }

  final case class DecoderException(override val message: String, maybeCause: Option[Throwable]) extends DynamoException(message, maybeCause.orNull) {
    def union(that: DecoderException): DecoderException = {
      val errorLog = message + "\n" + that.getMessage
      DecoderException(errorLog, maybeCause.orElse(that.maybeCause))
    }
  }
}
