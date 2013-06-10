package scalads

import sun.font.TrueTypeFont

/**
 * @author Bryce Anderson
 *         Created on 6/9/13
 */
package object core {

  type JoinOp = JoinOperation.JoinOperation
  type FilterOp = Operation.Operation
  type SortDir = SortDirection.SortDirection

  case class Projection(path: List[String], clazz: Option[Class[_]] = None) { self =>
    override def equals(other: Any) = other match {
      case Projection(path, clazz) if path == self.path && clazz.equals(self.clazz) => true
      case _ => false
    }
  }

  object SortDirection extends Enumeration {
    type SortDirection = Value
    val asc, desc = Value
  }

  object Operation extends Enumeration {
    type Operation = Value
    val gt, lt, ge, le, eq, ne = Value
  }

  object JoinOperation extends Enumeration {
    type JoinOperation = Value
    val and, or = Value
  }
}
