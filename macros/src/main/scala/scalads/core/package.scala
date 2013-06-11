package scalads

import sun.font.TrueTypeFont

/**
 * @author Bryce Anderson
 *         Created on 6/9/13
 */
package object core {

  case class Projection(path: List[String])

  sealed trait SortDirection
  object SortDirection {
    case object ASC extends SortDirection
    case object DSC extends SortDirection
  }

  sealed trait Operation
  object Operation {
    case object GT extends Operation
    case object LT extends Operation
    case object GE extends Operation
    case object LE extends Operation
    case object EQ extends Operation
    case object NE extends Operation
  }

  sealed trait JoinOperation
  object JoinOperation {
    case object AND extends JoinOperation
    case object OR extends JoinOperation
  }
}
