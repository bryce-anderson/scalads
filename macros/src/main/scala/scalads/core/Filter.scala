package scalads.core

/**
 * @author Bryce Anderson
 *         Created on 6/9/13
 */


sealed trait Filter

case class SingleFilter(axis: Projection, op: Operation.Operation, value: Any) extends Filter

case class CompositeFilter(f1: Filter, f2: Filter, op: JoinOp) extends Filter
