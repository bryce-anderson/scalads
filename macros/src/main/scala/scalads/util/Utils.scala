package scalads.util

/**
 * @author Bryce Anderson
 *         Created on 5/26/13
 */
object Utils {

  // Traditional identity doesn't work because the type T is difficult to define in macros
  final def optIdent[T](in: Option[T]) = in

}
