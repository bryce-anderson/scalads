package scalads

/**
 * @author Bryce Anderson
 *         Created on 6/17/13
 */

import scala.annotation.StaticAnnotation

package object annotations {
  case class Rename(name: String) extends StaticAnnotation
}
