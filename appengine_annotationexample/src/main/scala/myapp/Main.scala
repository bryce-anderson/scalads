package myapp

import jaxed.servlet.AnnotationHandler

/**
 * @author Bryce Anderson
 *         Created on 7/13/13
 */
class Main extends AnnotationHandler {
  mapClass[Find]("/find")
  mapClass[Submit]("/submit")
  mapClass[Names]("/names")
  mapClass[Delete]("/delete/:id")
}
