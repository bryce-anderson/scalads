package myapp

/**
 * @author Bryce Anderson
 *         Created on 7/13/13
 */

import javax.ws.rs.{FormParam, GET, POST}

class Find extends PeopleSearch {

  @GET
  def get() = {
    <html><body>
      <h3>Find Person</h3>
      {personForm("find")}
      <a href="names">Return to Main Menu</a>
    </body></html>
  }

  @POST
  def post(@FormParam("name") name: String, @FormParam("age") age: String) = {
    val nameOption = name match {
      case "" => None
      case s => Some(s)
    }

    val ageOption = if (age == "") None else try Some(age.toInt) catch { case _: NumberFormatException => None }

    val query = ds.query[Person]

    val results = (nameOption, ageOption) match {
      case (Some(name), Some(age)) => query.filter(p => p.name == name && p.age == age)
      case (Some(name), None) => query.filter(p => p.name == name)
      case (None, Some(age)) => query.filter(p => p.age == age)
      case (None, None) => query
    }

    makeNamesHTMLTable(results.getIterator())
  }

}
