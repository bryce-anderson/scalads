package myapp

import javax.ws.rs.{FormParam, POST, GET}

import jaxed.servlet.Redirect

/**
 * @author Bryce Anderson
 *         Created on 7/13/13
 */
class Submit extends PeopleSearch {

  @GET
  def get() = {
    <html>
      <body>
        <h3>Person to Submit</h3>
        {personForm("submit")}
        <a href="names">Return to Main Menu</a>
      </body>
    </html>
  }

  @POST
  def post(@FormParam("name") name: String, @FormParam("age") age: Int) = {
    val person = Person(name, age)
    ds.put(person)

    "redirect to /names"
    Redirect("/names")
  }

}
