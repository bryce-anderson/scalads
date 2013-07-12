package myapp

import org.scalatra.ScalatraServlet
import com.google.appengine.api.datastore.Entity

import scala.xml.NodeBuffer
import scalads.core.EntityBacker

/**
 * @author Bryce Anderson
 *         Created on 6/30/13
 */

class Main extends ScalatraServlet with DataConnection {

  private def makeNamesHTMLTable(iterator: Iterator[Person with EntityBacker[Person, Entity]]) = {
    val respBuffer = new NodeBuffer

    iterator.foreach{p =>
      val deladdress = "delete/" + p.ds_idString()

      respBuffer += <tr>
        <td>{p.name}</td>
        <td>{p.age}</td>
        <td>
          <form name="input" action={deladdress} method="get"><button>Delete</button></form>
        </td>
      </tr>
    }

    <html><body>
      <h2>List of People.</h2>
      <a href="names">All People</a><br/>
      <a href="submit">Submit new person.</a><br/>
      <a href="find">Find People.</a>
      <table>
        {respBuffer.result()}
      </table>
    </body></html>
  }

  def personForm(action: String) =
    <form name="input" action={action} method="post">
      <table>
        <tr>
          <td>Name:</td>
          <td><input type="text" name="name"/></td>
        </tr>
        <tr>
          <td>Age:</td>
          <td><input type="number" name="age" /></td>
        </tr>
      </table>
      <button>Submit</button>
    </form>

  // Now the actual routes

  get("/") {
    redirect("/names")
  }

  get("/future") {
    <html><body>This is NOT a future.</body></html>
  }


  get("/names") {
    val people = ds.query[Person].getIterator()

    makeNamesHTMLTable(people)
  }

  get("/delete/:id") {
    ds.query[Person].remove(params("id"))
    redirect("/names")
  }

  get("/find") {
    <html><body>
      <h3>Find Person</h3>
      {personForm("find")}
      <a href="names">Return to Main Menu</a>
    </body></html>
  }

  post("/find") {
    val nameOption = params.get("name").flatMap( _ match {
      case "" => None
      case s => Some(s)
    })

    val ageOption = params.get("age").flatMap( _ match {
      case "" => None
      case str => Some(str.toInt)
    })

    val query = ds.query[Person]

    val results = (nameOption, ageOption) match {
      case (Some(name), Some(age)) => query.filter(p => p.name == name && p.age == age)
      case (Some(name), None) => query.filter(p => p.name == name)
      case (None, Some(age)) => query.filter(p => p.age == age)
      case (None, None) => query
    }

    makeNamesHTMLTable(results.getIterator())
  }

  get("/submit") {
     <html>
       <body>
        <h3>Person to Submit</h3>
        {personForm("submit")}
        <a href="names">Return to Main Menu</a>
      </body>
     </html>
  }

  post("/submit") {
    val person = Person(multiParams("name").head, multiParams("age").head.toInt)
    ds.put(person)
    redirect("/names")
  }
}
