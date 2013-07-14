package myapp

import com.google.appengine.api.datastore.Entity

import scala.xml.NodeBuffer
import scalads.core.EntityBacker
import javax.ws.rs.GET

/**
 * @author Bryce Anderson
 *         Created on 6/30/13
 */

trait PeopleSearch extends DataConnection {

  protected def makeNamesHTMLTable(iterator: Iterator[Person with EntityBacker[Person, Entity]]) = {
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
}

class Names extends PeopleSearch {
  @GET
  def get() = {
    val people = ds.query[Person].getIterator()

    makeNamesHTMLTable(people)
  }
}

class Delete extends PeopleSearch {
  import jaxed.servlet.Redirect

  @GET
  def get(id: String) = {
    ds.query[Person].remove(id)
    "redirect to /names"
    Redirect("/names")
  }
}