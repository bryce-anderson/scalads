package myapp

import org.scalatra.{FutureSupport, ScalatraServlet}
import scalads.mongodb.{ScalaDSObject, MongoDatastore}
import reactivemongo.api.{DB, MongoConnection, MongoDriver}

import play.api.libs.iteratee.{Enumerator, Iteratee}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.xml.NodeBuffer
import scala.concurrent.{ExecutionContext, Future}
import scalads.core.EntityBacker

/**
 * @author Bryce Anderson
 *         Created on 6/30/13
 */


case class Person(name: String, age: Int)


class Main extends ScalatraServlet with FutureSupport {

  protected implicit def executor: ExecutionContext = implicitly[ExecutionContext]

  private lazy val ds = {
    import com.typesafe.config.ConfigFactory

    val config = ConfigFactory.load()
    val driver = new MongoDriver
    val connection: MongoConnection = driver.connection(List(config.getString("mongo.address")+":"+config.getInt("mongo.port")))
    def db:DB = connection("testdb")

    val username = config.getString("mongo.username")
    if (username != "null") {
      db.authenticate(username, config.getString("mongo.pass"))(3.seconds)
    }

    new MongoDatastore(db)
  }

  private def makeNamesTable(enum: Enumerator[Person with EntityBacker[Person, ScalaDSObject]]) = {
    val respBuffer = new NodeBuffer

    val finished = enum |>> Iteratee.foreach{p =>
      val deladdress = "/delete/" + p.ds_idString()

      respBuffer += <tr>
        <td>{p.name}</td>
        <td>{p.age}</td>
        <td>
          <form name="input" action={deladdress} method="get"><button>Delete</button></form>
        </td>
      </tr>
    }

    finished.map { _ =>
      <html><body>
        <h2>List of People.</h2>
        <a href="/names">All People</a><br/>
        <a href="/submit">Submit new person.</a><br/>
        <a href="/find">Find People.</a>
        <table>
          {respBuffer.result()}
        </table>
      </body></html>
    }
  }

  get("/names") {
    val people = ds.query[Person].getIterator().enumerate

    makeNamesTable(people)
  }

  get("/delete/:id") {
    ds.query[Person].remove(params("id"))
    redirect("/names")
  }

  def personForm(action: String) =
    <form name="input" action={action} method="post">
      Name: <input type="text" name="name"/><br/>
      Age: <input type="number" name="age" /><br/>
      <button>Submit</button>
    </form>

  get("/find") {
    <html><body>
      Find person:<br/>
      {personForm("/find")}
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


    makeNamesTable(results.getIterator().enumerate)
  }

  get("/submit") {
     <html><body>
     Person to Submit:<br/>
       {personForm("/submit")}
       <a href="/names">Return to Main Menu</a>
     </body></html>
  }

  post("/submit") {
    val person = Person(multiParams("name").head, multiParams("age").head.toInt)
    ds.put(person)
    redirect("/submit")
  }
}
