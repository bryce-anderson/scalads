package scalads.mongodb

import com.mongodb.{BasicDBObject, DBObject}
import org.bson.types.ObjectId

/**
 * @author Bryce Anderson
 *         Created on 6/19/13
 */


class ScalaDSObject(val collection: String, val json: DBObject) { self =>

  def this(collection: String) = this(collection, new BasicDBObject())

  def getReplacement(): ScalaDSObject = {
    json.get(MongoDatastore.id) match {
      case null => sys.error("Cannot replace entity: doesn't have key.")
      case id: ObjectId => new ScalaDSObject(collection, new BasicDBObject(MongoDatastore.id, id))
    }
  }
}