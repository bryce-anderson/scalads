package scalads.mongodb

import reactivemongo.bson._

/**
 * @author Bryce Anderson
 *         Created on 6/19/13
 */


class ScalaDSObject(val collection: String, val json: BSONDocument) { self =>

  def this(collection: String) = this(collection, BSONDocument())

  def getReplacement(): ScalaDSObject = {
    json.get(MongoDatastore.id) match {
      case None => sys.error("Cannot replace entity: doesn't have key.")
      case Some(value: BSONObjectID) => new ScalaDSObject(collection, BSONDocument(MongoDatastore.id -> value))
    }
  }
}