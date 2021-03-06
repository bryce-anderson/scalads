package scalads.mongodb

import reactivemongo.bson._

/**
 * @author Bryce Anderson
 *         Created on 6/19/13
 */


class ScalaDSObject(val collection: String, val json: BSONDocument) { self =>

  def this(collection: String) = this(collection, BSONDocument())

  def getID: BSONObjectID =json.get(MongoDatastore.id) match {
    case None => sys.error("Cannot replace entity: doesn't have key.")
    case Some(value: BSONObjectID) => value
    case e => sys.error(s"BSONDocument contains wrong type in id field: $e")
  }

  def getIDString() = getID.stringify

  def getReplacement(): ScalaDSObject = new ScalaDSObject(collection, BSONDocument(MongoDatastore.id -> getID))
}