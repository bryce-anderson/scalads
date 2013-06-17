# ScalaDS

ScalaDS aims to provide macro based serialization and deserialization of classes for different datastore backends.

## Philosophy

This is an experiment to see if I could make a datastore that would abstract the core features of serialization and generating queries while letting the native features of the backends shine through. To that end, entities are always natively accessible from query results even when deserializing to its scala class.

# Features
- Automatic serialization and deserialization of fields in class constructors (case class and regular class)
- De-serialized classes are *augmented* with convenience methods for updating the datastore
- Supports nested types and flattens the structure for storage
- Type safe query system
- Don’t need to add Serializable interface to classes
- Returned “entities” are the real scala case classes augmented with different datastore methods to allow updating, removing, etc. See the ‘ds_’ methods of the EntityBacker trait that is mixed in to returned entities.



## Supported Backends

### Google App Engine Datastore
The starting point for this project, Googles App Engine Datastore is a first class citizen.

### MongoDB
Fresh off the press is MongoDB support. It’s largely feature complete, but like the rest of this young lib, there are almost certainly bugs.

## Flux

The library is still largely in the experimental stage. I expect the library to have an initially high code. Hopefully features will come and bugs will go...

## sbt Dependency

There are currently no packages in maven repos which can automatically be pulled. The best way to get the library is to pull the github url directly.

## Examples

*getting the datastore will depend on the backend being used as there are different connection considerations*

### Serializing a class

```scala

val ds = Datastore.getDatastoreService()

case class Person(name: String, age: Int)

val bill = Person("Bill", 28)

ds.put(bill)
```

### Querying a class

```scala

val ds = Datastore.getDatastoreService()

case class Person(name: String, age: Int)

val it = ds.query[Person].getIterator

for(p <- it) println(s"Hello ${p.name}. You are ${p.age} old!")

```

### Applying filters and sorting

```scala

val ds = Datastore.getDatastoreService()

case class Person(name: String, age: Int)

val it = ds.query[Person]
    .filter(p => p.age > 21)
    .sortAsc(_.age)
    .getIterator

for(p <- it) println(s"Hello ${p.name}. You are older than 21")

```

### Projecting Results

```scala

val ds = Datastore.getDatastoreService()

case class Person(name: String, age: Int)
case class Team(teamName: String, person1: Person, person2: Person)

val team = Team("cool", Person("Bill", 12), Person("Jane", 11))
ds.put(team)

val it = ds.query[Team]
  .sortAsc(_.teamName)       // Shortcut for .sortAsc(p => p.age)
  .project{ t =>
  val p1 = t.person1
  (t.teamName, p1.name)
}

for(p <- it) println(s"Hello ${p._2}. You are on team ${p._1}")

```

## Author
Bryce Anderson
