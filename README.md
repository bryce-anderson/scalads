# ScalaDS

ScalaDS aims to provide macro based serialization and deserialization of classes for different datastore backends.

# Features
- Automatic serialization and deserialization of fields in class constructors (case class and regular class)
- Deserialized classes are *augmented* with convienence methods for updating the datastore
- Supports nested types and flattens the structure for storage
- Type safe query system
- Dont need to add Serializable interface to classes


## Supported Backends

Right now the project is getting lifted off the ground with Googles App Engine Datastore. Other backends may be targeted in the future such as MongoDB and possibly traditional SQL style storage systems.

## Flux

The library is still largely in the experimental stage. Features and bugs will be in high flux until the abstract datastore interface has been generalized. Hopefully features will come and bugs will go...

## sbt Dependency

There are currently no packages in maven repos which can automatically be pulled. The best way to get the library is to pull the github url directly.

## Examples

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
val favoriteNumber = 7

val it = ds.query[Person]
    .filter(p => p.age > 21)
    .sortAsc(_.age)       // Shortcut for .sortAsc(p => p.age)
    .project(p => (p.name, favoriteNumber))

for(p <- it) println(s"Hello ${p._1}. You get a favorite number: ${p._2}")

```

## Author
Bryce Anderson