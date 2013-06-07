package macros

import scalads.macroimpls
import scalads.Entity
import scalads.writers.GAEDSWriter
import scalads.readers.GAEObjectReader

import scalads.macroimpls.Deserializer.deserialize

/**
 * @author Bryce Anderson
 *         Created on 5/30/13
 */
class DeserializerSpec extends GAESpecTemplate {

  case class Simple(in1: Int, in2: String)
  val mySimple = Simple(1, "two")
  case class Compound(in1: Int, in2: Simple, in3: String)
  val myCompound = Compound(1, mySimple, "three")
  case class Three(in: Compound)
  val three = Three(myCompound)

  "Deserializer" should "extract a simple entity" in {
    val writer = new GAEDSWriter(new Entity("macros.DeserializerSpec.Simple"))
    macroimpls.Serializer.serialize(mySimple, writer)
    val entity = writer.result

    val reader = GAEObjectReader(entity)

    val simple = deserialize[Simple](reader)

    simple should equal (mySimple)
  }

  it should "Work with compound objects" in {
    val writer = new GAEDSWriter(new Entity("macros.DeserializerSpec.Compound"))
    macroimpls.Serializer.serialize(myCompound, writer)
    val entity = writer.result

    val reader = GAEObjectReader(entity)

    val compound = deserialize[Compound](reader)
    println(compound.ds_key)

    compound should equal (myCompound)
  }

  it should "work with three fold deap objects" in {
    val writer = new GAEDSWriter(new Entity("macros.DeserializerSpec.Three"))
    macroimpls.Serializer.serialize(three, writer)
    val entity = writer.result

    val reader = GAEObjectReader(entity)

    val out = deserialize[Three](reader)

    out should equal (three)
  }

  it should "handle objects with type args" in {

    case class Test[A](in: A)
    val three = Test(1)
    val writer = new GAEDSWriter(new Entity("macros.DeserializerSpec.Test[Int]"))
    macroimpls.Serializer.serialize(three, writer)
    val entity = writer.result

    val reader = GAEObjectReader(entity)

    val out = deserialize[Test[Int]](reader)

    out should equal (three)
  }

  it should "generate similar classes" in {
    val writer = new GAEDSWriter(new Entity("macros.DeserializerSpec.Simple"))
    macroimpls.Serializer.serialize(mySimple, writer)
    val entity = writer.result

    val reader = GAEObjectReader(entity)

    val simple1 = deserialize[Simple](reader)
    val simple2 = deserialize[Simple](reader)

    simple1 should equal (simple2)
  }
}
