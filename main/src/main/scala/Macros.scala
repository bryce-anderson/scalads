
import language.experimental.macros


trait Macros {
//  def serialize[U](obj: U, writer: JsonWriter[_])(implicit defaultFormats: Formats) =
//    macro macroimpls.Serializer.serializeImpl[U]
//
//  def decompose[U](obj: U)(implicit defaultFormats: Formats): JValue =
//    macro macroimpls.Serializer.decompose[U]
//
//  def deserialize[U](reader: Reader)(implicit defaultFormats: Formats): U =
//    macro macroimpls.Deserializer.deserialize_impl[U]
//
//  def read[U](str: String)(implicit defaultFormats: Formats): U =
//    macro macroimpls.Deserializer.read_impl[U]
//
//  def serializeToString[U](obj: U)(implicit defaultFormats: Formats): String =
//    macro macroimpls.Serializer.serializeToString[U]
//
//  def decomposeWithBuilder[U, T](obj: U, builder: JsonWriter[T])(implicit formats: Formats) =
//  macro macroimpls.Serializer.decomposeWithBuilder_impl[U,T]
//
//  def extract[U](jvalue: JValue)(implicit defaultFormats: Formats): U =
//  macro macroimpls.Deserializer.extract_impl[U]
}

object Macros extends Macros

