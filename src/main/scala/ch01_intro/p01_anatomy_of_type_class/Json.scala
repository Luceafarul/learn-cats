package ch01_intro.p01_anatomy_of_type_class

sealed trait Json
final case class JsObject(get: Map[String, Json]) extends Json
final case class JsString(get: String) extends Json
final case class JsNumber(get: Double) extends Json
case object JsNull extends Json

object Json {
    def toJson[A](value: A)(implicit writer: JsonWriter[A]): Json = writer.write(value)
}

object JsonSyntax {
    implicit class JsonWriterOps[A](value: A) {
        def toJson(implicit writer: JsonWriter[A]): Json = writer.write(value)
    }
}

trait JsonWriter[A] {
    def write(value: A): Json
}
