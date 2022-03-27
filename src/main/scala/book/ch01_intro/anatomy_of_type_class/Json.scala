package book.ch01_intro.anatomy_of_type_class

sealed trait Json
final case class JsObject(values: Map[String, Json]) extends Json
final case class JsString(value: String) extends Json
final case class JsNumber(value: BigDecimal) extends Json
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

object JsonWriter {
  implicit val intWriter: JsonWriter[Int] = (value: Int) => JsNumber(BigDecimal(value))

  implicit val stringWriter: JsonWriter[String] = (value: String) => JsString(value)

  implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] = {
    case Some(value) => writer.write(value)
    case None => JsNull
  }
}
