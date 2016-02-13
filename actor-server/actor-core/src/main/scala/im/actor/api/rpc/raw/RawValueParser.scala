package im.actor.api.rpc.raw

import im.actor.api.rpc.collections._

import scala.util.Try

trait RawValueParser[A] {
  def parse(raw: ApiRawValue): Option[A]
}

// target parsing type is ApiArrayValue
// todo: add parser for ApiMapValue, to make Map[String, Any]. Not too usefull
object RawValueParser {
  def apply[T: RawValueParser]: RawValueParser[T] = implicitly[RawValueParser[T]]

  def parse[A: RawValueParser](raw: ApiRawValue): Option[A] = RawValueParser[A].parse(raw)

  import shapeless._

  implicit val booleanParser = new RawValueParser[Boolean] {
    def parse(raw: ApiRawValue): Option[Boolean] = raw match {
      case ApiStringValue(v) ⇒ Try(v.toBoolean).toOption
      case _                 ⇒ None
    }
  }

  implicit val stringParser = new RawValueParser[String] {
    def parse(raw: ApiRawValue): Option[String] = raw match {
      case ApiStringValue(v) ⇒ Some(v)
      case _                 ⇒ None
    }
  }

  implicit val doubleParser = new RawValueParser[Double] {
    def parse(raw: ApiRawValue): Option[Double] = raw match {
      case ApiDoubleValue(v) ⇒ Some(v)
      case _                 ⇒ None
    }
  }

  implicit val intParser = new RawValueParser[Int] {
    def parse(raw: ApiRawValue): Option[Int] = raw match {
      case ApiInt32Value(v) ⇒ Some(v)
      case _                ⇒ None
    }
  }

  implicit val longParser = new RawValueParser[Long] {
    def parse(raw: ApiRawValue): Option[Long] = raw match {
      case ApiInt64Value(v) ⇒ Some(v)
      case _                ⇒ None
    }
  }

  implicit val hnilParser: RawValueParser[HNil] = new RawValueParser[HNil] {
    def parse(raw: ApiRawValue): Option[HNil] = raw match {
      case ApiArrayValue(arr) if arr.isEmpty ⇒ Some(HNil)
      case _                                 ⇒ None
    }
  }

  // we know how to parse ApiRawValue to hcons only if ApiRawValue is Array
  implicit def hconsParser[H: RawValueParser, T <: HList: RawValueParser]: RawValueParser[H :: T] = new RawValueParser[H :: T] {
    def parse(raw: ApiRawValue): Option[H :: T] = raw match {
      case ApiArrayValue(arr) ⇒ arr match {
        case h +: t ⇒
          for {
            head ← RawValueParser[H].parse(h)
            tail ← RawValueParser[T].parse(ApiArrayValue(t))
          } yield head :: tail
      }
      case _ ⇒ None
    }
  }

  implicit def caseClassParser[A, R <: HList](implicit gen: Generic[A] { type Repr = R }, reprParser: RawValueParser[R]): RawValueParser[A] = new RawValueParser[A] {
    def parse(raw: ApiRawValue): Option[A] = reprParser.parse(raw).map(gen.from)
  }

}
