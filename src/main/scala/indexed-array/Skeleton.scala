package sportarray

import scala.collection.mutable.HashMap
import com.github.nscala_time.time.Imports._

import sportdate.{IsSportDate, IsSportDateInstances, IsSportDateSyntax, SportDate}
import sportdate.IsSportDateInstances._
import sportdate.IsSportDateSyntax._

object Skeleton {

  abstract class IsBase[A]

  // Define your DataTypes here - these are the types of value used in the body of the array
  abstract class IsElement[A] extends IsBase[A]
  implicit val PositionsData = new IsElement[Double] {}
  //implicit val WeightsData = new DataType[Int] {}
  //abstract class PositionsData extends DataType { type Self = Double }
  //abstract class WeightsData extends DataType { type Self = Double }
  //abstract class ValuesData extends DataType { type Self = Double }
  //abstract class PricesData extends DataType { type Self = Double }

  // IsIdxElem is a trait which defines whether a particular type can be used for an index
  trait IsIdxElem[A]
  // We are using DateTime (imported from nscala_time) as our date type throughout
  type DateType = DateTime
  // A couple of other example index types
  case class Element(name: String)
  case class Composite(name: String)
  object IsIdxElemImplicits {
    implicit val dateTimeIsIdxElem = new IsIdxElem[DateType] {}
    implicit val elementIsIdxElem = new IsIdxElem[Element] {}
    implicit val compositeIsIdxElem = new IsIdxElem[Composite] {}
  }
}
