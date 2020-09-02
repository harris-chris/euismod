package sportarray

import scala.collection.mutable.HashMap
import com.github.nscala_time.time.Imports._

import sportdate.{IsSportDate, IsSportDateInstances, IsSportDateSyntax, SportDate}
import sportdate.IsSportDateInstances._
import sportdate.IsSportDateSyntax._

object Skeleton {


  // Define your DataTypes here - these are the types of value used in the body of the array
  sealed trait DataType {
    type ElemT 
  }
  trait PositionsData extends DataType { type ElemT = Double }
  trait WeightsData extends DataType { type ElemT = Double }
  trait ValuesData extends DataType { type ElemT = Double }
  trait PricesData extends DataType { type ElemT = Double }

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
