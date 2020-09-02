package sportarray

import sportdate.{IsSportDateInstances, IsSportDateSyntax}
import Skeleton.{IsIdxElem, IsSecurity, DateType}
import Skeleton.IsIdxElemImplicits._

object IndicesObj {

  abstract class IsIndex[ElemT: IsIdxElem] {
    def vals: List[ElemT]
    def indexOf(at: ElemT): Option[Int]
  }

  case class DateIndex(
    val vals: List[DateType]
  ) extends IsIndex[DateType] {
    def indexOf(at: DateType): Option[Int] = vals.indexOf(at) match {
      case -1 => None
      case n => Some(n)
    }
  }

  case class HoldingsIndex(
    val vals: List[IsSecurity]
  ) extends IsIndex[IsSecurity] {
    def indexOf(at: IsSecurity): Option[Int] = vals.indexOf(at) match {
      case -1 => None
      case n => Some(n)
    }
  }
}
