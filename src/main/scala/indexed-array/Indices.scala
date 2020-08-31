package petri

import DateObj.Date
import Skeleton.{IsIdxElem, IsDate, IsSecurity}

object IndicesObj {

  trait IsIndex[ElemT <: IsIdxElem] {
    val vals: List[ElemT]
    def indexOf(at: ElemT): Option[Int]
  }

  case class DateIndex(
    val vals: List[IsDate[Date]]
  ) extends IsIndex[IsDate[Date]] {
    def indexOf(at: IsDate[Date]): Option[Int] = vals.indexOf(at) match {
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
