package sportarray

import sportdate.{IsSportDateInstances, IsSportDateSyntax}
import Skeleton.{IsIdxElem, IsSecurity, IsDate}

object IndicesObj {

  trait IsIndex[ElemT <: IsIdxElem[_]] {
    val vals: List[ElemT]
    def indexOf(at: ElemT): Option[Int]
  }

  case class DateIndex(
    val vals: List[IsDate[_]]
  ) extends IsIndex[IsDate[_]] {
    def indexOf(at: IsDate[_]): Option[Int] = vals.indexOf(at) match {
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
