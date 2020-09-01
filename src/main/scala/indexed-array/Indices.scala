package sportarray

import sportdate.{IsSportDateInstances, IsSportDateSyntax}
import sportdate.{IsSportDate => IsDate}
import Skeleton.{IsIdxElem, IsSecurity, Date}

object IndicesObj {

  trait IsIndex[ElemT <: IsIdxElem] {
    val vals: List[ElemT]
    def indexOf(at: ElemT): Option[Int]
  }

  case class DateIndex(
    val vals: List[Date]
  ) extends IsIndex[Date] {
    def indexOf(at: Date): Option[Int] = vals.indexOf(at) match {
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
