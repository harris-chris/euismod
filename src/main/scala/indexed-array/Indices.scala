package sportarray

import sportdate.{IsSportDateInstances, IsSportDateSyntax}
import Skeleton.{IsIdxElem, IsSecurity, IsDate}
import Skeleton.IsIdxElemImplicits._

object IndicesObj {

  abstract class IsIndex[ElemT: IsIdxElem] {
    val vals: List[ElemT]
    def indexOf(at: ElemT): Option[Int]
  }

  case class DateIndex[A: IsDate](
    val vals: List[A]
  ) extends IsIndex[A] {
    def indexOf(at: A): Option[Int] = vals.indexOf(at) match {
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
