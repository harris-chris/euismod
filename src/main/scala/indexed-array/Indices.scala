package sportarray

import sportdate.{IsSportDateInstances, IsSportDateSyntax}
import Skeleton.{IsIdxElem, DateType, Element, Composite}
import Skeleton.IsIdxElemImplicits._

object IndicesObj {

  case class Index[ElemT: IsIdxElem] (
    vals: List[ElemT]
  ) {
    def indexOf(at: ElemT): Option[Int] = vals.indexOf(at) match {
      case -1 => None
      case n => Some(n)
    }
    def apply(i: Int): ElemT = vals(i)
    def ++(idx: Index[ElemT]) = Index[ElemT](vals ++ idx.vals)
  }
  object Index {
    def apply[ElemT: IsIdxElem](vals: ElemT*): Index[ElemT] = Index[ElemT](vals.toList)
  }
}
