//package sportarray

//import sportdate.{IsSportDateInstances, IsSportDateSyntax}
//import Skeleton.{DateType, Element, Composite}
//import Skeleton.IsIdxElemImplicits._
//import shapeless.{HList, HNil, :: => #:}

//object IndicesObj {

  //case class Index[ElemT] (
    //vals: List[ElemT]
  //) {
    //def indexOf(at: ElemT): Option[Int] = vals.indexOf(at) match {
      //case -1 => None
      //case n => Some(n)
    //}
    //def apply(i: Int): ElemT = vals(i)
    //def ++(idx: Index[ElemT]) = Index[ElemT](vals ++ idx.vals)
    //def :+(elem: ElemT) = Index[ElemT](vals :+ elem)
    //def toList = vals
    //def length = vals.length
  //}
  //object Index {
    //def empty[ElemT]: Index[ElemT] = Index[ElemT](Nil: List[ElemT])
    //def apply[ElemT](vals: ElemT*): Index[ElemT] = Index[ElemT](vals.toList)
  //}
//}
