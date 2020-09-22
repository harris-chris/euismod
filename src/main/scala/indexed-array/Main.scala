package sportarray

object Main extends App {
  import ListOfListsObj._
  import ArrayDefs._
  import ArrayDefs.Is1dSpArrSyntax._
  import Skeleton._
  import Skeleton.IsIdxElemImplicits._
  import IndicesObj.Index
  import sportdate.SportDate
  import sportdate.{IsSportDateInstances, IsSportDateSyntax}
  type Dim2T = DateType
  val dim2 = Index(
    SportDate.YMD(2020,8,1), SportDate.YMD(2020,8,2), SportDate.YMD(2020,8,3), 
    SportDate.YMD(2020,8,4), SportDate.YMD(2020,8,5),
  )
  val values1d = List(0.1, 0.2, 0.3, 0.4, 0.5)
  val list1d = List1d[Dim2T, PositionsData](dim2, values1d)
  values1d.zipWithIndex.forall({case(x, i) => x == list1d.iloc(i)})
}
