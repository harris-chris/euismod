package sportarray

import ArrayDefs._
import Skeleton._
import IndicesObj._

import shapeless._
import shapeless.ops.hlist._

object ListOfListsObj {

  case class List1d[T: IsElement] (
    data: List[T],
  )
  implicit def list1dIsArray[T: IsElement] = new IsArray[List1d, T] {
    type S = T
    def getEmpty[_T: IsElement] = List1d[_T](Nil: List[_T])
    def getAtN(a: List1d[T], n: Int) = a.data(n)
    def length(a: List1d[T]) = a.data.length
    def cons(a: List1d[T], other: S) = List1d(other :: a.data)
  }
  implicit def list1dIs1d[T: IsElement] = Is1d[List1d, T]

  case class List2d[T: IsElement] ( 
    data: List[List[T]],
  )
  implicit def list2dIsArray[T: IsElement] = new IsArray[List2d, T] {
    type S = List1d[T]
    def getEmpty[_T: IsElement]: List2d[_T] = List2d[_T](Nil: List[List[_T]])
    def getAtN(a: List2d[T], n: Int): S = List1d(a.data(n))
    def length(a: List2d[T]): Int = a.data.length
    def cons(a: List2d[T], sub: S): List2d[T] = List2d(sub.data :: a.data)
  }
  implicit def list2dIs2d[T: IsElement] = Is2d[List2d, T, List1d[T]]

  case class List3d[T: IsElement] ( 
    data: List[List[List[T]]],
  )
  implicit def list3dIsArray[T: IsElement] = new IsArray[List3d, T] {
    type S = List2d[T]
    def getEmpty[_T: IsElement] = List3d[_T](Nil: List[List[List[_T]]])
    def getAtN(a: List3d[T], n: Int) = List2d(a.data(n))
    def length(a: List3d[T]) = a.data.length
    def cons(a: List3d[T], sub: S): List3d[T] = List3d(sub.data :: a.data)
  }
  implicit def list3dIs3d[T: IsElement] = Is3d[List3d, T, List2d[T]]
}

