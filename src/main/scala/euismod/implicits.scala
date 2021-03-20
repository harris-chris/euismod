package euismod

import shapeless._
import shapeless.ops.hlist._

package object implicits {

  implicit def listIsArray[T] = new IsArray[List, T] {
    type S = T
    def getEmpty[_T] = Nil: List[_T]
    def getAtN(a: List[T], n: Int) = a(n)
    def length(a: List[T]) = a.length
    def cons(a: List[T], sub: S) = sub :: a
  }

  type LL[T] = List[List[T]]
  implicit def listOfListsIsArray[T] = new IsArray[LL, T] {
    type S = List[T]
    def getEmpty[_T] = Nil: List[List[_T]]
    def getAtN(a: List[List[T]], n: Int) = a(n)
    def length(a: List[List[T]]) = a.length
    def cons(a: List[List[T]], sub: S): List[List[T]] = sub :: a
  }

  case class List1d[T] (
    data: List[T],
  )
  implicit def list1dIsArray[T] = new IsArray[List1d, T] {
    type S = T
    def getEmpty[_T] = List1d[_T](Nil: List[_T])
    def getAtN(a: List1d[T], n: Int) = a.data(n)
    def length(a: List1d[T]) = a.data.length
    def cons(a: List1d[T], sub: S) = List1d(sub :: a.data)
  }

  case class List2d[T] ( 
    data: List[List[T]],
  )
  implicit def list2dIsArray[T] = new IsArray[List2d, T] {
    type S = List1d[T]
    def getEmpty[_T]: List2d[_T] = List2d[_T](Nil: List[List[_T]])
    def getAtN(a: List2d[T], n: Int): S = List1d(a.data(n))
    def length(a: List2d[T]): Int = a.data.length
    def cons(a: List2d[T], sub: S): List2d[T] = List2d(sub.data :: a.data)
  }

  case class List3d[T] ( 
    data: List[List[List[T]]],
  )
  implicit def list3dIsArray[T] = new IsArray[List3d, T] {
    type S = List2d[T]
    def getEmpty[_T] = List3d[_T](Nil: List[List[List[_T]]])
    def getAtN(a: List3d[T], n: Int) = List2d(a.data(n))
    def length(a: List3d[T]) = a.data.length
    def cons(a: List3d[T], sub: S): List3d[T] = List3d(sub.data :: a.data)
  }
}

