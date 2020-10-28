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
  implicit def list1dIsArray[T: IsElement] = IsArray[List1d[T], T](
    fgetEmpty = self => List1d[T](List()),
    fgetAtN = (self, n) => self.data(n),
    flength = self => self.data.length,
    fcons = (self, elem) => List1d(elem :: self.data),
  )
  implicit def list1dIs1d[T: IsElement] = Is1d[List1d[T], T]

  case class List2d[T: IsElement] ( 
    data: List[List[T]],
  )
  implicit def list2dIsArray[T: IsElement] = IsArray[List2d[T], List1d[T]] (
    fgetEmpty = self => List2d[T](List(List())),
    fgetAtN = (self, n) => List1d(self.data(n)),
    flength = self => self.data.length,
    fcons = (self, elem) => List2d(elem.data :: self.data),
  )
  implicit def list2dIs2d[T: IsElement] = Is2d[List2d[T], List1d[T]]

  case class List3d[T: IsElement] ( 
    data: List[List[List[T]]],
  )
  implicit def list3dIsArray[T: IsElement] = IsArray[List3d[T], List2d[T]] (
    fgetEmpty = self => List3d[T](List(List(List()))),
    fgetAtN = (self, n) => List2d(self.data(n)),
    flength = self => self.data.length,
    fcons = (self, elem) => List3d(elem.data :: self.data),
  )
  implicit def list3dIs3d[T: IsElement] = Is3d[List3d[T], List2d[T]]
}

