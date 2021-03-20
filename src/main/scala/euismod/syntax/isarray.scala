package euismod
package syntax

import shapeless._
import euismod._

trait IsArraySyntax {
  implicit class IsArrayOps[A[_], T, _S](a: A[T])(implicit 
    val tc: IsArray[A, T] { type S = _S },
  ) {
    def getEmpty[_T] = tc.getEmpty[_T]
    def empty = tc.getEmpty[T]
    def getAtN(n: Int): _S = tc.getAtN(a, n)
    def apply[R](r: R)(implicit ai: ApplyIndex[A[T], R]) = tc.apply(a, r)
    def ::(other: _S) = tc.cons(a, other)
    def ++[B[_]](b: B[T])(implicit cn: Concatenate[A, B, T, Nat._0]) = tc.++(a, b)
    def length: Int = tc.length(a)
    def toList(implicit ls: ListSubs[A[T]]): ls.Out = tc.toList(a)
    def fromList(listS: List[_S]): A[T] = tc.fromList(listS)
    def shape(implicit sh: Shape[A[T]]): sh.Out = tc.shape(a)
    def flatten(implicit fl: Flatten[A, T]): List[T] = fl(a)
    def fromElems[AR <: HList, SH <: HList](listT: List[T], shape: SH)(implicit 
      ga: SubArrays.Aux[A[T], AR],
      fr: FromElemsAndSubArraysOpt[AR, T, SH],
    ): fr.Out = tc.fromElems(a, listT, shape)
    def fromElems[AR <: HList, SH <: HList](listT: List[T])(implicit 
      sh: Shape[A[T]] { type Out = SH },
      ga: SubArrays.Aux[A[T], AR],
      fr: FromElemsAndSubArraysOpt[AR, T, SH], 
    ): fr.Out = tc.fromElems(a, listT)
    def reshape[AR <: HList, SH <: HList](shape: SH)(implicit 
      fl: Flatten[A, T],
      ga: SubArrays.Aux[A[T], AR],
      rs: FromElemsAndSubArraysOpt[AR, T, SH],
    ) = tc.reshape(a, shape)
    def map[_T, AR <: HList, SH <: HList](f: T => _T)(implicit
      ai: IsArray[A, _T],
      fl: Flatten[A, T],
      sh: Shape.Aux[A[T], SH], 
      ga: SubArrays.Aux[A[_T], AR],
      fr: FromElemsAndSubArraysOpt.Aux[AR, _T, SH, Option[A[_T]]],
    ): A[_T] = tc.map(a, f)
  }
}
