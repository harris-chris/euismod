package sportarray

object ErrorsObj {
  trait IsError

  import ArrayDefs._

  trait EinsumError
  type EinsumEither[A] = Either[EinsumError, A]

  case class OutOpElemNotInInOp[A](
    inOp: List[A],
    outOp: List[A],
    badElems: List[A],
  ) extends EinsumError {
    def getErr: String = s"Element(s) ${badElems} in output operand ${outOp} was not found in input operand ${inOp}" 
  }
  //case class IndexOutOfBound[A[_], T] (
    //sh: ShapeRT[A[T], HNil],
    //arr: A[T],
    //idx: Int,
  //) extends IsError {
    //def show: String = 
      //s"Array ${arr} with shape ${sh(arr)} has no index ${idx}"
  //}
}
