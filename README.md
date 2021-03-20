Euismod provides fully-typed n-dimensional array functionality to any array-like object.

A [numpy](https://numpy.org/)-like set of array methods are provided by [shapeless](https://github.com/milessabin/shapeless)-like typeclasses.

For example:
```
import euismod._

// a very simple array-like - a list of lists
// an instance of the euismod.IsArray typeclass is already imported for lists of lists
val arrayLike2d = List(List("a", "b", "c"), List("d", "e", "f"))

println(arrayLike2d.shape) // (2, 3)
println(arrayLike2d(List

// and here's an instance of it.
val numbersOfFruitIIntendToBuy = ListOfListsWithIndices[Numbers, LocalDate, String](
  values=List(List(0, 1, 0), List(1, 0, 2)),
  indices=(
    List(LocalDate.of(2021,1,1), LocalDate.of(2020,1,2)), 
    List("Apple", "Orange", "Tomato, technically"),
  )
)

// very similar: an array of arrays can be a 2-d array
case class ArrayOfArrayWithIndices[T <: UnitType, Idx0, Idx1](
  values: Array[Array[T#ElemT]],
  indices: (List[Idx0], List[Idx1]),
)

val gramsOfPowdersIAlsoWillBuy = ArrayOfArrayWithIndices[Grams, LocalDate, String](
  values=Array(Array(100, 130, 150), Array(210, 0, 50)),
  indices=(
    List(LocalDate.of(2021,1,1), LocalDate.of(2020,1,2)), 
    List("Sugar", "Salt", "Sumac"),
  )
)

// consistent setter, getter and other functions are provided automatically
println(gramsOfPowdersIAlsoWillBuy.loc("2020-01-01", "Salt")) // 130, of course
val moarOranges = numbersOfFruitIIntendToBuy.setLoc(("2020-01-02", "Orange"), 30)
// addition is handled automatically...
val doubleThatFruitOrder = numbersOfFruitIIntendToBuy + numbersOfFruitIIntendToBuy 
// but in a UnitType-aware way; the following will produce a compile error
val neverMixYourUnits = numbersOfFruitIIntendToBuy + gramsOfPowdersIAlsoWillBuy
// getting an element will return an array of one lesser dimension
val arr1d = numbersOfFruitIIntendToBuy.loc("Orange") // arr1d implements the Is1dIndexArr typeclass
// thanks to the marvels of typing, arithmetic between array types can be handled automatically
trait Euros extends UnitType { type ElemT = Double }
case class SingleIndexedList[T <: UnitType, Idx0](
  values: List[T#ElemT],
  indices: List[Idx0],
)
val eurosPerGram = SingleIndexedList[Euros, String](
  values=List(0.01, 0.015, 0.02, 2400.0),
  indices=List("Sugar", "Salt", "Sumac", "Radium"),
)
val totalCostOfMyPowders = gramsOfPowdersIAlsoWillBuy * eurosPerGram
```

How does this help humanity with its current problems? *Glad you asked*
1. Array types can be hot-swapped, or used in a mixed fashion in the code.
2. Strictly typing the arrays reduces the likelihood of error
3. Retaining index and UnitType information for the arrays allows many common array operations (joins, merges, dot products) to be done with a trivially simple API
