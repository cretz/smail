package scimap

import scala.collection.immutable.TreeSet
import scala.collection.immutable.NumericRange
import scala.math.ScalaNumber
import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

trait Util {
  
  type BigIntRange = (BigInt, Option[BigInt])
  
  def bigIntGaps(ranges: Iterable[BigIntRange]): TreeSet[BigIntRange] = {
    ranges.foldLeft(TreeSet.empty[BigIntRange]) { case (set, range) =>
      bigIntGaps(range._1, range._2, set)
    }
  }
  
  // We promise only the last one can have an empty end 
  @tailrec
  final def bigIntGaps(
    start: BigInt,
    end: Option[BigInt],
    currentSet: TreeSet[BigIntRange] = TreeSet.empty
  ): TreeSet[BigIntRange] = {
    // We must find the next start/end number
    val from = currentSet.from(start -> None)
    var properStart = start
    var properEnd = Option.empty[BigInt]
    if (!from.isEmpty) {
      // Next non-consecutive value
      properEnd = from.tail.find({ range =>
        if (range._1 == properStart + 1) { properStart = range._2.get; false }
        else true
      }).map(_._1)
    }
    // Need to update end if set
    if (!end.isEmpty) properEnd = end.map { end =>
      if (properEnd.isEmpty) end
      else end.min(properEnd.get)
    }
    // Now that we have a range, we can do the next one recursively
    // If the end is not empty, we need to run this again with what's asked for excluded
    if (properEnd.isEmpty || properEnd.get == properStart) currentSet + (properStart -> properEnd)
    else bigIntGaps(properEnd.get, end, currentSet + (properStart -> properEnd))
  }
}
object Util extends Util