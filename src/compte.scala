/**
 * Created by sgreene on 10/26/15.
 */
object compte {
  def main(args: Array[String]) = {

    val numericArgs = if (args(0)=="/") args.drop(1) else args
    val permittedOperations = if (args(0)=="/") "+-*/" else "+-*"

    if (numericArgs.length < 2) usage

    val goal = numericArgs(0).toInt
    val numbers = numericArgs.drop(1).map(x => x.toInt).toList

    val a = new Attempt(goal, numbers, permittedOperations, Nil)
    val rawSolutions = a.solve(Nil)
    val solutions = uniquify(rawSolutions).sortWith(_.length < _.length)
    // val solutions = rawSolutions

    println("\nWays to compute " + goal + " with " + numbers + ":\n")
    for ( i <- solutions.indices ) {
      val solution = solutions(i).reverse
      printf("%3d)  ", i+1)
      for ( e <- solution.dropRight(1) ) print(e + ", ")
      println(solution.last)
    }
  }

  def usage(): Unit = {
    println("usage: compte <goal> <number1 number2 ...>")
    System.exit(1)
  }

  def uniquify(solutions: List[List[Operation]]): List[List[Operation]] = solutions match {
    case first :: rest => {
      val nullOp = nullOpMatch(first)
      if (nullOp) {
        return uniquify(rest)
      } else {
        val solMatch = rest find (solutionsMatch(_, first))   // test each element of rest against first
        if (solMatch.isDefined) return uniquify(rest)

        val effMatch = rest find (effectiveMatch(_, first))   // test each element of rest against first
        if (effMatch.isDefined) return uniquify(rest)

        first :: uniquify(rest)
      }

    }
    case Nil => Nil
  }

  def solutionsMatch(a: List[Operation], b: List[Operation]): Boolean = {
    if (a.length != b.length) { return false }

    val aSorted = a.sortWith(_.compare(_) > 0)
    val bSorted = b.sortWith(_.compare(_) > 0)

    for (i <- a.indices) if ( ! aSorted(i).isEqual(bSorted(i)) ) return false
    true
  }

  def effectiveMatch(a: List[Operation], b: List[Operation]): Boolean = {
    if (a.length != b.length) { return false }
    var i=a.length-1
    while (i >= 1) {
      if ( a(i).isEqual(b(i)) ) i = i-1
      else {
        if ( pairIsEquivalent(a(i), a(i-1), b(i), b(i-1) ) ) i = i-2
        else return false
      }
    }
    true
  }

  def nullOpMatch(a: List[Operation]): Boolean = {
    // a find (_, opIsNull())
    false
  }

  def pairIsEquivalent( a1: Operation, a2: Operation, b1: Operation, b2: Operation): Boolean = {

    // printf("testing [%s, %s] for equivalence to [%s, %s]\n", a1, a2, b1, b2)
    if (a1.left  != b1.left  || a2.result != b2.result ) return false
    if (a1.op    != b2.op    || a2.op     != b1.op     ) return false
    if (a1.right != b2.right || a2.right  != b1.right  ) return false

    true
  }
}
