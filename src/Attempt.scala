import javax.swing.plaf.nimbus.NimbusLookAndFeel

/**
 * Created by sgreene on 10/26/15.
 */
class Attempt(val goal: Int, val numbers: List[Int], permitted: String, ops: List[Operation]) {

  def solve(solutions: List[List[Operation]]): List[List[Operation]] = {

    // check if any of the numbers in the list is the goal
    this.solveOne match {

      // if it is, don't go any further
      case Some(solution) => solution :: solutions

      // if it's not, try all combinations
      case None           => this.solveMany ::: solutions
    }
  }

  // check if any of the numbers in the list is the goal - if so, return a solution
  def solveOne: Option[List[Operation]] = {
    // println("  Attempting to find " + goal + " using one of " + numbers)
    // println("  Prior operations: " + ops)
    numbers find(_ == goal) match {
      case Some(x) => ops match {
        case Nil      => Some(new Operation(x, 0, "=") :: ops)
        case x :: xs  => Some(ops)
      }
      case None => None
    }
  }

  def solveMany: List[List[Operation]] = {
    if (numbers.length < 2) { return Nil }

    var acc: List[List[Operation]] = Nil

    for ( left <- numbers ) {
      val remainLeft = numbers diff List(left)
      for ( right <- remainLeft ) {
        val remainLR = remainLeft diff List(right)
        if ( (left >= right) && (permitted contains "+") ) acc = acc ::: subsolve(left, right, remainLR, "+")
        if ( (left >= right) && (permitted contains "*") ) acc = acc ::: subsolve(left, right, remainLR, "*")
        if ( (left >  right) && (permitted contains "-") ) acc = acc ::: subsolve(left, right, remainLR, "-")
        if ( (left >  right) && (permitted contains "/") && (left % right == 0) ) acc = acc ::: subsolve(left, right, remainLR, "/")
      }
    }

    acc
  }

  def subsolve(l: Int, r: Int, numbers: List[Int], op: String): List[List[Operation]] = {
    // println("subsolve " + l + ", " + r + ", " + op)
    val combine = new Operation(l, r, op)
    val next = new Attempt(goal, combine.result :: numbers, permitted, combine :: ops)
    next.solve(Nil)
  }

}
