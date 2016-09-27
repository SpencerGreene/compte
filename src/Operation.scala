/**
 * Created by sgreene on 10/26/15.
 */
class Operation(val left: Int, val right: Int, val op: String) {

  override def toString: String = {
    val expr = op match {
      case "+"  => left + " + " + right
      case "*"  => left + " × " + right
      case "-"  => left + " - " + right
      case "/"  => left + " / " + right
      case "="  => left + "."
    }
    expr + " = " + this.result
  }

  def result: Int = op match {
    case "+"  => left + right
    case "*"  => left * right
    case "-"  => left - right
    case "/"  => left / right
    case "="  => left
  }

  def isEqual(o: Operation): Boolean = {
    if (this.op != o.op) { return false }
    op match {
      case "+" | "*" =>
        (this.left == o.left && this.right == o.right) || (this.left == o.right && this.right == o.left)
      case "-" | "/" =>
        this.left == o.left && this.right == o.right
      case "=" =>
        this.left == o.left
    }
  }

  def ==(o: Operation): Boolean = this.isEqual(o)
  def equals(o: Operation): Boolean = this.isEqual(o)

  def !=(o: Operation): Boolean = !(this == o)

  def stdLeft: Int = op match {
    case "+" | "*" => if (left > right) left else right
    case "-" | "=" | "/" => left
  }

  def stdRight: Int = op match {
    case "+" | "*" => if (left > right) right else left
    case "-" | "=" | "/" => right
  }

  def compare(o: Operation): Int = {
    if (this == o) return(0)

    val resultOrder = this.result.compare(o.result)
    if (resultOrder != 0) { return resultOrder }

    val opOrder = this.op.compare(o.op)
    if (opOrder != 0) { return opOrder }

    val leftOrder = this.stdLeft.compare(o.stdLeft)
    if (leftOrder != 0) { return leftOrder }

    val rightOrder = this.stdRight.compare(o.stdRight)
    if (rightOrder != 0) { return rightOrder }

    assert(false, "Operation.compare: should not get here")
    0

  }

}
