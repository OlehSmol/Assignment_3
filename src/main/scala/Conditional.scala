class Conditional(first: Expr, second: Expr) extends Expr{
  override def isConditional: Boolean = true
  override def firstOp: Expr = first
  override def secondOp: Expr = second
}
