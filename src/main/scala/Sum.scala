class Sum(first: Expr, second: Expr) extends Expr{
  override def isSum: Boolean = true
  override def firstOp: Expr = first
  override def secondOp: Expr = second
}
