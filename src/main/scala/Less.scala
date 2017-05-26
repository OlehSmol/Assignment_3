class Less(first:Expr, second:Expr) extends Expr{
  override def isLess: Boolean = true
  override def firstOp: Expr = first
  override def secondOp: Expr = second
}
