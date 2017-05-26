class Product(first: Expr, second: Expr) extends Expr{
  override def isProduct: Boolean = true
  override def firstOp: Expr = first
  override def secondOp: Expr = second
}
