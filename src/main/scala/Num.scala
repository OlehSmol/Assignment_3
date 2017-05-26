class Num(n: Int) extends Expr{
  override def isNumber: Boolean = true
  override def numValue: Int = n
}
