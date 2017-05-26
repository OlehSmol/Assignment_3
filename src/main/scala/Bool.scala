class Bool(b: Boolean) extends Expr{
  override def isBoolean: Boolean = true
  override def boolValue: Boolean = b
}
