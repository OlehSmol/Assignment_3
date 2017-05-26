class Var(name: String) extends Expr{
  override def isVar: Boolean = true
  override def varName: String = name
}