trait Expr {
  def isReducible: Boolean = false
}

case class Number(n: Int) extends Expr{
  override def toString: String = n.toString
}
case class Bool(bool: Boolean) extends Expr{
  override def toString: String = bool.toString
}
case class Var(name: String) extends Expr{
  override def isReducible = true
  override def toString: String = name
}
case class Sum(lOp: Expr, rOp: Expr) extends Expr{
  override def isReducible = true
  override def toString: String = "(" + lOp + "+" + rOp + ")"
}
case class Product(lOp: Expr, rOp: Expr) extends Expr{
  override def isReducible = true
  override def toString: String = lOp + "*" + rOp
}
case class Less(lOp: Expr, rOp: Expr) extends Expr{
  override def isReducible = true
  override def toString: String = "(" + lOp + "<" + rOp + ")"
}
case class Conditional(condExp: Expr, ifExpr: Expr, elseExpr: Expr) extends Expr{
  override def isReducible = true
  override def toString: String = "((" + condExp + ") ? (" + ifExpr + ") : (" + elseExpr + "))"
}
case class Error(name: String) extends Expr{
  override def toString: String = name
}

trait Stat{
  def isReducible: Boolean = true
}

case class DoNothing() extends Stat{
  override def toString: String = ""
}

case class Assign(varn: String, expr: Expr) extends Stat{
  override def toString: String = "(" + varn + "=" + expr + ")"
}

case class IfElse(condExp: Expr, ifStat: Stat, elseStat: Stat) extends Stat{
  override def toString: String = "if( " + condExp + ") then {" + ifStat + "} else {" + elseStat + "}"
}

case class Seq(stat: Stat*) extends Stat{
  override def toString: String = stat.head + "; " + stat.tail
}

case class While(condExp: Expr, stat: Stat) extends Stat{
  override def toString: String = "while( " + condExp + ") do {" + stat + "}" ;
}