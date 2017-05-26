trait Expr {
  def isNumber: Boolean = false
  def isBoolean: Boolean = false
  def isSum: Boolean = false
  def isProduct: Boolean = false
  def isLess: Boolean = false
  def isVar: Boolean = false
  def isConditional: Boolean = false
  def isReduciable: Boolean = false
  def numValue: Int = throw new Error("It`s not number value")
  def boolValue: Boolean = throw new Error("It`s not boolean value")
  def varName: String = throw new Error("It`s not variable")
  def firstOp: Expr = throw new Error("It hasn`t first operator")
  def secondOp: Expr = throw new Error("It hasn`t second operator")
}