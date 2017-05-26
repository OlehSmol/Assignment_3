class Machine {
  def run(expr: Expr): Expr = {
    println(expr);
    if(expr.isReduciable)
      run(reductionStep(expr))
    else
    expr
  }

  def reductionStep(expr: Expr): Expr = {
    if(expr.isReduciable)
      expr
    else if(expr.isSum){
      new Num(reductionStep(expr.firstOp).numValue + reductionStep(expr.secondOp).numValue)
    }
    else if(expr.isProduct){
      new Num(reductionStep(expr.firstOp).numValue*reductionStep(expr.secondOp).numValue)
    }
    else if(expr.isLess){
      new Bool(reductionStep(expr.firstOp).numValue<reductionStep(expr.secondOp).numValue)
    }
    else if(expr.isLess){
      new Bool(reductionStep(expr.firstOp).numValue<reductionStep(expr.secondOp).numValue)
    }
    else
      throw new Error(s"Unrecognised expression $expr.")
  }
}
