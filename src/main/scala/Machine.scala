final class Machine {
  def lastEnv: Map[String, Any] = Map()

  def run(stat: Stat, env: Map[String, Any]): Map[String, Any]  = {
    stat match {
      case DoNothing() => env
      case Assign(varn: String, expr: Expr) => reduce(expr, env) match {
          case i: Number => env + (varn -> i.n)
          case b: Bool => env + (varn -> b.bool)
          case default => env + ("___error" -> "Assignment failed")
        }
      case IfElse(condExp: Expr, ifStat: Stat, elseStat: Stat) => reduce(condExp, env) match {
          case b: Bool => if (b.bool) {
              run(ifStat, env)
            } else {
              run(elseStat, env)
            }
          case default => env + ("___error" -> "Condition failed")
        }
      case Seq(stat@_*) =>
        if(stat.isEmpty)
          env
        else stat.head match {
          case s: Stat => {
            def newEnv = run(s, env)
            if(newEnv.contains("___error")){
              newEnv
            } else
              run(Seq(stat.tail: _*), newEnv)
          }
          case default: Stat => env + ("___error" -> "Seq failed")
        }

      case While(condExp: Expr, stat: Stat) => reduce(condExp, env) match {
        case b: Bool => if (b.bool) {
            def newEnv = run(stat, env)

            if (newEnv != env) {
              run(While(condExp, stat), newEnv)
            } else {
              env + ("___error" -> "Endless loop")
            }
          } else {
            env
          }
        }
        case default => env + ("___error" -> "Condition failed")
    }
  }

  def reduce(expr: Expr, env: Map[String, Any]): Expr = {
    println(expr)

    if (expr.isReducible)
      reduce(reductionStep(expr, env), env)
    else
      expr
  }

  def reductionStep(expr: Expr, env: Map[String, Any]): Expr = {
    expr match {
      case Var(op : String) => {
        if(env.contains(op)){
            env(op) match {
              case i:Int => Number(i);
              case b:Boolean => Bool(b);
              case default => Error("Var wrong format")
            }
        } else {
          Error("Var doesn`t exist")
        }
      }
      case Sum(lOp, rOp) => if (lOp.isReducible)
          Sum(reductionStep(lOp, env), rOp)
        else if (rOp.isReducible)
          Sum(lOp, reductionStep(rOp, env))
        else {
          (lOp, rOp) match {
            case (a: Number, b: Number) => Number(a.n + b.n)
            case default => Error("Sum should take (Number, Number)")
          }
        }
      case Product(lOp, rOp) => if (lOp.isReducible)
          Product(reductionStep(lOp, env), rOp)
        else if (rOp.isReducible)
          Product(lOp, reductionStep(rOp, env))
        else {
          (lOp, rOp) match {
            case (a: Number, b: Number) => Number(a.n * b.n)
            case default => Error("Product should take (Number, Number)")
          }
        }
      case Less(lOp, rOp) => if (lOp.isReducible)
          Less(reductionStep(lOp, env), rOp)
        else if (rOp.isReducible)
          Less(lOp, reductionStep(rOp, env))
        else {
          (lOp, rOp) match {
            case (a: Number, b: Number) => Bool(a.n < b.n)
            case default => Error("Less should take (Number, Number)")
          }
        }
      case Conditional(stOpt, ifOpt, elseOpt) => if (stOpt.isReducible)
          Conditional(reductionStep(stOpt, env), ifOpt, elseOpt)
        else {
          stOpt match {
            case b: Bool => if(b.bool){
                if(ifOpt.isReducible)
                  reductionStep(ifOpt, env)
                else
                  ifOpt
              }
              else{
                if(elseOpt.isReducible)
                  reductionStep(elseOpt, env)
                else
                  elseOpt
              }

            case default => Error("Conditional should take (Bool, Expr, Expr)")
          }
        }
      case default => Error("Wrong command")
    }
  }
}
