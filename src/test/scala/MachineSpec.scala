import collection.mutable.Stack
import org.scalatest._

class MachineSpec extends FlatSpec{

  /////////////////////////// EXPRESSIONS \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

  it should "Number does not reduce" in {
    assert(new Machine().reduce(Number(1), Map()) == Number(1))
  }

  it should "Bool does not reduce" in {
    assert(new Machine().reduce(Bool(true), Map()) == Bool(true))
  }

  it should "Number Var reduces to its value" in {
    assert(new Machine().reduce(Var("x"), Map("x" -> 1)) == Number(1))
  }
  it should "Bool Var reduces to its value" in {
    assert(new Machine().reduce(Var("x"), Map("x" -> true)) == Bool(true))
  }
  it should "Unknown Var does not reduce" in  {
    assert(new Machine().reduce(Var("x"), Map()) == Error("Var doesn`t exist"))
  }

  // SUM

  it should "Sum of two Numbers reduces to Number with their sum" in {
    assert(new Machine().reduce(Sum(Number(1), Number(2)), Map()) == Number(3))
  }

  it should "Sum of Number and Bool does not reduce" in {
    assert(new Machine().reduce(Sum(Number(1), Bool(true)), Map()) == Error("Sum should take (Number, Number)"))
  }

  it should "Sum of Bool and Number does not reduce" in {
    assert(new Machine().reduce(Sum(Bool(true), Number(1)), Map()) == Error("Sum should take (Number, Number)"))
  }

  it should "left Sum operand reduces if it is reducible and right is left unchanged" in {
    assert(new Machine().reductionStep(
      Sum(
        Sum(Number(1), Number(2)),
        Sum(Number(1), Number(2))
      ), Map()) ==
      Sum(
        Number(3),
        Sum(Number(1), Number(2))
      ))
  }

  it should "otherwise right Sum operand reduces" in {
    assert(new Machine().reductionStep(
      Sum(
        Number(3),
        Sum(Number(1), Number(2))
      ), Map()) ==
      Sum(
        Number(3),
        Number(3)
      ))
  }

  // PROD
  it should "Prod of two Numbers reduces to Number with their product" in {
    assert(new Machine().reduce(Product(Number(1), Number(2)), Map()) == Number(2))
  }
  it should "Prod of Number and Bool does not reduce" in {
    assert(new Machine().reduce(Product(Number(1), Bool(true)), Map()) == Error("Product should take (Number, Number)"))
  }
  it should "Prod of Bool and Number does not reduce" in {
    assert(new Machine().reduce(Product(Bool(true), Number(1)), Map()) == Error("Product should take (Number, Number)"))
  }
  it should "left Prod operand reduces if it is reducible and right is left unchanged" in {
    assert(new Machine().reductionStep(
      Product(
        Sum(Number(1), Number(2)),
        Sum(Number(1), Number(2))
      ), Map()) ==
      Product(
        Number(3),
        Sum(Number(1), Number(2))
      ))
  }
  it should "otherwise right Prod operand reduces" in {
    assert(new Machine().reductionStep(
      Product(
        Number(3),
        Sum(Number(1), Number(2))
      ), Map()) ==
      Product(
        Number(3),
        Number(3)
      ))
  }

  // LESS
  it should "Less of two Numbers reduces to Bool indicating whether first number is less than the second" in {
    assert(new Machine().reduce(
      Less(
        Number(1),
        Number(2)
      ), Map()) ==
      Bool(true))
  }
  it should "Less of Number and Bool does not reduce" in {
    assert(new Machine().reduce(Less(Number(1), Bool(true)), Map()) == Error("Less should take (Number, Number)"))
  }
  it should "Less of Bool and Number does not reduce" in {
    assert(new Machine().reduce(Less(Bool(true), Number(1)), Map()) == Error("Less should take (Number, Number)"))
  }
  it should "left Less operand reduces if it is reducible and right is left unchanged" in {
    assert(new Machine().reductionStep(
      Less(
        Sum(Number(1), Number(2)),
        Number(2)
      ), Map()) ==
      Less(
        Number(3),
        Number(2)
      ))
  }
  it should "otherwise right Less operand reduces" in {
    assert(new Machine().reductionStep(
      Less(
        Number(2),
        Sum(Number(1), Number(2))
      ), Map()) ==
      Less(
        Number(2),
        Number(3)
      ))
  }

  // IfElse
  it should "IfElse reduces to thenExpr for Bool(true) condition" in {
    assert(new Machine().reduce(
      Conditional(
        Bool(true),
        Number(1),
        Number(2)
      ), Map()) ==
      Number(1))
  }
  it should "IfElse reduces to elseExpr for Bool(false) condition" in {
    assert(new Machine().reduce(
      Conditional(
        Bool(false),
        Number(1),
        Number(2)
      ), Map()) ==
      Number(2))
  }
  it should "IfElse for Number condition does not reduce" in {

    assert(new Machine().reduce(
        Conditional(
          Number(1),
          Number(1),
          Number(2)
        ), Map()) ==
      Error("Conditional should take (Bool, Expr, Expr)")
    )
  }
  it should "IfElse for reducible condition reduces its condition" in {
    assert(new Machine().reduce(
      Conditional(
        Less(Number(1), Number(2)),
        Number(1),
        Number(2)
      ), Map()) ==
      Number(1))
  }

  /////////////////////////// STATEMENTS \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  it should "DoNothing does not alter environment" in {
    assert(new Machine().run(DoNothing(), Map()) == Map())
  }
  // Assign
  it should "Assign adds new variable for number expression" in {
    assert(new Machine().run(Assign("x", Number(1)), Map()) == Map("x" -> 1))
  }
  it should "Assign adds new variable for boolean expression" in {
    assert(new Machine().run(Assign("x", Bool(true)), Map()) == Map("x" -> true))
  }
  it should "Assign updates existing variable for number expression" in {
    assert(new Machine().run(Assign("x", Number(2)), Map("x" -> 1)) == Map("x" -> 2))
  }
  it should "Assign updates existing variable for boolean expression" in {
    assert(new Machine().run(Assign("x", Bool(false)), Map("x" -> true)) == Map("x" -> false))
  }
  it should "Assign updates existing variable for expression with the same variable" in {
    assert(new Machine().run(
      Assign("x", Sum(Var("x"), Number(1))),
      Map("x" -> 1)) ==
      Map("x" -> 2))
  }
  it should "Assign does not occur for erroneous expression" in {
    assert(new Machine().run(
      Assign("x", Sum(Bool(true), Number(1))),
      Map()) ==
      Map("___error" -> "Assignment failed"))
  }

  // If
  it should "'If' runs thenStat if condition is Bool(true)" in {
    assert(new Machine().run(
      IfElse(Bool(true), Assign("x", Number(1)), Assign("x", Number(2))),
      Map()) == Map("x" -> 1))
  }
  it should "'If' runs elseStat if condition is Bool(false)" in {
    assert(new Machine().run(
      IfElse(Bool(false), Assign("x", Number(1)), Assign("x", Number(2))),
      Map()) == Map("x" -> 2))
  }
  it should "'If' statement fails for erroneous condition" in {
    assert(new Machine().run(
      IfElse(Less(Number(1), Bool(true)), Assign("x", Number(1)), Assign("x", Number(2))),
      Map()) == Map("___error" -> "Condition failed"))
  }
  it should "'If' statement fails for condition expression that reduces to Number" in {
    assert(new Machine().run(
      IfElse(Number(1), Assign("x", Number(1)), Assign("x", Number(2))),
      Map()) == Map("___error" -> "Condition failed"))
  }

  // Seq
  it should "'Seq' does nothing if empty" in {
    assert(new Machine().run(Seq(), Map()) == Map())
  }
  it should "'Seq' executes one its statement if contains only one" in {
    assert(new Machine().run(
      Seq(Assign("x", Number(1))),
      Map()) ==
      Map("x" -> 1))
  }
  it should "'Seq' executes its statements one by one" in {
    assert(new Machine().run(
      Seq(Assign("x", Number(1)), Assign("x", Number(2))),
      Map()) ==
      Map("x" -> 2))
  }
  it should "'Seq' does not execute remained statements after first failure" in {
    assert(new Machine().run(
      Seq(Assign("x", Sum(Bool(true), Bool(true))),
        Assign("x", Number(3))),
      Map()) ==
      Map("___error" -> "Assignment failed"))
  }

  // While
  it should "'While' executes thenStat multiple times while condition reduces to Bool(true)" in {
    assert(new Machine().run(
      While(Less(Var("x"), Number(5)),
        Assign("x", Sum(Var("x"), Number(1)))),
      Map("x" -> 1)) ==
      Map("x" -> 5))
  }
  it should "'While' does not execute thenStat if condition reduces to Bool(false) from the start" in {
    assert(new Machine().run(
      While(Bool(false),
        Assign("x", Number(1))),
      Map()) ==
      Map())
  }
  it should "'While' statement fails for erroneous condition" in {
    assert(new Machine().run(
      While(Less(Bool(true), Number(1)),
        Assign("x", Number(1))),
      Map()) ==
      Map("___error" -> "Condition failed"))
  }
  it should "'While' statement fails for condition expression that reduces to Number" in {
    assert(new Machine().run(
      While(Number(1),
        Assign("x", Number(1))),
      Map()) ==
      Map("___error" -> "Condition failed"))
  }
  it should "'While' statement fails if thenStat statement fails" in {
    assert(new Machine().run(
      While(Bool(false),
        Assign("x", Less(Bool(true), Number(1)))),
      Map()) ==
      Map())

  }
}