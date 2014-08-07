package com.example

import opsemantic.SmallStep._

object OpSemantic {
  def main(args: Array[String]): Unit = {
    val add =Add(Multiply(Number(1), Number(2)), Multiply(Number(3), Number(4)))

    val result = Machine.run(add)()

    println("result = " + result)

    val lt = LessThan(add, Number(4))

    println("result = " + Machine.run(lt)())

    val gt = GreaterThan(add, Number(4))

    println("result = " + Machine.run(gt)())

    val addVars = Add(Variable("x"), Variable("y"))

    val env = Map("x" -> Number(3), "y" -> Number(5))
    println("result = " + Machine.run(addVars)(env))

    val assign = Assign("x", Add(Variable("x"), Number(1)))

    val machine = MachineStatements(assign, env)

    println("result = " + machine.run)

    val if_st = If(Variable("x"), Assign("y", Number(1)), Assign("y", Number(2)))

    val machine_if = MachineStatements(if_st, Map("x" -> MBoolean(true)))

    println("result = " + machine_if.run)

    val if_st2 = If(LessThan(Variable("x"), Number(2)) , Assign("y", Number(1)), Assign("y", Number(2)))

    val machine_if2 = MachineStatements(if_st2, env)

    println("result = " + machine_if2.run)

    val seq = Sequence(Assign("x", Number(4)), Assign("y", Add(Variable("x"), Number(5))))

    val machine_seq = MachineStatements(seq, Map("x" -> Number(4)))

    println("result = " + machine_seq.run)

    val while_seq = While(LessThan(Variable("x"), Number(5)),
        Assign("x", Multiply(Variable("x"), Number(3))))

    val machine_while = MachineStatements(while_seq, Map("x"-> Number(1)))

    println("result = " + machine_while.run)
  }
}
