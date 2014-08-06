package opsemantic

object SmallStep {

  /**
   * The environment is the place where variable values are stored
   * Some reducible expression and/or statement involving variables
   * must be reduced on a given environment
   */
  type Environment = Map[String, Expression]

  /**
   * Expresions have a value
   */
  trait Expression {
    def isReducible: Boolean
    def reduce(env: Environment = Map()): Expression
    def value: Any = this
  }

  case class Number(n: Int) extends Expression {
    def isReducible = false
    def reduce(env: Environment) = this
    override def value: Int = n
    override def toString = n.toString
  }

  case class Add(left: Expression, rigth: Expression) extends Expression {
    def isReducible = true
    def reduce(env: Environment) = if (left.isReducible) Add(left.reduce(env), rigth)
    else if (rigth.isReducible) Add(left, rigth.reduce(env))
    else Number(left.asInstanceOf[Number].value + rigth.asInstanceOf[Number].value)
    override def toString = left.toString + " + " + rigth.toString

  }

  case class Multiply(left: Expression, rigth: Expression) extends Expression {
    def isReducible = true
    def reduce(env: Environment) = if (left.isReducible) Multiply(left.reduce(env), rigth)
    else if (rigth.isReducible) Multiply(left, rigth.reduce(env))
    else Number(left.asInstanceOf[Number].value * rigth.asInstanceOf[Number].value)
    override def toString = left.toString + " * " + rigth.toString
  }

  case class MBoolean(p: Boolean) extends Expression {
    def isReducible = false
    def reduce(env: Environment) = this
    override def value = p
    override def toString = p.toString
  }

  case class LessThan(left: Expression, rigth: Expression) extends Expression {
    def isReducible = true
    def reduce(env: Environment) = if (left.isReducible) LessThan(left.reduce(env), rigth)
    else if (rigth.isReducible) LessThan(left, rigth.reduce(env))
    else MBoolean(left.asInstanceOf[Number].value < rigth.asInstanceOf[Number].value)
    override def toString = left.toString + " < " + rigth.toString
  }

  case class GreaterThan(left: Expression, rigth: Expression) extends Expression {
    def isReducible = true
    def reduce(env: Environment) = if (left.isReducible) GreaterThan(left.reduce(env), rigth)
    else if (rigth.isReducible) GreaterThan(left, rigth.reduce(env))
    else MBoolean(left.asInstanceOf[Number].value > rigth.asInstanceOf[Number].value)
    override def toString = left.toString + " < " + rigth.toString
  }

  case class Variable(name: String) extends Expression{
    def isReducible = true
    def reduce(env: Environment) = env(this.name)
    override def toString = name
  }

  /**
   * A Statement hasn't value, but can be reduced
   * and while reducing the environment will be changed
   * (side effects)
   */
  trait Statement {
    def isReducible: Boolean
    def reduce(env: Environment = Map()): (Statement, Environment)
  }

  /**
   * Every statement will be reduce after a finite number
   * of steps to this one
   */
  case class DoNothing() extends Statement{
    def isReducible = false
    def reduce(env: Environment) = (this, env)
    def ==(st: Statement) = st.isInstanceOf[DoNothing]
    override def toString = "noop"
  }

  case class Assign(name: String, expr: Expression) extends Statement{
    def isReducible = true
    def reduce(env: Environment) : (Statement, Environment) =
      if(expr.isReducible) (Assign(name, expr.reduce(env)), env)
      else {  (DoNothing(), merge(env, Map(name -> expr.asInstanceOf[Number])))  }
    override def toString = name + " = " + expr.toString
  }

  case class If(condition: Expression, consecuence: Statement, alternative: Statement) extends Statement{
    def isReducible = true
    def reduce(env: Environment) : (Statement, Environment) =
      if(condition.isReducible) (If(condition.reduce(env), consecuence, alternative), env)
      else if(condition.value == true) (consecuence, env)
      else (alternative, env)
    override def toString = "if (" + condition.toString + ") then (" + consecuence.toString + ") else (" + alternative.toString + ")"
  }

  case class Sequence(first: Statement, second: Statement) extends Statement{
    def isReducible = true
    def reduce(env: Environment): (Statement, Environment) =
      if(first.isReducible) {var pair = first.reduce(env) ; (Sequence(pair._1, second), pair._2)}
      else {second.reduce(env)}
    override def toString = first.toString + " ; " + second.toString
  }

  case class While(condition: Expression, body: Statement) extends Statement{
    def isReducible = true
    def reduce(env: Environment): (Statement, Environment) =
      (If(condition, Sequence(body, this), DoNothing()), env)
    override def toString = "while (" + condition + ") {" + body + "}"
  }

  /**
   * This machine reduce expressions
   */
  object Machine {
    def run(e: Expression)(env: Environment = Map()): Expression = if (!e.isReducible) e
      else {
        println(e); run(e.reduce(env))(env)
      }
  }

  /**
   * This machine reduce statements. It is the right one able to
   * run a sequence of statement, that is a program
   * @param st
   * @param env
   */
  case class MachineStatements(st: Statement, env: Environment) {
    var machine: (Statement, Environment) = (st, env)
    def step: (Statement, Environment) =
      if(machine._1.isReducible) machine._1.reduce(machine._2)
      else (DoNothing(), machine._2)
    def run = {
      while (machine._1.isReducible){
        machine = { println(machine._1, machine._2) ; step }
      }
      println(machine._1, machine._2)
    }
  }

  def merge(env1: Environment, env2: Environment) = env1 ++ env2.map{ case (k,v) => k -> v }
}

