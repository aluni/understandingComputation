package com.example

import finiteautomata._

object FiniteAutomata {
  def main(args: Array[String]): Unit = {

    val ruleBook: FA.RuleBook = Map(
      (1, 'a') -> 2,
      (1, 'b') -> 1,
      (2, 'a') -> 2,
      (2, 'b') -> 3,
      (3, 'a') -> 3,
      (3, 'b') -> 3
    )

    println(ruleBook((2, 'b')))

    val dfa = FA.DFA(1, Set(1, 3), ruleBook)

    println(dfa.isAccepted)

    val dfa2 = FA.DFA(1, Set(3), ruleBook)

    dfa2.readString("baaab")

    println(dfa2.isAccepted)

    val dfaDesign = FA.DFADesign(1, Set(3), ruleBook)

    println(dfaDesign.isAccepted("a"))
    println(dfaDesign.isAccepted("baa"))
    println(dfaDesign.isAccepted("baba"))

  }
}
