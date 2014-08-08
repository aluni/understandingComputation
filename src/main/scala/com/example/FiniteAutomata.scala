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

    val nfaRuleBook: FA.NFARuleBook = Map(
      (1,'a') -> Set(1),
      (1,'b') -> Set(1,2),
      (2,'a') -> Set(3),
      (2,'b') -> Set(3),
      (3,'a') -> Set(4),
      (3,'b') -> Set(4)
    )

    val nfa = FA.NFA(Set(1), Set(4), nfaRuleBook)
    println({nfa.readCharacter('b') ; nfa.isAccepted })
    println({nfa.readCharacter('a') ; nfa.isAccepted })
    println({nfa.readCharacter('b') ; nfa.isAccepted })

    val nfaDesign = FA.NFADesign(1, Set(4), nfaRuleBook)
    println("nfaDesign")
    println(nfaDesign.isAccepted("bab"))
    println(nfaDesign.isAccepted("bbbbb"))
    println(nfaDesign.isAccepted("bbaabb"))

    val nfaRuleBool2: FA.NFARuleBook = Map(
      (1,'-') -> Set(2,4),
      (2,'a') -> Set(3),
      (3,'a') -> Set(2),
      (4,'a') -> Set(5),
      (5,'a') -> Set(6),
      (6,'a') -> Set(4)
    )

    val nfa2 =FA.NFA(Set(1), Set(2,4), nfaRuleBool2)

    println(nfa2.followFreeMoves(Set(1)))
  }
}
