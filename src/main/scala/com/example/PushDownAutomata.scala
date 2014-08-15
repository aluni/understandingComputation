package com.example

import finiteautomata._

object PushDownAutomata {
  def main(args: Array[String]): Unit = {
    val stack = PDA.Stack(List('a','b','c','d','e'))

    println(stack)

    println(stack.top)

    println(stack.pop.pop.top)

    println(stack.push('x').push('y').top)

    println(stack.push('x').push('y').pop.top)

    val config = PDA.Configuration(1, PDA.Stack(List('$')))

    val ruleBook: PDA.RuleBook =  Map(
      (1, '(', '$') -> (2, List('b', '$')),
      (2, '(', 'b') -> (2, List('b', 'b')),
      (2, ')', 'b') -> (2, List()),
      (2, '-', '$') -> (1, List('$'))
    )

    val dpda = PDA.DPDA(config,List(1),ruleBook)

    println(dpda.isAccepted)

    dpda.readString("(()".toList)

    println(dpda.isAccepted)

    println(dpda.currentConfig)

    val dpdaDesign =  PDA.DPDADesign(1, '$', List(1), ruleBook)

    println("----------")
    println(dpdaDesign.isAccepted("()".toList))
    println(dpdaDesign.isAccepted("(((((((())))))))".toList))
    println(dpdaDesign.isAccepted("()(())((((()))))()".toList))
    println(dpdaDesign.isAccepted("(()()()()((())))))".toList))
    println(dpdaDesign.isAccepted("(())())(())".toList))

    val ruleBookPalindromo: PDA.RuleBook = Map(
      (1,'a','$') -> (1, List('a','$')),
      (1,'a','a') -> (1, List('a','a')),
      (1,'a','b') -> (1, List('a','b')),
      (1,'b','$') -> (1, List('b','$')),
      (1,'b','a') -> (1, List('b','a')),
      (1,'b','b') -> (1, List('b','b')),
      (1,'m','$') -> (2, List('$')),
      (1,'m','a') -> (2, List('a')),
      (1,'m','b') -> (2, List('b')),
      (2,'a','a') -> (2, List()),
      (2,'b','b') -> (2, List()),
      (2,'-','$') -> (3, List('$'))
    )

    val dpdaPalindromo = PDA.DPDADesign(1,'$',List(3),ruleBookPalindromo)

    println("Palindromo 1, con caracter central")
    println(dpdaPalindromo.isAccepted("abbamabba".toList))
    println(dpdaPalindromo.isAccepted("ababmbaba".toList))
    println(dpdaPalindromo.isAccepted("abbaabba".toList))


    val ruleBookPalindromo2: PDA.NRuleBook = Map(
      (1,'a','$') -> Set((1, List('a','$'))),
      (1,'a','a') -> Set((1, List('a','a'))),
      (1,'a','b') -> Set((1, List('a','b'))),
      (1,'b','$') -> Set((1, List('b','$'))),
      (1,'b','a') -> Set((1, List('b','a'))),
      (1,'b','b') -> Set((1, List('b','b'))),
      (1,'-','$') -> Set((2, List('$'))),
      (1,'-','a') -> Set((2, List('a'))),
      (1,'-','b') -> Set((2, List('b'))),
      (2,'a','a') -> Set((2, List())),
      (2,'b','b') -> Set((2, List())),
      (2,'-','$') -> Set((3, List('$')))
    )

    val dpdaPalindromo2 = PDA.NDPDADesign(1,'$',Set(3),ruleBookPalindromo2)

    println("Palindromo 2, sin caracter central")
    println(dpdaPalindromo2.isAccepted("a".toList))
    println(dpdaPalindromo2.isAccepted("abba".toList))
    println(dpdaPalindromo2.isAccepted("abbbbbbaa".toList))
    println(dpdaPalindromo2.isAccepted("aa".toList))
    println(dpdaPalindromo2.isAccepted("ababbaba".toList))
    println(dpdaPalindromo2.isAccepted("abbaabba".toList))
    println(dpdaPalindromo2.isAccepted("abbaaaaabba".toList))
    println(dpdaPalindromo2.isAccepted("abbaaaaaabba".toList))

  }
}
