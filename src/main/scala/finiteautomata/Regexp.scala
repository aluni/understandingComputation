package finiteautomata

import finiteautomata.FA.NFADesign

object Regexp {

  trait Pattern{
    val precedence: Int = 3
    val str = toString
    def toNFADesign: FA.NFADesign
    def matches(str: String) = toNFADesign.isAccepted(str)
    def bracket(outerPrecedence: Int): String = {
      if (precedence < outerPrecedence) "(" + str + ")"
      else str
    }
  }

  case class Empty() extends Pattern{
    override def toString = "e"
    def toNFADesign = {
      val startState: FA.State = new Object
      val acceptStates = Set(startState)
      val ruleBook: FA.NFARuleBook = Map()

      FA.NFADesign(startState, acceptStates, ruleBook)
    }
  }

  case class Literal(character: Char) extends Pattern {
    override def toString = character.toString
    def toNFADesign = {
      val startState = new Object
      val acceptState = new Object
      val ruleBook: FA.NFARuleBook = Map(
        (startState, character) -> Set(acceptState)
      )
      NFADesign(startState, Set(acceptState), ruleBook)
    }
  }

  case class Concatenate(first: Pattern, second: Pattern) extends Pattern {
    override val precedence = 1
    override def toString = List(first, second).map (p => p.bracket(precedence)).mkString("")
    def toNFADesign = {
      val firstNFADesign = first.toNFADesign
      val secondNFADesign = second.toNFADesign

      val startState = firstNFADesign.startState
      val acceptStates = secondNFADesign.acceptStates

      val rules = firstNFADesign.ruleBook ++ secondNFADesign.ruleBook
      val extraRules = firstNFADesign.acceptStates.map (st => (st, '-') -> Set(secondNFADesign.startState))

      val ruleBook: FA.NFARuleBook = rules ++ extraRules.toMap

      NFADesign(startState, acceptStates, ruleBook)

    }
  }

  case class Choose(first: Pattern, second: Pattern) extends Pattern {
    override val precedence = 0
    override def toString = List(first, second).map(p => p.bracket(precedence)).mkString("|")
    def toNFADesign = {
      val firstNFADesign = first.toNFADesign
      val secondNFADesign = second.toNFADesign
      val startState = new Object
      val acceptStates = firstNFADesign.acceptStates ++ secondNFADesign.acceptStates
      val rules = firstNFADesign.ruleBook ++ secondNFADesign.ruleBook
      val extraRules = Map(
        (startState, '-') -> Set(firstNFADesign.startState, secondNFADesign.startState)
      )
      val ruleBook: FA.NFARuleBook = rules ++ extraRules.toMap

      NFADesign(startState, acceptStates, ruleBook)

    }

  }

  case class Repeat(first: Pattern) extends Pattern {
    override val precedence = 2
    override def toString = first.bracket(precedence) + "*"
    def toNFADesign = {
      val firstNFADesign = first.toNFADesign
      val startState = new Object
      val acceptStates = firstNFADesign.acceptStates ++ Set(startState)
      val rules = firstNFADesign.ruleBook
      val extraRules = (acceptStates.map (st => (st, '-') -> Set(firstNFADesign.startState))).toMap  ++  Map((startState, '-') -> Set(firstNFADesign.startState))

      val ruleBook: FA.NFARuleBook = rules ++ extraRules

      NFADesign(startState, acceptStates, ruleBook)
    }
  }
}
