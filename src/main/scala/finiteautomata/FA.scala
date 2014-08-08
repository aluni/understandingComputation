package finiteautomata

import scala.util.{Try, Success, Failure}

object FA {

  type State = Int

  type Rule = (State, Char) => State

  type RuleBook = Map[(State, Char) , State]

  case class DFA(var currentState: State, acceptStates: Set[State], ruleBook: RuleBook){
    def nextState(character: Char) = ruleBook((currentState, character))
    def isAccepted = acceptStates.contains(currentState)
    def readCharacter(character: Char) {
      currentState = nextState(character)
    }
    def readString(str: String) {
      str.foreach(c => readCharacter(c))
    }
  }

  case class DFADesign(startState: State, acceptStates: Set[State], ruleBook: RuleBook){
    def toDFA = DFA(startState, acceptStates, ruleBook)
    def isAccepted(str: String) = { val dfa = toDFA; dfa.readString(str); dfa.isAccepted }
  }

  type NFARuleBook = Map[(State, Char), Set[State]]

  case class NFA(var currentStates: Set[State], acceptStates: Set[State], ruleBook: NFARuleBook){
    def nextStates(character: Char)(states: Set[State] = currentStates): Set[State] = {
      states.flatMap {
        st => {
          // In case there aren't any rule to change state
          // the empty set is returned. We must catch the
          // exception
          val tryNextStates = Try(ruleBook((st, character)))
          tryNextStates match {
            case Success(ns) => ns
            case Failure(ns) => Set().asInstanceOf[Set[State]]
          }
        }
      }
    }
    def followFreeMoves(states: Set[State]): Set[State] = {
      val moreStates = nextStates('-')(states)
      if(moreStates.subsetOf(states))
        states
      else
        followFreeMoves(moreStates ++ states)
    }

    def isAccepted = currentStates.map(st => acceptStates.contains(st)).contains(true)
    def readCharacter(character: Char) {
//      println("from: " + currentStates + " with " + character)
      currentStates = nextStates(character)()
//      println("to: " + currentStates + " with " + character)
    }
    def readString(str: String) {
      str.foreach(c => readCharacter(c))
    }
  }

  case class NFADesign(startState: State, acceptStates: Set[State], ruleBook: NFARuleBook){
    def toNFA = NFA(Set(startState), acceptStates, ruleBook)
    def isAccepted(str: String) = {val nfa = toNFA; nfa.readString(str) ; nfa.isAccepted}
  }

}
