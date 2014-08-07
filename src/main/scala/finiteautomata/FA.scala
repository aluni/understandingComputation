package finiteautomata

object FA {

  type State = Int

  type Rule = (State, Char) => State

  type RuleBook = Map[(State, Char) , State]

  case class DFA(var currentState: State, acceptStates: Set[State], ruleBook: RuleBook){
    def isAccepted = acceptStates.contains(currentState)
    def readCharacter(character: Char) {
      currentState = ruleBook((currentState, character))
    }
    def readString(str: String) {
      str.foreach(c => readCharacter(c))
    }
  }

  case class DFADesign(startState: State, acceptStates: Set[State], ruleBook: RuleBook){
    def toDFA = DFA(startState, acceptStates, ruleBook)
    def isAccepted(str: String) = { val dfa = toDFA; dfa.readString(str); dfa.isAccepted }
  }
}
