package finiteautomata

import scala.util.{Failure, Success, Try}
import scala.util.control.Breaks._

object PDA {

  type Symbol = Char
  type PopSymbol = Char
  type State = Any
  type PushSymbols = List[Symbol]

  type RuleBook = Map[(State, Symbol, PopSymbol), (State, PushSymbols)]
  type NRuleBook = Map[(State, Symbol, PopSymbol), Set[(State, PushSymbols)]]

  case class Stack(contents: List[Symbol]) {
    def push(s: Symbol) = Stack(s :: contents)

    def pop = Stack(contents.drop(1))

    def top = contents.head

    override def toString = "Stack (" + top.toString + ")," + contents.drop(1).mkString(",")
  }

  case class Configuration(var state: State, var stack: Stack) {
    val stuckState = new Object

    def stuck = Configuration(stuckState, stack)

    def isStucked = state == stuckState

    override def toString = "[state: " + state + ", stack: " + stack + "]"
  }

  def nextConfiguration(conf: Configuration, s: Symbol)(ruleBook: RuleBook) = {
    val tryStateAndPushSymbols = Try(ruleBook(conf.state, s, conf.stack.top))
    tryStateAndPushSymbols match {
      case Success(stateAndPushSymbols) => {
        var stack = conf.stack.pop
        stateAndPushSymbols._2.reverse.foreach { pushSymbol =>
          stack = stack.push(pushSymbol)
        }
        Configuration(stateAndPushSymbols._1, stack)
      }
      case Failure(_) => conf.stuck
    }
  }

  case class DPDA(var currentConfig: PDA.Configuration, acceptStates: List[State], ruleBook: RuleBook) {
    def readSymbol(s: Symbol) {
      currentConfig = nextConfiguration(currentConfig, s)(ruleBook)
    }

    def readString(str: List[Symbol]) {
      for (s <- str.toList) {
        if (currentConfig.isStucked) break
        readSymbol(s)
        followFreeMoves
      }
    }

    def isAccepted = acceptStates.contains(currentConfig.state)

    def followFreeMoves = {
      val tryFreeMove = Try(ruleBook(currentConfig.state, '-', currentConfig.stack.top))
      tryFreeMove match {
        case Success(configAndPushSymbols) => readSymbol('-')
        case Failure(_) => {}
      }
    }
  }

  case class DPDADesign(startState: State, bottonSymbol: Symbol, acceptStates: List[State], ruleBook: RuleBook) {
    def isAccepted(str: List[Symbol]) = {
      val dpda = toDPDA
      dpda.readString(str)
      dpda.isAccepted
    }

    def toDPDA = {
      val startStack = Stack(List(bottonSymbol))
      val startConfig = Configuration(startState, startStack)
      DPDA(startConfig, acceptStates, ruleBook)
    }
  }

  def nextNConfigurations(conf: Configuration, s: Symbol)(ruleBook: NRuleBook): Set[Configuration] = {
    val trySetStateAndPushSymbols = Try(ruleBook(conf.state, s, conf.stack.top))
    trySetStateAndPushSymbols match {
      case Success(setStateAndPushSymbols) => {
        setStateAndPushSymbols.map { stAndp =>
          var stack = conf.stack.pop
          stAndp._2.reverse.foreach { pushSymbol =>
            stack = stack.push(pushSymbol)
          }
          Configuration(stAndp._1, stack)
        }

      }
      case Failure(_) => Set().asInstanceOf[Set[Configuration]]
    }
  }

  def nextConfigurations(confs: Set[Configuration], s: Symbol)(ruleBook: NRuleBook): Set[Configuration] = {
    confs.flatMap { c => nextNConfigurations(c, s)(ruleBook)}
  }

  case class NDPDA(var currentConfigs: Set[PDA.Configuration], acceptStates: Set[State], ruleBook: NRuleBook) {
    def readSymbol(s: Symbol) {
      currentConfigs = nextConfigurations(currentConfigs, s)(ruleBook)
      currentConfigs = followFreeMoves(currentConfigs)
    }

    def readString(str: List[Symbol]) {
      if (!currentConfigs.head.isStucked) {
        for (s <- str.toList) {
          readSymbol(s)
        }
      }
    }

    def isAccepted = acceptStates.subsetOf((currentConfigs.map(c => c.state)))

    def followFreeMoves(configs: Set[Configuration]): Set[Configuration] = {
      var moreConfigs = nextConfigurations(currentConfigs, '-')(ruleBook)
      if (moreConfigs.subsetOf(configs)) configs
      else followFreeMoves(configs ++ moreConfigs)
    }
  }

  case class NDPDADesign(startState: State, bottonSymbol: Symbol, acceptStates: Set[State], ruleBook: NRuleBook) {
    def isAccepted(str: List[Symbol]) = {
      val ndpda = toNDPDA
      ndpda.readString(str)
      ndpda.isAccepted
    }

    def toNDPDA = {
      val startStack = Stack(List(bottonSymbol))
      val startConfig = Configuration(startState, startStack)
      NDPDA(Set(startConfig), acceptStates, ruleBook)
    }
  }
}