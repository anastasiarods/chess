package solution


import akka.actor.Actor
import akka.actor.{Actor, ActorRef, Terminated}
import chess._
import chess.ChessGame._

import scala.concurrent.Future

/**
  * Вспомогательный объект для организации рекурсии.
  */
object Solver {
  case class Resume(solutions: List[List[(Int, Piece)]], boards: List[ChessBoard])
}

/**
  * Actor, рекурсивно решающий в разных потоках задачу
  * @param chessGame
  */
class Solver(val chessGame: ChessGame) extends Actor {

  import context.dispatcher
  import Solver._

  //глобальные переменные - максимальное число активных future
  val MaxFutures = System.getProperty("max.futures", "3").toInt
  //максимальное число досок в future
  val BoardsInFuture = System.getProperty("boards.in.future", "10000").toInt

  var allSolutions = List[List[(Int, Piece)]]()
  var stack = List[ChessBoard]()
  var active = 1

  lazy val main = sender()

  /**
    * Метод, получающий информацию об имеющихся решениях.
    * Если еще не все шахматные доски решены, вызывает функцию для их решение в новом потоке.
    * @return
    */
  def receive = {
    case Resume(solutions, boards) =>
      main
      allSolutions :::= solutions
      stack :::= boards
      active -= 1

      if (active == 0 && stack.isEmpty) {
        main ! allSolutions
        context.system.terminate() //завершение вычислений
      } else
        goFutures()
  }

  /**
    * Метод, создающий новый объект future, в котором рекурсивно решается задача.
    */
  def goFutures() = while (active < MaxFutures && stack.nonEmpty) {
    val (boards, tail) = stack.splitAt(BoardsInFuture)
    stack = tail

    //создание новой future
    active += 1
    Future {
      //разбиение списка шахматных досок на решенные и не решенные(если piecesLeft пусто)
      val (solved, unsolved) = boards.partition(_.piecesLeft.isEmpty)

      //продолжение рекурсии
      self ! Resume(solved.map(_.piecesOnBoard), unsolved.flatMap(_.withPiece))
    }
  }
}
