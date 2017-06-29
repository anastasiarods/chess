package solution

import akka.actor._
import scala.concurrent.Await
import akka.util.Timeout
import akka.pattern.ask
import chess.IO
import solution.Solver.Resume
import scala.concurrent.duration._

object Application extends App {
  //чтение входных данных
  val chessGame = IO.input()

  //создание системы Акторов
  val system = ActorSystem("chess")

  //создание актора solver
  val solver = system.actorOf(Props(classOf[Solver], chessGame), "solver")
  
  implicit val timeout = Timeout(5.days) // needed for `?` below

  //Ask actor - получение результата от актора
  val result = solver ? Resume(Nil, chessGame.initialBoard :: Nil)

  //блокирование, вывод результата, mapTo - преобразование future to list
  IO.output(Await.result(result.mapTo[List[_]], timeout.duration))
}