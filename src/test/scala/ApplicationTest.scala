
import org.scalatest.FreeSpec
import org.scalatest.FunSuite
import chess.ChessGame
import solution._

import akka.actor._
import scala.concurrent.Await
import akka.util.Timeout
import akka.pattern.ask
import solution.Solver.Resume
import scala.concurrent.duration._

/**
  * Created by argali on 6/23/17.
  */
class ApplicationTest extends FunSuite {

  def testChess(tests: (ChessGame, Int)) {
    val (chessGame, n) = tests
    val system = ActorSystem("chess")
    val solver = system.actorOf(Props(classOf[Solver], chessGame), "solver")
    implicit val timeout = Timeout(5.days)
    val result = solver ? Resume(Nil, chessGame.initialBoard :: Nil)
    assert(Await.result(result.mapTo[List[_]], timeout.duration).size == n)
  }

  test("Should have 92 solutions for 8×8 board containing 8 Queens "){
    val chessGame = ChessGame(8, 8, queens = 8)
    testChess(chessGame, 92)
  }

  test("Should have 2 solutions for 4×4 board containing 4 Queens "){
    val chessGame = ChessGame(4, 4, queens = 4)
    testChess(chessGame, 2)
  }

  test("Should have 4 solutions for  3×3 board containing 2 Kings and 1 Rook"){
    val chessGame = ChessGame(3, 3, kings = 2, rooks = 1)
    testChess(chessGame, 4)
  }

  test("Should have 8 solutions for 4x3 board containing 2 Knight, Kings, Rook and Bishop"){
    val chessGame = ChessGame(4, 3, knights = 2, kings =  1, rooks = 1, bishops = 1)
    testChess(chessGame, 8)
  }

}
