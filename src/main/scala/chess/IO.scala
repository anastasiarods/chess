package chess

import scala.io.StdIn

/**
  * Объект, представляющий интерфейс программы
  */
object IO {

  def input(): ChessGame = {
    print("Columns(x dimension): ")
    val columns = StdIn.readInt()
    print("Rows(y dimension):  ")
    val rows = StdIn.readInt()
    print("Number of Kings: ")
    val k = StdIn.readInt()
    print("Number of Queens: ")
    val q = StdIn.readInt()
    print("Number of Rooks: ")
    val r = StdIn.readInt()
    print("Number of Bishops: ")
    val b = StdIn.readInt()
    print("Number of Knights: ")
    val n = StdIn.readInt()
    ChessGame(columns, rows, k, q, r , b, n)
  }
  
  def output(result: => Traversable[_]) {
    val (r, t) = timeIt(result)
    val res = result.size
    val sec = t * 0.001
    println(f"Number of solutions: $res%d (time: $sec%.3f s)")
  }


  def timeIt[T](block: => T): (T, Long) = {
    val t0 = System.currentTimeMillis()
    val result = block
    val t1 = System.currentTimeMillis()
    (result, t1 - t0)
  }

}