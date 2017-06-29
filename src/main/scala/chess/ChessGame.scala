package chess

/**
  * Объект, содержащий описание шахматной доски и фигур.
  */

object ChessGame {

  /**
    * Класс, представляющий точку на шмахматной доске
    * 
    * @param x
    * @param y
    */
  case class Point(x: Int, y: Int)

  /**
    * Класс, для представления размерности шахматного поля.
    * 
    * @param x
    * @param y
    */
  case class Dim(x: Int, y: Int) {
    val size = x * y
    val allIndices = 0.until(size).toList
  }

  /**
    * Интерфейс шахматной фигуры.
    */
  sealed trait Piece {
    /**
      * Функция, проверяющая может ли аттаковать одна фигура другую.
      * 
      * @param from
      * @param to
      * @return
      */
    def attacks(from: Point, to: Point): Boolean

    /**
      * Вспомогательная функция, проверяющая стоят ли две фигуры на одной прямой линии
      * .
      * @param from
      * @param to
      * @return
      */
    def onStraightLine(from: Point, to: Point) = from.x == to.x || from.y == to.y

    /**
      * Вспомогательная функция, проверяющая стоят ли две фигуры на одной диагонали.
      * 
      * @param from
      * @param to
      * @return
      */
    def onDiagonal(from: Point, to: Point) = (from.x - to.x).abs == (from.y - to.y).abs
  }

  /**
    * Case объекты для каждого вида шахматных фигур.
    */
  case object Rook extends Piece {
    def attacks(from: Point, to: Point): Boolean = onStraightLine(from, to)
  }

  case object Bishop extends Piece {
    def attacks(from: Point, to: Point): Boolean = onDiagonal(from, to)
  }

  case object Queen extends Piece {
    def attacks(from: Point, to: Point): Boolean =
      onStraightLine(from,to) || onDiagonal(from,to)
  }

  case object King extends Piece {
    def attacks(from: Point, to: Point): Boolean =
      (from.x - to.x).abs <= 1 && (from.y - to.y).abs <= 1
  }

  case object Knight extends Piece {
    def attacks(from: Point, to: Point): Boolean = {
      val a = (from.x - to.x).abs
      val b = (from.y - to.y).abs
      a + b == 3 && a > 0 && b > 0
    }
  }


  /**
    * Класс, представляющий шахматное поле
    *
    * @param piecesOnBoard расположенные фигуры
    * @param piecesLeft фигуры, которые надо расположить
    * @param safeIndices безопасные клетки
    * @param dimension размер доски
    */
  case class ChessBoard(piecesOnBoard: List[(Int, Piece)], piecesLeft: List[Piece], safeIndices: List[Int])(implicit dimension: Dim) {

    /**
      * Расположение фигур из piecesLeft на шахматной доске
      * @return
      */
    def withPiece: List[ChessBoard] = {
      var boards = List[ChessBoard]()
      val onePiece :: nextPieces = piecesLeft
      var remIndices = safeIndices

      while(remIndices.nonEmpty) {
        val point = toPoint(remIndices.head)
        //если точка безопасна
        if(!piecesOnBoard.exists(pp => onePiece.attacks(point, toPoint(pp._1)))) {
          val nextIndices =
            (if (indicesFree.nonEmpty) indicesFree else remIndices.tail)
              .filterNot(i => i == remIndices.head || onePiece.attacks(point, toPoint(i)))

          //Добавляем фигуру, создавая новую доску
          val newBoard = ChessBoard((remIndices.head, onePiece) :: piecesOnBoard, nextPieces, nextIndices)
          boards = newBoard :: boards
        }
        remIndices = remIndices.tail
      }
      boards
    }

    /**
      * Вспомогательная переменная, возващает безопасные клетки на доске.
      */
    private lazy val indicesFree: List[Int] = piecesLeft match {
      case piece1 :: piece2 :: _ if piece1 != piece2 =>
        dimension.allIndices.filterNot(index => piecesOnBoard.exists {
          case (i, p) => index == i || p.attacks(toPoint(i), toPoint(index))
        })
      case _ => Nil
    }

    /**
      * Преобразование порядкового номера в списке в точку на шхматной доске
      * 
      * @param index индекс в списке
      * @return координаты точки
      */
    private def toPoint(index: Int): Point =
      Point(index / dimension.y, index % dimension.y)
  }
}


/**
  * Класс, для ссодания описание шахматной игры.
  *
  * @param rows
  * @param columns
  * @param kings
  * @param queens
  * @param rooks
  * @param bishops
  * @param knights
  */

case class ChessGame(rows: Int, columns: Int, kings: Int = 0,
                     queens: Int = 0, rooks: Int = 0, bishops: Int = 0, knights: Int = 0) {

  import ChessGame._

  implicit val dimension = Dim(rows, columns)

  //создание списка шахматных фигур
  val pieces = List (
    Queen -> queens,
    Rook -> rooks,
    Bishop -> bishops,
    Knight -> knights,
    King -> kings
  ).flatMap { case (p, n) => List.fill(n)(p)} //преобразование пар в список

  val initialBoard = ChessBoard(Nil, pieces, dimension.allIndices)
}