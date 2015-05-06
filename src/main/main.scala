package main

import scala.reflect.ClassTag

/**
 * @author michal.lacko
 */
object Main {

  object Field extends Enumeration {
    val Empty, Player1, Player2 = Value
  }

  /*trait Field
  case class Empty() extends Field
  case class Player1() extends Field
  case class Player2() extends Field
*/
  class Board(val fields: Array[Array[Field.Value]]) {}

  val boardSize = 3
  val winSize = 3

  def InitializeBoard: Board = {
    new Board(Array.fill(boardSize, boardSize)(Field.Empty))
  }

  def AddMove(field: Field.Value, board: Board, x: Int, y: Int) = {
    
    var fields = board.fields.map { line => line.clone() }
    if (fields(x)(y) != Field.Empty)
      throw new IllegalArgumentException("x:" + x + " y:" + y + " dosnt contain an empty field (" + fields(x)(y) + ")");

    fields(x)(y) = field

    new Board(fields)
  }

  def FreeMoves(board: Board) = {

    for (
      x <- 0 to board.fields.length - 1;
      y <- 0 to board.fields(x).length - 1 if (board.fields(x)(y) == Field.Empty)
    ) yield (x, y)
  }

  def PlayerWon(player: Field.Value, board: Board): Boolean = {
    def WonHorizontal: Boolean = {
      for (y <- 0 to board.fields.length - 1) {
        var cur = 0
        for (x <- 0 to board.fields(y).length - 1) {
          board.fields(y)(x) match {
            case field if field == player => cur += 1
            case _                        => cur = 0
          }

          if (cur >= winSize) {
            return true
          }
        }
      }
      false
    }

    def WonVertical: Boolean = {
      for (y <- 0 to board.fields.length - 1) {
        var cur = 0
        for (x <- 0 to board.fields(y).length - 1) {
          board.fields(x)(y) match {
            case field if field == player => cur += 1
            case _                        => cur = 0
          }

          if (cur >= winSize) {
            return true
          }
        }
      }
      false
    }

    def WonDiag1: Boolean = {
      for (y <- 0 to board.fields.length - 1) {
        var cur = 0
        for (x <- 0 to board.fields(y).length - 1) {

          val yy = y + x

          if (yy < board.fields.length) {

            board.fields(x)(yy) match {
              case field if field == player => cur += 1
              case _                        => cur = 0
            }

            if (cur >= winSize) {
              return true
            }
          }
        }
      }
      false
    }

    def WonDiag2: Boolean = {
      for (y <- 0 to board.fields.length - 1) {
        var cur = 0
        for (x <- 0 to board.fields(y).length - 1) {

          val yy = y - x

          if (yy >= 0) {

            board.fields(x)(yy) match {
              case field if field == player => cur += 1
              case _                        => cur = 0
            }

            if (cur >= winSize) {
              return true
            }
          }
        }
      }
      false
    }

    WonHorizontal || WonVertical || WonDiag1 || WonDiag2
  }

  def printBoard(board: Board) = {
    board.fields.foreach { line =>
      {
        line.foreach { field => print(field + " ") }
        println()
      }
    }
  }

  def Max(maxField: Field.Value, minField: Field.Value, board: Board, max: Int, min: Int): (Int, Int, Int) = {
    val moves = FreeMoves(board)

    (PlayerWon(maxField, board), PlayerWon(minField, board)) match {
      case (true, _) => {
        println("won")
        (1, -1, -1)
      }
      case (_, true) => {
        println("lost")
        (-1, -1, -1)
      }
      case (false, false) if moves.length == 0 => (0, -1, -1)
      case _ => {
        var maxCur = Int.MinValue
        var bestMove = (-1, -1)
        for ((x, y) <- FreeMoves(board)) {
          val cur = Min(maxField, minField, AddMove(maxField, board, x, y), Math.max(maxCur, max), min)
          
          if (cur > maxCur) {
            maxCur = cur
            bestMove = (x, y)
          }

          if (maxCur >= min)
            (maxCur, x, y)
        }

        (maxCur, bestMove._1, bestMove._2)
      }
    }
  }

  def Min(maxField: Field.Value, minField: Field.Value, board: Board, max: Int, min: Int): Int = {
    val moves = FreeMoves(board)
    (PlayerWon(maxField, board), PlayerWon(minField, board)) match {
      case (true, _) => {
        println("won")
        (1)
      }
      case (_, true) => {
        println("lost")
        (-1)
      }
      case (false, false) if moves.length == 0 => (0)
      case _ => {
        var minCur = Int.MaxValue
        for ((x, y) <- moves) {
          val (cur, _, _) = Max(maxField, minField, AddMove(minField, board, x, y), max, Math.min(minCur, min))

          if (cur < minCur) {
            minCur = cur
          }

          if (minCur <= max)
            minCur
        }

        minCur
      }
    }
  }

  def InitializeMiniMax(board: Board): (Int, Int, Int) = {
    Max(Field.Player1, Field.Player2, board, Int.MinValue, Int.MaxValue)
  }

  def main(args: Array[String]) {
    var board = InitializeBoard
   /* board = AddMove(Field.Player1, board, 0, 0)
    board = AddMove(Field.Player2, board, 1, 1)
    board = AddMove(Field.Player1, board, 0, 1)
    board = AddMove(Field.Player2, board, 2, 1)*/
    val (result, x, y) = InitializeMiniMax(board)

    println("result:" + result + " x:" + x + " y:" + y);

  }
}