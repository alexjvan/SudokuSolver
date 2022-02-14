package com.alexvanmatre.sudokusolver.solver

import com.alexvanmatre.sudokusolver.solver.enums.Number
import com.alexvanmatre.sudokusolver.solver.objects.Square
import com.alexvanmatre.sudokusolver.solver.objects.exceptions.QueueEmptyException

import scala.collection.immutable.Queue

class SudokuSolver {
  var puzzle: Seq[Square] = Seq()

  private var queue: Queue[Square] = Queue()

  def importPuzzle(importing: Array[Array[Int]]) = {
    var i = -1
    val mapped = importing.flatMap(internal =>
      internal.map(number => {
        i += 1
        Square(
          i,
          Number(number)
        )
      })
    )
    puzzle = mapped
    queueSolved()
  }

  private def queueSolved() =
    queue = queue.appendedAll(puzzle.filter(_.given))

  def solve() =
    while(!isSolved())
      solveStep()

  def isSolved(): Boolean =
    this.puzzle.forall(_.solved)

  def solveStep() = {
    if(queue.isEmpty) {
//      onceOver()
      if(queue.isEmpty) {
        throw new QueueEmptyException()
      }
    } else {
      val dequeued = queue.dequeue
      queue = dequeued._2
      val square = dequeued._1
      adjustFromSquare(square)
      checkSolo(square)
    }
  }

  def queueSize(): Int =
    queue.size

  def onceOver(): Unit =
    puzzle.foreach(checkSolo)

  def adjustFromSquare(square: Square) = {
    val indices = manipulateIndices(square.index)
    indices.foreach(
      index => {
        val item = puzzle(index)
        item.solved match {
          case false => {
            item.possibilities = item.possibilities.filterNot(_.value == square.number.value)
            item.possibilities.length match {
              case 1 => setSquare(item)
              case _ => {
                // Do nothing
              }
            }
          }
          case true => {
            // Do nothing
          }
        }
      }
    )
  }

  def manipulateIndices(index: Int): List[Int] =
    (
      verticalIndices(index) ++
        horizontalIndices(index) ++
        squareIndices(index)
    ).filterNot(_ == index)
      .distinct.sorted

  private def verticalIndices(index: Int): List[Int] =
    1.to(9).flatMap(number => {
      List(
        index - number * 9,
        index + number * 9
      )
    }).filter(i => i >= 0 && i < 81).toList

  private def horizontalIndices(index: Int): List[Int] =
    0.to(index % 9).map(index - _).toList ++
      0.to(8 - index % 9).map(index + _).toList

  private def squareIndices(index: Int): List[Int] = {
    val topLeft = index - (9 * (index / 9 % 3)) - index % 3
    List(
      topLeft,
      topLeft + 1,
      topLeft + 2,
      topLeft + 9,
      topLeft + 10,
      topLeft + 11,
      topLeft + 18,
      topLeft + 19,
      topLeft + 20
    )
  }

  def setSquare(square: Square) = {
    square.number = square.possibilities.head
    square.solved = true
    queue = queue.appended(square)
  }

  def checkSolo(square: Square) = {
    checkSoloArray(verticalIndices(square.index).map(puzzle(_)))
    checkSoloArray(horizontalIndices(square.index).map(puzzle(_)))
    checkSoloArray(squareIndices(square.index).map(puzzle(_)))
  }

  def checkSoloArray(squares: Seq[Square]) = {
    // TODO: I think I want to keep this for now -
    Number.possibilities.foreach(number => {
      val matches = squares.filter(_.possibilities.map(_.value).contains(number.value))
      matches.length match {
        case 1 => {
          matches.head.possibilities = Seq(number)
          setSquare(matches.head)
        }
        case _ => {/* Do nothing */}
      }
    })
    // TODO check how many times a number appears in a square for an index
    //    None should be 1, should be greater than 5
    //    If the number of times a number appears in a square equals the number of times
    //    Other numbers (equal to the number of times) appear in the SAME squares
    //    Required for those, remove the numbers from all other squares in index
    //        Will probably need to create a new way of queueing operations
  }

}
