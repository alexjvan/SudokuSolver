package com.alexvanmatre.sudokusolver.consoleinterface

import com.alexvanmatre.sudokusolver.solver.SudokuSolver
import com.alexvanmatre.sudokusolver.solver.objects.Square
import com.alexvanmatre.sudokusolver.solver.objects.exceptions.QueueEmptyException

import java.io.{File, FileNotFoundException}
import java.util.Scanner
import scala.math.BigDecimal.RoundingMode
import scala.util.control.Breaks.break

class Interface(puzzlePath: String, wantStats: Boolean) {
  val startTime: Long = System.nanoTime()
  var endTime: Long = 0

  val solver = new SudokuSolver()
  var puzzle = createPuzzle(puzzlePath)
  solve()

  private def createPuzzle(file: String) = {
    val fileObj = new File(file)
    if(!fileObj.exists()) {
      throw new FileNotFoundException()
    }
    parseFile(fileObj)
  }

  private def parseFile(file: File): Array[Array[Int]] = {
    val reader = new Scanner(file)
    var lines: List[List[Int]] = List()
    while(reader.hasNextLine()) {
      val data = reader.nextLine()
      if(data != "") {
        val split = data.split(" ")
        if(split.length == 81) {
          lines = 0.until(9).map(
            y => 0.until(9).map(
              x => {
                split(y * 9 + x).toInt
              }
            ).toList
          ).toList
        } else if(split.length == 9) {
          lines = lines.appended(split.map(_.toInt).toList)
        }
      }
    }
    lines.map(_.toArray).toArray
  }

  def solve() = {
    solver.importPuzzle(puzzle)
    printPuzzle(solver.puzzle)
    var lastSequence: Seq[Int] = solver.puzzle.flatMap(_.possibilities.map(_.value))
    var i = 0
    var notDiffer = 0
    while(!solver.isSolved()) {
      try {
        solver.solveStep()
        if(puzzlesDiffer(lastSequence, solver.puzzle)) {
          printPuzzle(solver.puzzle)
        } else
          notDiffer += 1
      } catch {
        case _: QueueEmptyException => {
          println("Queue empty! Can't complete")
          break
        }
        case e: Exception => {
          e.printStackTrace()
        }
      }
      lastSequence = solver.puzzle.flatMap(_.possibilities.map(_.value))
      i += 1
    }
    endTime = System.nanoTime()
    if(wantStats)
      printStats(i, notDiffer)
  }

  def copySeq(copier: Seq[Square]): Seq[Square] =
    Seq(
      copier.map(square => square.copy())
    ).flatten

  def puzzlesDiffer(comp: Seq[Int], another: Seq[Square]): Boolean = {
    var second = another.flatMap(_.possibilities.map(_.value))
    if(comp.length != second.length)
      true
    else {
      0.until(comp.length).foreach(i => {
        if(comp(i) != second(i))
          return true
      })
      false
    }
  }

  def printPuzzle(puzzle: Seq[Square]) = {
    1.to(9).foreach(y => {
      0.until(3).foreach(xSquare => {
        0.until(3).foreach(x => {
          print(puzzle((y - 1) * 9 + xSquare * 3 + x) + " ")
        })
        if(xSquare != 2)
          print("| ")
      })
      println()
      if(y != 9 && y % 3 == 0)
        println("----------------------")
    })
    println()
  }

  def printStats(cycles: Int, notChanged: Int) = {
    println("-------------------------")
    println("Stats")
    println("Runtime: " +
      BigDecimal(
        (endTime - startTime).doubleValue / 1000000000
      ).setScale(3, RoundingMode.HALF_UP)
    + "s")
    println("Cycles: " + cycles)
    println("Cycles nothing changed: " + notChanged)
    println("Cycles with changes: " + (cycles - notChanged))
    println("-------------------------")
  }

}
