package com.alexvanmatre.sudokusolver.solver.enums

sealed trait Number { val value: Int }

object Unsolved extends Number { val value = 0 }
object One extends Number { val value = 1 }
object Two extends Number { val value = 2 }
object Three extends Number { val value = 3 }
object Four extends Number { val value = 4 }
object Five extends Number { val value = 5 }
object Six extends Number { val value = 6 }
object Seven extends Number { val value = 7 }
object Eight extends Number { val value = 8 }
object Nine extends Number { val value = 9 }

object Number {
  def possibilities = Seq(One, Two, Three, Four, Five, Six, Seven, Eight, Nine)

  def apply(number: Int) =
    number match {
      case 1 => One
      case 2 => Two
      case 3 => Three
      case 4 => Four
      case 5 => Five
      case 6 => Six
      case 7 => Seven
      case 8 => Eight
      case 9 => Nine
      case _ => Unsolved
    }
}