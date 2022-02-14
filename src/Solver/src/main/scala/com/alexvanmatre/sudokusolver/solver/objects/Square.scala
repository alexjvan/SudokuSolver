package com.alexvanmatre.sudokusolver.solver.objects

import com.alexvanmatre.sudokusolver.solver.enums.{Eight, Five, Four, Nine, Number, One, Seven, Six, Three, Two, Unsolved}

case class Square(
   index: Int,
   var number: Number
) {
  val given: Boolean = number.value != Unsolved.value
  var solved: Boolean = given

  var possibilities: Seq[Number] =
    if(!solved)
      Number.possibilities
    else
      Seq(number)

  override def toString: String =
    number.value match {
      case 0 => possibilities.map(_.value.toString)
        .toString().substring(4)
      case _ => number.value.toString
    }
}
