package org.grid

import scala.util.Try

object AirplaneTask {

  def solution(numRows: Int, reservations: String): Int = {
    val charMap = Try {
      reservations
        .split(" ").foldLeft(Map[Int, List[Char]]())
        {
          (acc: Map[Int, List[Char]], i: String) => {
            val key = i.charAt(0).asDigit
            acc.updated(key, acc.getOrElse(i.charAt(0).asDigit, List.empty[Char]) :+ i.charAt(1))
          }
        }
    }.getOrElse(Map[Int, List[Char]]())

    (1 to numRows)
      .map { (i: Int) =>
        List("ABC", "DEF", "EFG", "HJK")
          .filterNot { (s: String) => charMap.getOrElse(i, List[Char]()).exists(s.contains) }
      }
      .map { (lst: List[String]) => lst match {
        case List(_*) if lst.contains("DEF") && lst.contains("EFG") => lst.filterNot(_ == "DEF")
        case List(_*) => lst
      }}
      .map { (l: List[String]) => l.length }.sum
  }

  def main(args: Array[String]): Unit = {
    println(solution(1, ""))
  }
}
