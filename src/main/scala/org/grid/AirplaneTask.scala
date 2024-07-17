package org.grid

case class Seat(row: Int, letter: Char)
case class Output(reservations: Int, duplicates: Set[Seat], unparsableInputChunks: List[String])

class AirplaneTask {

  def filterInput(numRows: Int, reservations: String): (Seq[Seat], Array[String]) = {
    val pattern = s"[1-$numRows][A-HJ-K]".r
    val (matched: Array[String], notMatched: Array[String]) = reservations
      .split(" ")
      .partition {(s: String) => s match {
        case pattern(_*) => true
        case _ => false
      }}

    (matched.map {(s: String) => Seat(s.charAt(0).asDigit, s.charAt(1))}, notMatched)
  }

  def createMap(seats: Seq[Seat]): (Map[Int, Set[Seat]], Set[Seat]) =
    seats.foldLeft((Map[Int, Set[Seat]](), Set[Seat]())) {
      (acc: (Map[Int, Set[Seat]], Set[Seat]), seat: Seat) => {

        val row = seat.row
        val seatSet = acc._1.getOrElse(row, Set.empty[Seat])

        seatSet.contains(seat) match {
          case true => (acc._1.updated(row, seatSet - seat), acc._2 + seat) // duplicate
          case false => (acc._1.updated(row, seatSet + seat), acc._2)
        }
      }
    }

  def filterSeatRow(i: Int, seatMap: Map[Int, Set[Seat]]): List[String] = {
    val SeatSeq = seatMap.getOrElse(i, List.empty[Seat])
    List("ABC", "DEF", "EFG", "HJK")
      .filterNot { (s: String) => SeatSeq.exists { (seat: Seat) => s.contains(seat.letter) } }
  }

  def filterMiddleSeats(lst: List[String]): List[String] = lst match {
    case List(_*) if lst.contains("DEF") && lst.contains("EFG") => lst.filterNot(_ == "DEF")
    case List(_*) => lst
  }

  def solution(numRows: Int, reservations: String): Output = {

    val (seats: Seq[Seat], unparsableInputChunks: Array[String]) = filterInput(numRows, reservations)
    val (seatMap: Map[Int, Set[Seat]], duplicateSeat: Set[Seat]) = createMap(seats)

    val reservationCount = (1 to numRows)
      .map { (i: Int) => filterSeatRow(i, seatMap) }
      .map { filterMiddleSeats }
      .map { (l: List[String]) => l.length }.sum

    Output(reservationCount, duplicateSeat, unparsableInputChunks.toList)
  }
}
