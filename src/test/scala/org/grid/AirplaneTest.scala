package org.grid

import org.scalatest.funsuite.AnyFunSuite

class AirplaneTest extends AnyFunSuite {

  val airplane = new AirplaneTask()

  test("test if counts correctly") {
    val output1 = airplane.solution(1, "asdaf 1F 1J")
    assert(output1.reservations == 1)
    assert(output1.unparsableInputChunks == List("asdaf"))

    val output2 = airplane.solution(2, "")
    assert(output2.reservations == 6)

    val output3 = airplane.solution(3, "adsfag, room plane 1D 2D 3D kitchen")
    assert(output3.reservations == 9)
  }

  test("test filter input") {
    val input1 = "2A 1A 1B 1D asdf ggdffdg"
    val (matched1, notMatched1) = airplane.filterInput(2, input1)
    assert(matched1 == Seq(Seat(2, 'A'), Seat(1, 'A'), Seat(1, 'B'), Seat(1, 'D')))

    val input2 = "3H 2J 1K 3A number one"
    val (matched2, notMatched2) = airplane.filterInput(2, input2)
    assert(matched2 == Seq(Seat(2, 'J'), Seat(1, 'K')))
    assert(notMatched2 sameElements Array("3H", "3A", "number", "one"))
  }

  test("test create map") {
    val input1 = Seq(Seat(1, 'A'), Seat(2, 'H'), Seat(1, 'D'), Seat(1, 'J'), Seat(2, 'C'))
    val (seatMap1, duplicateSeat1) = airplane.createMap(input1)
    assert(seatMap1 == Map(1 -> Set(Seat(1, 'A'), Seat(1, 'D'), Seat(1,'J')), 2 -> Set(Seat(2, 'H'), Seat(2, 'C'))))
    assert(duplicateSeat1 == Set.empty[Seat])

    val input2 = Seq(Seat(1, 'H'), Seat(1, 'C'), Seat(2, 'B'), Seat(1, 'H'), Seat(1, 'A'), Seat(2, 'D'))
    val (seatMap2, duplicateSeat2) = airplane.createMap(input2)
    assert(seatMap2 == Map(1 -> Set(Seat(1, 'C'), Seat(1, 'A')), 2 -> Set(Seat(2, 'B'), Seat(2, 'D'))))
    assert(duplicateSeat2 == Set(Seat(1, 'H')))
  }

  test("test filter seat Row") {
    val i1 = 1
    val seatMap1 = Map(1 -> Set(Seat(1, 'B'), Seat(1, 'D')))
    val result = airplane.filterSeatRow(i1, seatMap1)
    assert(result == List("EFG", "HJK"))

    val i2 = 3
    val seatMap2 = Map(3 -> Set(Seat(3, 'H'), Seat(3, 'J')))
    val result2 = airplane.filterSeatRow(i2, seatMap2)
    assert(result2 == List("ABC", "DEF", "EFG"))
  }

  test("test filter middle seats") {
    val input1 = List("ABC", "DEF", "EFG", "HJK")
    assert(airplane.filterMiddleSeats(input1) == List("ABC", "EFG", "HJK"))

    val input2 = List("ABC", "DEF")
    assert(airplane.filterMiddleSeats(input2) == List("ABC", "DEF"))
  }
}
