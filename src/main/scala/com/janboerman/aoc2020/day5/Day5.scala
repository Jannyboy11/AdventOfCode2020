package com.janboerman.aoc2020
package day5

import scala.io.Source;

val source = Source.fromResource("day5input.txt")
val seatIDs = source.getLines().map(toSeatID).toSet

def toSeatID(line: String): Int = {
    var lowerRow = 0
    var upperRow = 128
    for (i <- 0 until 7) {
        if (line(i) == 'F') {
            upperRow -= (upperRow - lowerRow) / 2
        } else if (line(i) == 'B') {
            lowerRow += (upperRow - lowerRow) / 2
        }
    }

    var lowerColumn = 0
    var upperColumn = 8
    for (i <- 7 until 10) {
        if (line(i) == 'L') {
            upperColumn -= (upperColumn - lowerColumn) / 2
        } else if (line(i) == 'R') {
            lowerColumn += (upperColumn - lowerColumn) / 2
        }
    }

    lowerRow * 8 + lowerColumn
}

object Day5 {
    //for some weird reason, it's a compiler error if I define the main function at the top level annotated with @main o.0
    /* [error] -- [E007] Type Mismatch Error: C:\Users\Test\Projects\Scala\AdventOfCode2020\src\main\scala\com\janboerman\aoc2020\day5\Day5.scala:33:0
     * [error] 33 |@main def main: Unit = {
     * [error]    |^^^^^
     * [error]    |Found:    com.janboerman.aoc2020.day5.main
     * [error]    |Required: scala.annotation.Annotation
     */
    
    def main(args: Array[String]): Unit = {
        val result1 = seatIDs.max
        println(result1)
        
        val result2 = (16 to result1).toSet.removedAll(seatIDs).max
        println(result2)
    }
}
