package com.janboerman.aoc2020.day09

import scala.collection.mutable
import scala.io.Source

val source = Source.fromResource("day09input.txt")
val numbers = source.getLines().map(_.toLong).toIndexedSeq

object Day9 {
    
    def main(args: Array[String]): Unit = {
        val Some((result1, result1idx)) = numbers.drop(25).zipWithIndex.find { case (number, index) =>
            val slice = numbers.slice(index, index + 25)
            //could be improved by calculating the next sum more efficiently:
            //just add the newest value, remove the oldest.
            !LazyList.tabulate(25, 25)({slice(_) + slice(_)}).exists(_.exists(_ == number))
        }
        println(result1)
        
        //dynamic programming!
        val table: Array[Array[Agg]] = new Array(result1idx)
        def agg(y: Int, x: Int): Agg = {
            if (y == x) {
                val n = numbers(y)
                return Agg(n, n, n)
            }
            
            var row = table(y)
            if (row == null) {
                row = new Array(result1idx)
                table(y) = row
            }
            var res = row(x)
            if (res == null) {
                val Agg(minLeft, maxLeft, sumLeft) = agg(y, x-1)
                val Agg(minBelow, maxBelow, sumBelow) = agg(y+1, x)
                
                val newMin = Math.min(minLeft, minBelow)
                val newMax = Math.max(maxLeft, maxBelow)
                val newSum = sumLeft + numbers(x)
                
                res = Agg(newMin, newMax, newSum)
                row(x) = res
            }
            res
        }
        
        var searching = true
        var lowerIdx = result1idx - 1
        var upperIdx = result1idx - 1
        while (searching) {
            val Agg(min, max, sum) = agg(lowerIdx, upperIdx)
            if (sum == result1) {
                val result2 = min + max
                println(result2)
                searching = false
            }

            else if (sum < result1) {
                lowerIdx -= 1
            } else if (sum > result1) {
                upperIdx -= 1
            }
        }

    }

}

case class Agg(min: Long, max: Long, sum: Long)

/*  Structure of the table:
 *  Agg(0, 0), Agg(0, 1), Agg(0, 2), ..., Agg(0, idx-1)
 *             Agg(1, 1), Agg(1, 2), ..., Agg(1, idx-1)
 *                        Agg(2, 2), ..., Agg(2, idx-1)
 * 
 *  Agg(Y, X) can be calculated from Agg(Y+1, X) and Agg(Y, X-1)
 */