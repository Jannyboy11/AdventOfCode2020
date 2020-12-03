package com.janboerman.aoc2020
package day1

import scala.io.Source;

val source = Source.fromResource("day1input.txt")
val numbers = source.getLines().map(_.toInt).toArray

@main def main: Unit = {
    
    var result1 = -1;
    var result2 = -1
    
    for (n <- numbers) {
        for (o <- numbers) {
            val sum1 = n + o
            if (sum1 == 2020) {
                result1 = n * o
            }

            for (p <- numbers) {
                val sum2 = n + o + p
                if (sum2 == 2020) {
                    result2 = n * o * p
                }
            }
        }
    }
    
    println(result1)
    println(result2)

}
