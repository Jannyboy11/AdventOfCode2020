package com.janboerman.aoc2020
package day02

import scala.io.Source

val source = Source.fromResource("day02input.txt")
val entries = source.getLines().map { line =>
    val Array(pred, c, pw) = line.split(" ", 3)
    val Array(min, max) = pred.split("-", 2)
    Entry((min.toInt, max.toInt), c.charAt(0), pw)
} .toList

@main def main: Unit = {
    val result1 = entries.count { case Entry((min, max), character, password) =>
        val count = password.count(_ == character)
        min <= count && count <= max
    }
    println(result1)
    
    val result2 = entries.count { case Entry((occ1, occ2), character, password) =>
        (password.charAt(occ1 - 1) == character) != (password.charAt(occ2 - 1) == character)
    }
    println(result2)
}

case class Entry(numbers: (Int, Int), character: Char, password: String)
