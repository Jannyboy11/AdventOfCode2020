package com.janboerman.aoc2020.day2

import scala.io.Source

val source = Source.fromResource("day2input.txt")
val entries = source.getLines().map { line =>
    val Array(pred, c, pw) = line.split(" ", 3)
    val Array(min, max) = pred.split("-", 2)
    Entry((min.toInt, max.toInt), c.charAt(0), pw)
} .toList

@main def main: Unit = {
    val result1 = entries.count { entry =>
        val (min, max) = entry.numbers
        val count = entry.password.count(_ == entry.character)
        min <= count && count <= max
    }
    println(result1)
    
    val result2 = entries.count { entry =>
        val (occ1, occ2) = entry.numbers
        (entry.password.charAt(occ1 - 1) == entry.character) != (entry.password.charAt(occ2 - 1) == entry.character)
    }
    println(result2)
}

case class Entry(numbers: (Int, Int), character: Char, password: String)
