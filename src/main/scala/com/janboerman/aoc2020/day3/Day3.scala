package com.janboerman.aoc2020
package day3

import scala.io.Source

enum Tile {
    case Empty
    case Tree
}
import Tile._

type Grid = IndexedSeq[IndexedSeq[Tile]]
extension (g: Grid) def apply(x: Int, y: Int): Tile = g(y)(x)
extension (g: Grid) def width: Int = g(0).size
extension (g: Grid) def depth: Int = g.size

val source = Source.fromResource("day3input.txt")
val grid: Grid = source.getLines().map(line => line.map {
    case '.' => Empty
    case '#' => Tree
} .toIndexedSeq).toIndexedSeq

@main def main: Unit = {
    
    var result1 = -1;
    var result2 = 1L;
    
    for ((right, down) <- Seq((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))) {
        var treeCount = 0
        var x = 0
        var y = 0
        while (y < grid.depth) {
            if (grid(x, y) == Tree)
                treeCount += 1

            x += right
            y += down

            if (x >= grid.width)
                x -= grid.width
        }
        result2 *= treeCount.toLong
        
        if (right == 3 && down == 1) {
            result1 = treeCount
        }
    }
    
    println(result1)
    println(result2)
}

