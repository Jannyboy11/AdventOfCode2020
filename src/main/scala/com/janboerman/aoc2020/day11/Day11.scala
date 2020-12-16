package com.janboerman.aoc2020.day11

import scala.io.Source

val source = Source.fromResource("day11input.txt")
val grid: Grid = source.getLines().map(_.toIndexedSeq).toIndexedSeq

object TileGrid {
    type Tile = Char
    type Grid = IndexedSeq[IndexedSeq[Tile]]
    
    val Floor: Tile = '.'
    val EmptySeat: Tile = 'L'
    val Occupied: Tile = '#'
    
    extension (g: Grid)
        def height: Int = g.size
        def width: Int = g(0).size
    
        def getTile(y: Int, x: Int): Tile = {
            if (0 <= y && y < g.height && 0 <= x && x < g.width)
                g(y)(x)
            else
                Floor
        }
        def withTile(y: Int, x: Int, t: Tile): Grid = {
            if (0 <= y && y < g.height && 0 <= x && x < g.width)
                g.updated(y, g(y).updated(x, t))
            else
                g
        }
    
    def stepGrid1(g: Grid): Grid = {
        var res = g
        for (x <- 0 until g.width; y <- 0 until g.height) {

            val tile = g.getTile(y, x)
            
            if (tile == EmptySeat) {
                var occupiedNeighbour: Boolean = false
                for (i <- (x - 1) to (x + 1); j <- (y - 1) to (y + 1)) {
                    if (!(i == x && j == y)) {
                        if (g.getTile(j, i) == Occupied) occupiedNeighbour = true
                    }
                }
                if (!occupiedNeighbour) {
                    res = res.withTile(y, x, Occupied)
                }
            }

            else if (tile == Occupied) {
                var occupiedNeighbours: Int = 0
                for (i <- (x - 1) to (x + 1); j <- (y - 1) to (y + 1)) {
                    if (!(i == x && j == y)) {
                        if (g.getTile(j, i) == Occupied) occupiedNeighbours += 1
                    }
                }
                if (occupiedNeighbours >= 4) {
                    res = res.withTile(y, x, EmptySeat)
                }
            }
        }
        
        res
    }
    
    def stepGrid2(g: Grid): Grid = {
        var res = g

        for (x <- 0 until g.width; y <- 0 until g.height) {

            val tile = g.getTile(y, x)

            if (tile == EmptySeat) {
                var occupiedNeighbour: Boolean = false
                for ((diffY, diffX) <- Seq(-1 -> -1, -1 -> 0, -1 -> 1, 0 -> -1, 0 -> 1, 1 -> -1, 1 -> 0, 1 -> 1)) {
                    var j = y + diffY;
                    var i = x + diffX;
                    var t = g.getTile(j, i)
                    while (0 <= j && j < g.height && 0 <= i && i < g.width && t == Floor) {
                        j += diffY
                        i += diffX
                        t = g.getTile(j, i)
                    }
                    if (t == Occupied) occupiedNeighbour = true
                }
                if (!occupiedNeighbour) {
                    res = res.withTile(y, x, Occupied)
                }
            }

            else if (tile == Occupied) {
                var occupiedNeighbours: Int = 0
                for ((diffY, diffX) <- Seq(-1 -> -1, -1 -> 0, -1 -> 1, 0 -> -1, 0 -> 1, 1 -> -1, 1 -> 0, 1 -> 1)) {
                    var j = y + diffY;
                    var i = x + diffX;
                    var t = g.getTile(j, i)
                    while (0 <= j && j < g.height && 0 <= i && i < g.width && t == Floor) {
                        j += diffY
                        i += diffX
                        t = g.getTile(j, i)
                    }
                    if (t == Occupied) occupiedNeighbours += 1
                }
                if (occupiedNeighbours >= 5) {
                    res = res.withTile(y, x, EmptySeat)
                }
            }
        }
        
        res
    }
}
import TileGrid._

object Day11 {

    def main(args: Array[String]): Unit = {
        val equilibriumGrid1 = {
            var g: Grid = grid
            var h: Grid = grid

            while ({
                h = stepGrid1(h)
                g != h
            }) {
                g = h
            }
            g
        }

        val result1 = equilibriumGrid1.foldLeft(0)((sum, seq) => sum + seq.count(_ == Occupied))
        println(result1)

        val equilibriumGrid2 = {
            var g: Grid = grid
            var h: Grid = grid

            while ({
                h = stepGrid2(h)
                g != h
            }) {
                g = h
            }
            g
        }

        val result2 = equilibriumGrid2.foldLeft(0)((sum, seq) => sum + seq.count(_ == Occupied))
        println(result2)
    }
    
}
