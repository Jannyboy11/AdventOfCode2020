package com.janboerman.aoc2020.day10

import scala.collection.immutable.SortedSet
import scala.collection.mutable
import scala.io.Source

val source = Source.fromResource("day10input.txt")
val jolts = source.getLines().map(_.toInt).toArray

object Day10 {

    def main(args: Array[String]): Unit = {
        val result1 = {
            val treeSet = mutable.TreeSet.from(jolts)
            
            var jump1 = 0
            var jump3 = 0

            var lastJoltage = 0
            while (treeSet.nonEmpty) {
                val joltage = treeSet.head
                val diff = joltage - lastJoltage

                if (diff == 1) {
                    jump1 += 1
                } else if (diff == 3) {
                    jump3 += 1
                }

                lastJoltage = joltage
                treeSet.remove(joltage)
            }
            jump3 += 1 //device built-in adapter is always 3 higher than the highest adapter in the bag.
            
            jump1 * jump3
        }
        println(result1)
        
        val result2 = combinations(0, SortedSet.from(jolts), new mutable.HashMap())
        println(result2)
    }
    
    def combinations(input: Int, adapters: SortedSet[Int], memory: mutable.Map[Int, Long]): Long = {
        memory.get(input) match {
            case Some(combinations) => return combinations
            case _ =>
                var comb = 0L
                for (i <- 1 to 3) {
                    if (adapters(input + i)) {
                        comb += combinations(input + i, adapters, memory)
                    }
                }
                
                if (comb == 0L) comb = 1L   //needed for the 'highest' adapter

                memory.put(input, comb)
                comb
        }
    }
    
}
