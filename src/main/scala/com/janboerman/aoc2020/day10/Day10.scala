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
        
        val result2 = {
            val adapters = SortedSet.from(jolts)
            val combinationsCache = new mutable.HashMap[Int, Long]()
            combinationsCache.put(adapters.last, 1L)    //there is only one combination possible for the last adapter.
            combinations(0, adapters, combinationsCache)
        }
        println(result2)
    }
    
    def combinations(input: Int, adapters: SortedSet[Int], cache: mutable.Map[Int, Long]): Long = {
        cache.get(input) match {
            case Some(combinations) => combinations
            case None =>
                var comb = 0L
                for (i <- 1 to 3) {
                    if (adapters(input + i)) {
                        comb += combinations(input + i, adapters, cache)
                    }
                }

                cache.put(input, comb)
                comb
        }
    }
    
}
