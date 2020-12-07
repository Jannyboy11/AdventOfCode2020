package com.janboerman.aoc2020.day7

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

type Tone = String
type Colour = String

val exampleInput1 =
    """
      |light red bags contain 1 bright white bag, 2 muted yellow bags.
      |dark orange bags contain 3 bright white bags, 4 muted yellow bags.
      |bright white bags contain 1 shiny gold bag.
      |muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
      |shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
      |dark olive bags contain 3 faded blue bags, 4 dotted black bags.
      |vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
      |faded blue bags contain no other bags.
      |dotted black bags contain no other bags.""".stripMargin

val exampleInput2 =
    """
      |shiny gold bags contain 2 dark red bags.
      |dark red bags contain 2 dark orange bags.
      |dark orange bags contain 2 dark yellow bags.
      |dark yellow bags contain 2 dark green bags.
      |dark green bags contain 2 dark blue bags.
      |dark blue bags contain 2 dark violet bags.
      |dark violet bags contain no other bags.""".stripMargin

//val source = Source.fromString(exampleInput2)
val source = Source.fromResource("day7input.txt")
val rules = source.getLines().map { line =>
    if (line.isEmpty) {
        Rule(Bag("", ""), Set[(Int, Bag)]())
    } else {
        val Array(key, value) = line.split(" contain ")
        val Array(tone, colour, "bags") = key.split(" ")
        val outerBag = Bag(tone, colour)

        val contained: Set[(Int, Bag)] = value.substring(0, value.length - 1) match {
            case "no other bags" => Set[(Int, Bag)]()
            case rhs =>
                val buffer = new ListBuffer[(Int, Bag)]
                for (containedBag <- rhs.split(", ")) {
                    val Array(amount, tone, colour, _) = containedBag.split(" ")
                    buffer += amount.toInt -> Bag(tone, colour)
                }
                buffer.toSet
        }

        Rule(outerBag, contained)
    }
} .toSet

object Day7 {
    def main(args: Array[String]): Unit = {
        val myBag = Bag("shiny", "gold")

        val containmentMap = new mutable.HashMap[Bag, mutable.Set[Bag]]
        for (Rule(outerBag, containedBags) <- rules) {
            for ((amount, containedBag) <- containedBags) {
                containmentMap.getOrElseUpdate(containedBag, new mutable.HashSet[Bag]()).add(outerBag)
            }
        }

        def getContainerBags(bag: Bag): mutable.Set[Bag] = {
            containmentMap.getOrElse(bag, new mutable.HashSet())
        }

        val bagSearchSet = new mutable.HashSet[Bag]()
        bagSearchSet.addAll(getContainerBags(myBag))

        while ( {
            val newBags = bagSearchSet.flatMap(keyBag => getContainerBags(keyBag))
            val allKnownBags = newBags.forall(newBag => bagSearchSet.contains(newBag))
            if (!allKnownBags) {
                bagSearchSet.addAll(newBags)
            }
            !allKnownBags
        }) {}

        val result1 = bagSearchSet.size
        println(result1)

        val countMap = new mutable.HashMap[Bag, mutable.HashMap[Bag, BigInt]]
        for (Rule(outerBag, containedBags) <- rules) {
            for ((amount, containedBag) <- containedBags) {
                val innerMap = countMap.getOrElseUpdate(outerBag, new mutable.HashMap[Bag, BigInt])
                innerMap.getOrElseUpdate(containedBag, BigInt(amount))
            }
        }

        def getContainedBags(count: BigInt, bag: Bag): BigInt = countMap.get(bag) match {
            case Some(innerMap) if !innerMap.isEmpty =>
                count + innerMap
                    .map { case (innerBag, innerCount) => 
                        val amountInner = innerCount * count
                        getContainedBags(amountInner, innerBag)
                    }
                    .sum
            case _ => count
        }

        val result2 = getContainedBags(BigInt(1), myBag) - BigInt(1) /*discourn the shiny gold bag itself*/
        println(result2)
    }
}

case class Bag(tone: Tone, colour: Colour)
case class Rule(bag: Bag, contains: Set[(Int, Bag)])
