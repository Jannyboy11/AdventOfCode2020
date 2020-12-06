package com.janboerman.aoc2020.day6

import scala.collection.mutable
import scala.io.Source

val source = Source.fromResource("day6input.txt")
val groups = {
    val list = new mutable.ListBuffer[mutable.ListBuffer[String]]
    var groupAnswers = new mutable.ListBuffer[String]()
    for (personAnswer <- source.getLines()) {
        if (personAnswer.isEmpty) {
            list.append(groupAnswers)
            groupAnswers = new mutable.ListBuffer[String]()
        } else {
            groupAnswers.append(personAnswer)
        }
    }
    list.append(groupAnswers)

    list
}

object Day6 {
    
    def main(args: Array[String]): Unit = {
        val result1 = groups.map(_.flatten.toSet.size).sum
        println(result1)
        
        val result2 = groups.map(answers => ('a' to 'z').count(c => answers.forall(_.contains(c)))).sum
        println(result2)
    }

}
