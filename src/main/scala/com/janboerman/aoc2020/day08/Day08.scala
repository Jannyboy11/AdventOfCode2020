package com.janboerman.aoc2020.day08

import scala.io.Source
import scala.collection.mutable

type ProgramCounter = Int
type Accumulator = Int
type Program = IndexedSeq[Instruction]
enum OpCode {
    case Nop
    case Acc
    case Jmp
}
enum Action {
    case Plus
    case Minus
}
import OpCode._
import Action._

val source = Source.fromResource("day08input.txt")
val inputProg: Program = source.getLines().map { line =>
    val Array(opCode, operand) = line.split(" ")
    val oc = opCode match {
        case "nop" => Nop
        case "acc" => Acc
        case "jmp" => Jmp
    }
    val action = operand(0) match {
        case '+' => Plus
        case '-' => Minus
    }
    val argument = operand.substring(1).toInt
    Instruction(oc, Operand(action, argument))
} .toIndexedSeq

object Day8 {

    def main(args: Array[String]): Unit = {
        val result1 = {
            val (false, acc) = terminates(inputProg)
            acc
        }
        println(result1)
        
        val result2 = {
            val (true, acc) = LazyList.fill(inputProg.size)(inputProg)
                .zipWithIndex.map(modifyProgram)
                .map(terminates)
                .dropWhile { case (terminates, acc) => !terminates }
                .head
            acc
        }
        println(result2)
    }
    
    //because there are not conditional jumps yet, the halting problem does not hold here
    def terminates(prog: Program): (Boolean, Accumulator) = {
        var computer = new Computer(0, 0, prog)
        val visitedInstructions = new mutable.HashSet[Int]

        var lastAcc = 0
        while (visitedInstructions.add(computer.instructionPointer)) {
            lastAcc = computer.accumulator
            
            if (computer.instructionPointer >= prog.size)
                return (true, lastAcc)
            
            computer = computer.step()
        }

        (false, lastAcc)
    }
    
    def modifyProgram(prog: Program, pc: Int): Program = {
        val insn = prog(pc) match {
            case Instruction(Jmp, operand) => Instruction(Nop, operand)
            case Instruction(Nop, operand) => Instruction(Jmp, operand)
            case i => i
        }
        prog.updated(pc, insn)
    }
    
}

object Computer {
    def apply(program: Program): Computer = new Computer(0, 0, program)
}

case class Computer(instructionPointer: ProgramCounter, accumulator: Accumulator, prog: Program) {
    def step(): Computer = {
        prog(instructionPointer) match {
            case Instruction(Nop, _) => Computer(instructionPointer + 1, accumulator, prog)
            case Instruction(Acc, Operand(action, value)) => Computer(instructionPointer + 1, binOp(action)(accumulator, value), prog)
            case Instruction(Jmp, Operand(action, relative)) => Computer(binOp(action)(instructionPointer, relative), accumulator, prog)
        }
    }
    
    private def binOp(action: Action): (Int, Int) => Int = action match {
        case Plus => {_ + _}
        case Minus => {_ - _}
    }
}

case class Instruction(opcode: OpCode, operand: Operand) {
    override def toString: String = {
        val opCodeString = opcode match {
            case Nop => "nop"
            case Acc => "acc"
            case Jmp => "jmp"    
        }
        s"$opCodeString $operand"
    }
}

case class Operand(action: Action, value: Int) {
    override def toString: String = {
        val char = action match {
            case Plus => '+'
            case Minus => '-'
        }
        s"$char$value"
    }
}
