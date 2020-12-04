package com.janboerman.aoc2020
package day4

import scala.io.Source;
import scala.collection.mutable

type Ecl = "ecl";
type Pid = "pid";
type Eyr = "eyr";
type Hcl = "hcl";
type Byr = "byr";
type Iyr = "iyr";
type Cid = "cid";
type Hgt = "hgt";
type Key = Ecl | Pid | Eyr | Hcl | Byr | Iyr | Cid | Hgt
type Value = String

val source = Source.fromResource("day4input.txt")
val passports = {
    val list = new mutable.ListBuffer[mutable.Map[Key, Value]]
    var passport = new mutable.HashMap[Key, Value]()
    for (line <- source.getLines()) {
        if (line.isEmpty) {
            list.append(passport)
            passport = new mutable.HashMap[Key, Value]();
        } else {
            for (keyValuePair <- line.split(" ")) {
                val Array(k, v) = keyValuePair.split(":")
                passport.put(k.asInstanceOf[Key], v)
            }
        }
    }
    list.append(passport)
    
    list
}

def isValid1(passport: mutable.Map[Key, Value]): Boolean = {
    passport.contains(valueOf[Ecl])
        && passport.contains(valueOf[Pid])
        && passport.contains(valueOf[Eyr])
        && passport.contains(valueOf[Hcl])
        && passport.contains(valueOf[Byr])
        && passport.contains(valueOf[Iyr])
        && passport.contains(valueOf[Hgt])
}

def isValid2(passport: mutable.Map[Key, Value]): Boolean = {
    passport.get(valueOf[Byr]) match {
        case None => return false
        case Some(byr) =>
            if (byr.matches("\\d{4}")) {
                val byrInt = byr.toInt
                if (byrInt < 1920 || byrInt > 2002) return false
            } else return false
    }
    
    passport.get(valueOf[Iyr]) match {
        case None => return false
        case Some(iyr) =>
            if (iyr.matches("\\d{4}")) {
                val iyrInt = iyr.toInt
                if (iyrInt < 2010 || iyrInt > 2020) return false
            } else return false
    }

    passport.get(valueOf[Eyr]) match {
        case None => return false
        case Some(eyr) =>
            if (eyr.matches("\\d{4}")) {
                val eyrInt = eyr.toInt
                if (eyrInt < 2020 || eyrInt > 2030) return false
            } else return false
    }
    
    passport.get(valueOf[Hgt]) match {
        case None => return false
        case Some(hgt) =>
            if (hgt.matches("\\d+cm")) {
                val cm = hgt.substring(0, hgt.length - 2).toInt
                if (cm < 150 || cm > 193) return false
            } else if (hgt.matches("\\d+in")) {
                val in = hgt.substring(0, hgt.length - 2).toInt
                if (in < 59 || in > 76) return false
            } else return false
    }
    
    passport.get(valueOf[Hcl]) match {
        case None => return false
        case Some(hcl) =>
            if (!hcl.matches("#[0-9a-f]{6}")) return false
    }
    
    passport.get(valueOf[Ecl]) match {
        case None => return false
        case Some(ecl) =>
            if (!(ecl == "amb" || ecl == "blu" || ecl == "brn" || ecl == "gry" || ecl == "grn" || ecl == "hzl" || ecl == "oth"))
                return false
    }
    
    passport.get(valueOf[Pid]) match {
        case None => return false
        case Some(pid) =>
            if (!pid.matches("\\d{9}")) return false
    }
    
    true
}

@main def main: Unit = {
    val result1 = passports.count(isValid1)
    println(result1)
    
    val result2 = passports.count(isValid2)
    println(result2)
}
