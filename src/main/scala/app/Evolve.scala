package app

import scala.util.Random


/**
 * Created by Jason on 10/23/2016.
 *
*/
class Evolve(generation:List[String]) {

  val resistors= "^abcdefgh$"
  val resistorSet = resistors.toSet
  val operationsSet = "()+"


  val generationRanked = generation.map(genome => Solver.solve(genome)).sortBy(result => math.abs(result.resistance - 7000))

  //pick pairs randomly amongst highest performing strings

  //for each pair:
    //pass 1: resistor crossover:
    //pick two points on p1, copy resistors before first point on p1, copy resitors after second point on p1
    //pick remaining resitors from p2 in the order they appear in p2


    //pass 2: structure crossover:
    //pick two series operators or a pair of parallel operators in p1
    //substitute operators from a corespding series or parallel section in p2

  def crossover(p1:String, p2:String): String = {
    val r1 = resistors.charAt(Random.nextInt(resistors.length))
    val r2 = resistors.charAt(Random.nextInt(resistors.length))

    //pick random points to splice
    val leftTail:List[Char] = if (r1 != '^') p1.takeWhile(_ != r1).toList else List.empty[Char]
    val rightTail:List[Char] = if(r2 != '^') p1.dropWhile(_ != r2).toList else p1.toList

    //copy of p2 resistors and structure
    val p1Resistors = leftTail.toSet.union(rightTail.toSet)
    var middle = p2.filter(c => (resistorSet.contains(c) && !p1Resistors.contains(c)) || operationsSet.contains(c))
    var previousLength = middle.length

    // add missing parens
    val leftParen = middle.count(_ == '(')
    val rightParen = middle.count(_ == ')')
    if(leftParen < rightParen) {
      middle = List.fill(rightParen - leftParen)('(').mkString("") + middle
    } else {
      middle = middle + List.fill(leftParen - rightParen)(')').mkString("")
    }

    // remove trash
    while (middle.length != previousLength) {
      previousLength = middle.length
      middle = middle.replace("++", "+")
      middle = middle.replace("()", "")
    }

    (leftTail ::: middle.toList ::: rightTail).mkString("")

  }


}