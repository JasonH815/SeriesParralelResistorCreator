package app

import scala.util.Random


/**
 * Created by Jason on 10/23/2016.
 *
*/
object EvolutionOperators {

  implicit class StringOperations(genome:String) {

    def wrapParallel:String = "(" + genome + ")"

    def clean():String = {

      // fix bad mutations
      var result = genome
      result = result.replace("+)", ")")
      result = result.replace("(+", "(")
      result = result.replace("++", "+")
      result.replace("()", "")
    }

    def swap2Resistors():String = {

      val resistors = "abcdefgh"
      val r1 = resistors.charAt(Random.nextInt(resistors.length))
      val r2 = resistors.charAt(Random.nextInt(resistors.length))

      val idx1 = genome.indexOf(r1)
      val idx2 = genome.indexOf(r2)
      val genomeArray = genome.toCharArray
      genomeArray.update(idx1, r2)
      genomeArray.update(idx2, r1)
      genomeArray.mkString("")

    }

    def insertParallel():String = {
      val idx1 = Random.nextInt(genome.length)
      val idx2 = Random.nextInt(genome.length - idx1)

      val tail = genome.drop(idx1)
      (genome.take(idx1) + "(" + tail.take(idx2) + ")" + tail.drop(idx2)).clean()

    }

    def insertSeries():String = {
      val idx1 = Random.nextInt(genome.length - 1) + 1 //don't insert at beginning or end
      (genome.take(idx1) + "+" + genome.drop(idx1)).clean()

    }



    def deleteParallel():String = {
      val result = deleteChar('(', genome)
      deleteChar(')', result).clean()
    }

    def deleteSeries():String = {
      deleteChar('+', genome).clean()
    }

  }

  def deleteChar(c:Char, genome:String):String = {
    val split = genome.split(c)
    val idx = Random.nextInt(split.length)

    var count = 0
    split.foldLeft[String]("")((current, s) => {
      val result = if(idx == count) current + s else current + c + s
      count += 1
      result
    })
  }


  def crossover(p1:String, p2:String): String = {

    val resistors= "^abcdefgh$"
    val resistorSet = resistors.toSet
    val operationsSet = "()+"


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