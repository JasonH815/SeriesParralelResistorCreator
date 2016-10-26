package app

import scala.util.Random


/**
 * Created by Jason on 10/23/2016.
 *
*/
object EvolutionOperators {

  val resistors = "abcdefgh"
  val resistorsWithBounds = "^abcdefgh$"
  val resistorSet = resistors.toSet
  val resistorsWithBoundsSet = resistorsWithBounds.toSet
  val operators = "()+"
  val operatorSet = operators.toSet

  implicit class StringOperations(genome:String) {

    def cleanParens:String = {

      def getMatchingIdx(s: String): Int = {
        var count = 1
        for (i <- 1 until s.length) {
          if (s.charAt(i) == '(')
            count += 1
          else if (s.charAt(i) == ')')
            count -= 1
          if (count == 0)
            return i
        }
        -1
      }

      def getPriority(c: Option[Char], currentParen:Char, defaultPriority:Int): Int = {
        c.map {
          case p if p == ')' && currentParen == '(' => 1
          case p if p == '(' && currentParen == ')' => 1
          case p if "()".contains(currentParen) && resistorSet.contains(p) => 1
          case p if p == '+' => 2
          case _ => defaultPriority
        }.getOrElse(defaultPriority)
      }

      GlobalLogger.logger.debug(s"Starting Genome: $genome")

      var oldGenome = genome
      var newGenomeHead = ""

      var currentIdx = oldGenome.indexOf('(')
      if (currentIdx < 0)
        return genome // nothing to do

      while (currentIdx >= 0) {

        val matchingIdx = getMatchingIdx(oldGenome.drop(currentIdx))+currentIdx
        val left = if (currentIdx - 1 >= 0) Some(oldGenome.charAt(currentIdx - 1)) else None
        val right = if (matchingIdx + 1 < oldGenome.length) Some(oldGenome.charAt(matchingIdx + 1)) else None
        val rightTake = oldGenome.length - matchingIdx - 1

        //if the parens are wrapping we don't need them, we will wrap at the end
        if (left.isEmpty && right.isEmpty) {
          oldGenome = oldGenome.take(matchingIdx) + oldGenome.drop(matchingIdx + 1)
          oldGenome = oldGenome.take(currentIdx) + oldGenome.drop(currentIdx + 1)

        } else {
          val inner = oldGenome.drop(currentIdx + 1).dropRight(rightTake + 1)
          GlobalLogger.logger.trace(s"oldGenome: $oldGenome")
          GlobalLogger.logger.trace(s"inner: $inner")
          GlobalLogger.logger.trace(s"newGenome: $newGenomeHead")
          // remove parens wrapping a single resistor
          if (inner.length <= 1) {
            oldGenome = oldGenome.take(matchingIdx) + oldGenome.drop(matchingIdx + 1)
            oldGenome = oldGenome.take(currentIdx) + oldGenome.drop(currentIdx + 1)
          }

          else {
            //get inner priority
            val innerPriority = if (inner.sliding(2).exists(pair => pair.length == 2 && pair != "()" &&
              (pair.charAt(0) == ')' && resistorSet.contains(pair.charAt(1)) ||
               pair.charAt(1) == '(' && resistorSet.contains(pair.charAt(0)) ||
               pair == ")(" ||
               pair.forall(c => resistorSet.contains(c))))) 1
            else
              inner.map(c => getPriority(Some(c), '-', 3)).min

            GlobalLogger.logger.trace(s"left priority: ${getPriority(left, '(', 0)}")
            GlobalLogger.logger.trace(s"right priority: ${getPriority(right, ')', 0)}")
            GlobalLogger.logger.trace(s"inner priority: $innerPriority")

            if (innerPriority < getPriority(left, '(', 0) || innerPriority < getPriority(right, ')', 0)) {
              // we need to keep the parens
              newGenomeHead = newGenomeHead + oldGenome.take(currentIdx + 1)
              oldGenome = oldGenome.drop(currentIdx + 1)
            } else {
              // remove parens
              oldGenome = oldGenome.take(matchingIdx) + oldGenome.drop(matchingIdx + 1)
              oldGenome = oldGenome.take(currentIdx) + oldGenome.drop(currentIdx + 1)
            }
          }
        }

        currentIdx = oldGenome.indexOf('(')
      }

      GlobalLogger.logger.debug(s"New Genome: ${newGenomeHead + oldGenome}") // + newGenomeTail}")
      newGenomeHead + oldGenome
    }

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
      val idx2 = Random.nextInt(genome.length - idx1 + 1)

      val tail = genome.drop(idx1)
      val middle = tail.take(idx2)

      if (middle.length == 0) {
        return genome
      }

      (genome.take(idx1) + "(" + middle + ")" + tail.drop(idx2)).clean().cleanParens

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

    val p1Resistors = p1.filter(c => resistorSet.contains(c))
    Console.err.println(s"p1 resistors: $p1Resistors")

    val idx1 = 0//Random.nextInt(p1Resistors.length + 1)
    val idx2 = 8//Random.nextInt(p1Resistors.length - idx1 + 1)+idx1

    val r1 = if (idx1 < p1Resistors.length) p1Resistors.charAt(idx1) else '$'
    val r2 = if (idx2 < p1Resistors.length) p1Resistors.charAt(idx2) else '$'

    Console.err.println(s"idx1: $idx1")
    Console.err.println(s"idx2: $idx2")
    Console.err.println(s"p1: $p1")
    Console.err.println(s"p2: $p2")
    Console.err.println(s"r1: $r1")
    Console.err.println(s"r2: $r2")

    //pick random points to splice
    val leftTail:List[Char] = p1.takeWhile(_ != r1).toList
    val rightTail:List[Char] = p1.dropWhile(_ != r2).toList

    Console.err.println(s"left tail: ${leftTail.mkString("")}")
    Console.err.println(s"right tail: ${rightTail.mkString("")}")

    //copy of p2 resistors and structure
    //val p1Resistors = leftTail.toSet.union(rightTail.toSet)
    var middle = p2.filter(c => (resistorsWithBoundsSet.contains(c) && !p1Resistors.contains(c)) || operatorSet.contains(c))
    var previousLength = middle.length+1

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
      middle = middle.clean()
    }

    (leftTail ::: middle.toList ::: rightTail).mkString("")

  }


}