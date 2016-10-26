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

      def getPriority(c: Option[Char], currentParen:Char, defaultPriority:Int): Int = {
        c.map {
          case p if p == ')' && currentParen == '(' => 1
          case p if p == '(' && currentParen == ')' => 1
          case p if "()".contains(currentParen) && resistorSet.contains(p) => 1
          case p if p == '+' => 2
          case _ => defaultPriority
        }.getOrElse(defaultPriority)
      }

      GlobalLogger.logger.trace(s"Starting Genome: $genome")

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

      GlobalLogger.logger.trace(s"New Genome: ${newGenomeHead + oldGenome}") // + newGenomeTail}")
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
      val idx1 = getIndexForDelete('(', genome)
      if (idx1 < 0)
        return genome
      val idx2 = getMatchingIdx(genome.drop(idx1))+idx1
      (genome.take(idx1) + genome.slice(idx1 + 1, idx2) + genome.drop(idx2 + 1)).clean()
    }

    def deleteSeries():String = {
      val idx = getIndexForDelete('+', genome)
      if (idx >= 0) (genome.take(idx) + genome.drop(idx + 1)).clean() else genome
    }


  }

  def getIndexForDelete(c:Char, genome:String):Int = {
    val chars = genome.filter(_ == c)

    if(chars.length < 1) {
      return -1
    }

    // go to index of random occurrence of the char
    val n = Random.nextInt(chars.length) + 1
    var i = -1
    var count = 0
    while(count != n) {
      i = genome.indexOf(c, i + 1)
      count += 1
    }

    i
  }

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

  def crossover(p1:String, p2:String): String = {

    def addMissingParen(genome:String):String = {
      val leftParen = genome.count(_ == '(')
      val rightParen = genome.count(_ == ')')
      if(leftParen < rightParen) {
        List.fill(rightParen - leftParen)('(').mkString("") + genome
      } else {
        genome + List.fill(leftParen - rightParen)(')').mkString("")
      }
    }

    def chanceForMutation(genome:String)(implicit chance: Double = 0.1):String = {
      //val functions:List[] = List(insertSeries, deleteSeries, insertParallel(), deleteParallel(), swap2Resistors]
      var res = genome
      var roll = Random.nextDouble()
      while (roll < chance) {
        Random.nextInt(5) match {
          case 0 => res = res.insertSeries()
          case 1 => res = res.insertParallel()
          case 2 => res = res.deleteSeries()
            if (Random.nextBoolean()) res = res.insertSeries()
          case 3 => res = res.deleteParallel()
            if (Random.nextBoolean()) res = res.insertParallel()
          case 4=> res = res.swap2Resistors()
        }

        roll = Random.nextDouble()
      }
      res.wrapParallel.clean().cleanParens
    }

    val p1Resistors = p1.filter(c => resistorSet.contains(c))
    GlobalLogger.logger.trace(s"p1 resistors: $p1Resistors")

    val idx1 = Random.nextInt(p1Resistors.length + 1)
    val idx2 = Random.nextInt(p1Resistors.length - idx1 + 1)+idx1

    val r1 = if (idx1 < p1Resistors.length) p1Resistors.charAt(idx1) else '$'
    val r2 = if (idx2 < p1Resistors.length) p1Resistors.charAt(idx2) else '$'

    GlobalLogger.logger.trace(s"idx1: $idx1")
    GlobalLogger.logger.trace(s"idx2: $idx2")
    GlobalLogger.logger.trace(s"p1: $p1")
    GlobalLogger.logger.trace(s"p2: $p2")
    GlobalLogger.logger.trace(s"r1: $r1")
    GlobalLogger.logger.trace(s"r2: $r2")

    //get the left and right tails of the splice points
    val leftTail:List[Char] = p1.takeWhile(_ != r1).toList
    val rightTail:List[Char] = p1.dropWhile(_ != r2).toList

    GlobalLogger.logger.trace(s"left tail: ${leftTail.mkString("")}")
    GlobalLogger.logger.trace(s"right tail: ${rightTail.mkString("")}")

    //copy of p2 resistors and structure
    //val p1Resistors = leftTail.toSet.union(rightTail.toSet)
    val tailResistorsSet = (leftTail ::: rightTail).filter(c => resistorSet.contains(c)).toSet
    var middle = if (idx1 != idx2)
      p2.filter(c => (resistorSet.contains(c) && !tailResistorsSet.contains(c)) || operatorSet.contains(c))
    else ""

    GlobalLogger.logger.trace(s"middle: $middle")

    if (middle.length > 0) {
      var previousLength = middle.length + 1

      // clean
      while (middle.length != previousLength) {
        previousLength = middle.length
        middle = middle.clean()
      }

      // add missing parens
      middle = addMissingParen(middle).cleanParens
      GlobalLogger.logger.trace(s"cleaned middle: $middle")
    }

    var res = (leftTail ::: middle.toList ::: rightTail).mkString("")
    res = addMissingParen(res).wrapParallel.clean().cleanParens  //we need the wrap in case series is at head or tail
    GlobalLogger.logger.trace(s"Crossover: $res")
    res = chanceForMutation(res)
    GlobalLogger.logger.trace(s"Crossover after mutations: $res")
    res
  }


}