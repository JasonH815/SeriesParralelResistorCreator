package app

import java.util.concurrent.atomic.AtomicInteger

import scala.util.{Failure, Random, Success}
import EvolutionOperators._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}


/**
  * Created by Jason on 10/23/2016.
  */
object Evolve {

  val resistors = "abcdefgh"
  val generationCycles = 100
  val generationPopulation = 100000
  val targetResistance = 7000
  val numberOfResultsToKeep = 20
  val numberOfGenerations = 40


  def generate():Future[String] = {
    Future {
      var r = Random.shuffle(resistors.iterator).mkString("")

      for (i <- 0 until Random.nextInt(generationCycles)) {
        if (Random.nextBoolean()) r = r.insertSeries() else r = r.insertParallel()
      }

//    paren reducer check
//      val nominal = Solver.solve(r)
//      val cleaned = Solver.solve(r.cleanParens)
//      if(math.abs(nominal.resistance - cleaned.resistance) > 1E-10)
//        throw new RuntimeException(s"Bad clean paren implementation ${nominal.genome}: ${nominal.resistance} != ${cleaned.genome}: ${cleaned.resistance}")

      r.cleanParens
    }
  }

  def runGeneration():Unit = {
    val ordering = Ordering.by[SolverResult, Double](r => math.abs(r.resistance - targetResistance))
    val best = scala.collection.mutable.TreeSet.empty[SolverResult](ordering)

    var lastSolverResult = SolverResult("", Double.NegativeInfinity, "")
    var size = 0
    var resultProcessCount = 0

    var results = List.empty[Future[SolverResult]]

    for (i <- 0 until generationPopulation) {
      results = results.+:(generate().map{genome => Solver.solve(genome)}.recover{case ex => throw ex})
    }

    Await.result(Future.sequence(results), Duration.Inf).foreach{
      result =>
        if (size < numberOfResultsToKeep) {
          best += result
          size += 1
        } else if (Math.abs(result.resistance - targetResistance) < Math.abs(lastSolverResult.resistance - targetResistance)) {
          best += result
          best.remove(best.lastKey)
          lastSolverResult = best.lastKey
          size = best.size
        }
        resultProcessCount += 1
    }

    GlobalLogger.logger.info(s"results processed ${resultProcessCount} times")
    best.iterator.map(r => r.resistance.toString + ": " + r.repr).foreach(s => GlobalLogger.logger.info(s))

    def runGeneration(): Unit = {
      val bestList = best.iterator.toVector

      for (i <- 0 until generationPopulation) {
        val idx1 = Random.nextInt(bestList.length)
        val idx2 = Random.nextInt(bestList.length)
        val crossResult = Solver.solve(crossover(bestList(idx1).genome, bestList(idx2).genome))

        if (size < numberOfResultsToKeep) {
          best += crossResult
          size += 1
        } else if (Math.abs(crossResult.resistance - targetResistance) < Math.abs(lastSolverResult.resistance - targetResistance)) {
          best += crossResult
          best.remove(best.lastKey)
          lastSolverResult = best.lastKey
          size = best.size
        }
        resultProcessCount += 1
      }

      GlobalLogger.logger.info(s"results processed ${resultProcessCount} times")
      best.iterator.map(r => r.resistance.toString + ": " + r.repr).foreach(s => GlobalLogger.logger.info(s))
    }

    for (i <- 0 until numberOfGenerations) {
      runGeneration()
    }

  }

}
