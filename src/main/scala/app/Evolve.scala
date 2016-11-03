package app

import java.util.concurrent.atomic.AtomicInteger

import scala.util.{Failure, Random, Success}
import EvolutionOperators._

import scala.collection.immutable.TreeSet
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}


/**
  * Created by Jason on 10/23/2016.
  */
object Evolve {

  //parameters
  val resistors = "abcdefgh"
  val generationCycles = 10
  val generationPopulation = 10000
  val targetResistance = 7000
  val numberOfResultsToKeep = 1000
  val numberOfGenerations = 100
  val allowDuplicateResistors = false
  val limitLength = 100


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


  def runMainEvolution():Unit = {

    val ordering = Ordering.by[SolverResult, Double]((result) => math.abs(result.resistance - targetResistance))
    var best = TreeSet.empty[SolverResult](ordering)
    var resultsMap = Map.empty[Double, Set[SolverResult]]

    var lastSolverResult = SolverResult("", Double.NegativeInfinity, "")
    var size = 0  // we don't care too much about having atomic access to this, the best list will self-correct
    var resultProcessCount = 0

    def insertBest(result:SolverResult): TreeSet[SolverResult] = {
        resultsMap  = resultsMap +
          (result.resistance ->
            (resultsMap.getOrElse(result.resistance, Set.empty[SolverResult]) + result))
        best + result
      }


    def processResults(results:List[Future[SolverResult]]) = {
      Await.result(Future.sequence(results), Duration.Inf).foreach{
        result =>
          if (size < numberOfResultsToKeep) {
            best += result
            size += 1
          } else if (Math.abs(result.resistance - targetResistance) < Math.abs(lastSolverResult.resistance - targetResistance)) {
            best = insertBest(result)
            best -= best.lastKey
            lastSolverResult = best.lastKey
            size = best.size
          }
          resultProcessCount += 1
      }
    }

    def runGeneration(bestList:Vector[SolverResult]): List[Future[SolverResult]] = {
      var results = List.empty[Future[SolverResult]]
      for (i <- 0 until generationPopulation) {
        val idx1 = Random.nextInt(bestList.length)
        val idx2 = Random.nextInt(bestList.length)
        results = Future {
          Solver.solve(crossover(bestList(idx1).genome, bestList(idx2).genome,
            allowDuplicates = allowDuplicateResistors, limitLength = limitLength))
        }.recover { case ex => {
          Console.err.println(ex)
          SolverResult("", Double.NegativeInfinity, "")
        }} +: results
      }

      results
    }

    // seed generation
    var results = List.empty[Future[SolverResult]]
    for (i <- 0 until generationPopulation) {
      results = results.+:(generate().map{genome => Solver.solve(genome)}.recover{case ex => throw ex})
    }
    processResults(results)
    Console.err.println(s"Seed generation, $resultProcessCount results processed")

    //run generations, they should run sequentially but solve in parallel within a generation
    for (i <- 0 until numberOfGenerations) {
      val bestList = best.iterator.toVector
      processResults(runGeneration(bestList))
      Console.err.println(s"Generation ${i + 1}, $resultProcessCount results processed")
      //best.iterator.take(20).map(r => r.resistance.toString + ": " + r.repr).foreach(s => GlobalLogger.logger.info(s))
    }

    GlobalLogger.logger.info(s"results processed $resultProcessCount times")
    best.iterator.take(50).map(r => r.resistance.toString + ": " + r.repr).foreach(s => GlobalLogger.logger.info(s))
    GlobalLogger.logger.info("list of bst results")
    best.iterator.take(10)
      .flatMap(r => resultsMap(r.resistance))
      .map(r => r.resistance.toString + ": " + r.repr)
      .foreach(s => GlobalLogger.logger.info(s))

  }

}
