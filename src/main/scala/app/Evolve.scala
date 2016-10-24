package app

import scala.util.Random
import EvolutionOperators._

/**
  * Created by Jason on 10/23/2016.
  */
object Evolve {

  val resistors = "abcdefgh"
  val generationCycles = 50
  val generationPopulation = 1000000
  val targetResistance = 7000


  def generate():String = {
    var r = Random.shuffle(resistors.iterator).mkString("")
    r = r.wrapParallel

    for (i <- 0 until Random.nextInt(generationCycles)) {
      if (Random.nextBoolean()) r = r.insertSeries() else r = r.insertParallel()
    }

    r
  }

  def runGeneration():Unit = {

    val a = List.fill(generationPopulation)(generate()).map(genome => Solver.solve(genome)).sortBy(res => math.abs(res.resistance - targetResistance))

    a.take(20).foreach(r => GlobalLogger.logger.info(r.resistance.toString + ": " + r.repr))

  }

}
