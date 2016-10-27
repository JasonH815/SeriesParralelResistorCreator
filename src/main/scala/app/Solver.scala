package app

/**
  * Created by Jason on 10/23/2016.
  */
case class SolverResult(genome:String, resistance:Double, repr:String)  {
  override def equals(obj: scala.Any): Boolean = genome.sorted.equals(obj) && resistance.equals(obj)

  override def hashCode(): Int = genome.sorted.hashCode + resistance.toInt
}

object Solver {

  def solve(genome:String):SolverResult = {
    val repr = genome.map(c => Element(c).repr).mkString(" ")
    //GlobalLogger.logger.debug(repr)

    val resistance = Begin(genome).evaluate.r
    //GlobalLogger.logger.debug(resistance.toString)

    SolverResult(genome, resistance, repr)
  }
}
