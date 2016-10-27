package app

import org.apache.commons.lang3.builder.HashCodeBuilder

/**
  * Created by Jason on 10/23/2016.
  */
case class SolverResult(genome:String, resistance:Double, repr:String)  {
  override def equals(obj: scala.Any): Boolean = obj match {
    case other: SolverResult => (resistance - other.resistance) < 1E-9 && genome.sorted.equals(other.genome.sorted)
    case _ => false
  }

  override def hashCode(): Int = new HashCodeBuilder()
    .append(genome.sorted)
    .append(resistance).toHashCode
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
