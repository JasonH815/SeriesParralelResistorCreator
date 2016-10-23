import app.{Element, ProblemEncoding}

/**
  * Created by Jason on 10/22/2016.
  */
object Main {

  def main(args: Array[String]): Unit = {
    val a = Element.r330.b + Element.seriesLinker.b + Element.r330.b + Element.seriesLinker.b + Element.r330.b
    val b = Element.openParallel.b  +
            Element.r330.b + Element.seriesLinker.b +
              Element.openParallel.b + Element.r1200.b + Element.r1200.b + Element.closeParallel.b +
            Element.r330.b + Element.seriesLinker.b + Element.r330.b +
            Element.r330.b +
            Element.closeParallel.b
    Console.println(b)
    new ProblemEncoding(b)
  }

}
