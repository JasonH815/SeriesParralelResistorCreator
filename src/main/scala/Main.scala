import app.{Element, ProblemEncoding}

/**
  * Created by Jason on 10/22/2016.
  */
object Main {

  def main(args: Array[String]): Unit = {
    val a = "" + Element.r330.c + Element.seriesLinker.c + Element.r330.c + Element.seriesLinker.c + Element.r330.c
    val b = "" + Element.openParallel.c  +
            Element.r330.c + Element.seriesLinker.c +
              Element.openParallel.c + Element.r1200.c + Element.r1200.c + Element.closeParallel.c +
            Element.r330.c + Element.seriesLinker.c + Element.r330.c +
            Element.r330.c +
            Element.closeParallel.c
    Console.println(b)
    new ProblemEncoding(b)
  }

}
