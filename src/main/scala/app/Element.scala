package app

/**
  * Created by Jason on 10/23/2016.
  */
case class Element(c:Char, value:Option[Double], repr:String)


object Element {
  val r330 = Element('a', Some(330), "330")
  val r470 = Element('b', Some(470), "470")
  val r560 = Element('c', Some(560), "560")
  val r1200 = Element('d', Some(1200), "1200")
  val r2200 = Element('e', Some(2200), "2200")
  val r3300 = Element('f', Some(3300), "3300")
  val r4700 = Element('g', Some(4700), "4700")
  val r10000 = Element('h', Some(10000), "10000")
  val openParallel = Element('(', None, "(")
  val closeParallel = Element(')', None, ")")
  val seriesLinker = Element('+', None, "+")

  //TODO use element map instead
  def apply(c:Char):Element = {
    c match {
      case m if m == r330.c => r330
      case m if m == r330.c => r330
      case m if m == r470.c => r470
      case m if m == r560.c => r560
      case m if m == r1200.c => r1200
      case m if m == r2200.c => r2200
      case m if m == r3300.c => r3300
      case m if m == r4700.c => r4700
      case m if m == r10000.c => r10000
      case m if m == openParallel.c => openParallel
      case m if m == closeParallel.c => closeParallel
      case m if m == seriesLinker.c => seriesLinker
      case _ => throw new IllegalArgumentException("no encoding for " + c)
    }
  }
}