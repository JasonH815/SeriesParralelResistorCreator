package app

/**
  * the problem: You have only 8 resistors, with these values: 330, 470, 560, 1200,
  * 2200, 3300, 4700, and 10,000 Ω. Show how to connect them all together to make
  * an RT as close as possible to 7000 Ω. You may use each resistor only one time.
  **
  *Encoding:
  *0000 = 330
  *0001 = 470
  *0010 = 560
  *0011 = 1200
  *0100 = 2200
  *0101 = 3300
  *0110 = 4700
  *0111 = 10000
  *1000 = ( (parallel delimiter)
  *1001 = ) (parallel delimiter
  *1002 = + (series linker)
  *
  *
  *
  */

case class Element(b:String, value:Option[Double], repr:String)


object Element {
  val r330 = Element("0000", Some(330), "330")
  val r470 = Element("0001", Some(470), "470")
  val r560 = Element("0010", Some(560), "560")
  val r1200 = Element("0011", Some(1200), "1200")
  val r2200 = Element("0100", Some(2200), "2200")
  val r3300 = Element("0101", Some(3300), "3300")
  val r4700 = Element("0110", Some(4700), "4700")
  val r10000 = Element("0111", Some(10000), "10000")
  val openParallel = Element("1000", None, "(")
  val closeParallel = Element("1001", None, ")")
  val seriesLinker = Element("1010", None, "+")
  
  def apply(s:String):Element = {
    s match {
      case m if m == r330.b => r330
      case m if m == r330.b => r330
      case m if m == r470.b => r470
      case m if m == r560.b => r560
      case m if m == r1200.b => r1200
      case m if m == r2200.b => r2200
      case m if m == r3300.b => r3300
      case m if m == r4700.b => r4700
      case m if m == r10000.b => r10000
      case m if m == openParallel.b => openParallel
      case m if m == closeParallel.b => closeParallel
      case m if m == seriesLinker.b => seriesLinker
      case _ => throw new IllegalArgumentException("no encoding for " + s)
    }
  }
}

trait EvalState {
  def next(element: Element):EvalState
  val r:Double
  val element:Element
  def evaluate:EvalState = {
    if (this.isInstanceOf[EndState]) {
      this
    } else {
      next(element).evaluate
    }
  }
}

case class Begin(r:Double, input:String) extends EvalState {
  override def next(element: Element):EvalState = {
    GlobalLogger.logger.trace(s"resistance: $r")
    GlobalLogger.logger.trace(s"input: $input\n")
    if (element.value.isDefined) {
      if (tail.isEmpty)
        EndState(r + element.value.get)
      else
        EvalStringExpectSeries(r + element.value.get, tail)
    } else if(element.b == Element.openParallel.b) {
      OpenStack(r, tail, List[Element](Element.openParallel))
    }
    else throw new IllegalStateException("Expected open parallel or number")
  }
  val element = Element(input.take(4))
  val tail = input.drop(4)
}
case class OpenStack(r:Double, input:String, stack:List[Element]) extends EvalState {
  override def next(element: Element): EvalState = {
    GlobalLogger.logger.trace(s"resistance: $r")
    GlobalLogger.logger.trace(s"input: $input")
    GlobalLogger.logger.trace(s"stack: ${stack.mkString(", ")}\n")
    if (element.b == Element.closeParallel.b) {
      EvalStack(r, input.drop(4), stack, List())
    } else {
      OpenStack(r, tail, element :: stack)
    }
  }
  if (input.length < 4) {
    throw new IllegalStateException("Unexpected end of input")
  }
  val element = Element(input.take(4))
  val tail = input.drop(4)
}
case class EvalStack(r:Double, input:String, stack:List[Element], parallel:List[Double]) extends EvalState {
  override def next(element: Element): EvalState = {
    GlobalLogger.logger.trace(s"resistance: $r")
    GlobalLogger.logger.trace(s"input: $input")
    GlobalLogger.logger.trace(s"stack: ${stack.mkString(", ")}")
    GlobalLogger.logger.trace(s"parallel: ${parallel.mkString(", ")}\n")
    if (element.value.isDefined) {
      EvalStack(r, input, tail, element.value.get :: parallel)
    } else if(element.b == Element.seriesLinker.b) {
      EvalStackExpectNum(r, input, tail, parallel.tail, parallel.head)
    } else if(element.b == Element.openParallel.b) {
      val rNext = r + 1/parallel.map(r => 1/r).sum
      if (tail.isEmpty) {
        if (input.isEmpty)
          EndState(rNext)
        else
          Begin(rNext, input)
      } else {
        OpenStack(r, input, Element("-", Some(rNext), rNext.toString) :: tail)
      }
    } else throw new IllegalStateException("Invalid stack character encountered: " + element.repr)

  }
  val element = stack.head
  val tail = stack.tail
}
case class EvalStackExpectNum(r:Double, input:String, stack:List[Element], parallel:List[Double], n:Double) extends EvalState {
  override def next(element: Element): EvalState = {
    GlobalLogger.logger.trace(s"resistance: $r")
    GlobalLogger.logger.trace(s"input: $input")
    GlobalLogger.logger.trace(s"stack: ${stack.mkString(", ")}")
    GlobalLogger.logger.trace(s"parallel: ${parallel.mkString(", ")}")
    GlobalLogger.logger.trace(s"n: $n\n")
    if (element.value.isDefined)
      EvalStack(r, input, tail, n + element.value.get :: parallel)
    else throw new IllegalStateException("A number was expected. Got " + element.repr)
  }
  val element = stack.head
  val tail = stack.tail
}
case class EvalStringExpectSeries(r:Double, input:String) extends EvalState {
  override def next(element: Element): EvalState = {
    GlobalLogger.logger.trace(s"resistance: $r")
    GlobalLogger.logger.trace(s"input: $input\n")
    if (element.b == Element.seriesLinker.b) {
      Begin(r, tail)
    } else throw new IllegalStateException("Expected series linker. Got " + element.repr)
  }
  if (input.length < 4) {
    throw new IllegalStateException("Unexpected end of input")
  }
  val element = Element(input.take(4))
  val tail = input.drop(4)
}
case class EndState(r:Double) extends EvalState {
  override def next(element: Element): EvalState = this
  val element = Element("-", None, "-")
}

class ProblemEncoding(bits:String) {
  val repr = bits.grouped(4).map(e => Element(e).repr).mkString(" ")
  GlobalLogger.logger.info(repr)


  val resistance = Begin(0, bits).evaluate.r
  GlobalLogger.logger.info(resistance.toString)
}
