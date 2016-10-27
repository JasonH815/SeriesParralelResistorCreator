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



sealed trait EvalState {
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

case class Begin(input:String) extends EvalState {
  override val r = 0.0
  override def next(element: Element): EvalState = EvalInput(0, "(" + input + ")")
  override val element: Element = Element('_', None, "")
}

case class EvalInput(r:Double, input:String) extends EvalState {
  override def next(element: Element):EvalState = {
    GlobalLogger.logger.trace(s"resistance: $r")
    GlobalLogger.logger.trace(s"input: $input\n")
    if (element.value.isDefined) {
      if (tail.isEmpty)
        EndState(r + element.value.get)
      else
        EvalStringExpectSeries(r + element.value.get, tail)
    } else if(element.c == Element.openParallel.c) {
      OpenStack(r, tail, List[Element](Element.openParallel))
    }
    else throw new IllegalStateException("Expected open parallel or number")
  }
  val element = Element(input.head)
  val tail = input.tail
}

case class OpenStack(r:Double, input:String, stack:List[Element]) extends EvalState {
  override def next(element: Element): EvalState = {
    GlobalLogger.logger.trace(s"resistance: $r")
    GlobalLogger.logger.trace(s"input: $input")
    GlobalLogger.logger.trace(s"stack: ${stack.mkString(", ")}\n")
    if (element.c == Element.closeParallel.c) {
      EvalStack(r, input.tail, stack, List())
    } else {
      OpenStack(r, tail, element :: stack)
    }
  }
  if (input.isEmpty) {
    throw new IllegalStateException("Unexpected end of input")
  }
  val element = Element(input.head)
  val tail = input.tail
}
case class EvalStack(r:Double, input:String, stack:List[Element], parallel:List[Double]) extends EvalState {
  override def next(element: Element): EvalState = {
    GlobalLogger.logger.trace(s"resistance: $r")
    GlobalLogger.logger.trace(s"input: $input")
    GlobalLogger.logger.trace(s"stack: ${stack.mkString(", ")}")
    GlobalLogger.logger.trace(s"parallel: ${parallel.mkString(", ")}\n")
    if (element.value.isDefined) {
      EvalStack(r, input, tail, element.value.get :: parallel)
    } else if(element.c == Element.seriesLinker.c) {
      if (parallel.isEmpty) {
        Console.err.print("wtfbbq")
      }
      EvalStackExpectNum(r, input, tail, parallel.tail, parallel.head)
    } else if(element.c == Element.openParallel.c) {
      val rNext = r + 1/parallel.map(r => 1/r).sum
      if (tail.isEmpty) {
        if (input.isEmpty)
          EndState(rNext)
        else
          EvalStringExpectSeries(rNext, input)
      } else {
        OpenStack(r, input, Element('-', Some(rNext), rNext.toString) :: tail)
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
    if (element.c == Element.seriesLinker.c) {
      EvalInput(r, tail)
    } else throw new IllegalStateException("Expected series linker. Got " + element.repr)
  }
  if (input.isEmpty) {
    throw new IllegalStateException("Unexpected end of input")
  }
  val element = Element(input.head)
  val tail = input.tail
}
case class EndState(r:Double) extends EvalState {
  override def next(element: Element): EvalState = this
  val element = Element('_', None, "_")
}


