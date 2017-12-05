object DC03 {
  def main(args: Array[String]): Unit = {
    assert(distance(2) == 1)
    assert(distance(4) == 1)
    assert(distance(5) == 2)
    assert(distance(10) == 3)
    assert(distance(11) == 2)
    assert(distance(40) == 3)
    assert(distance(50) == 7)
    assert(distance(1024) == 31)
    println(distance(325489))
  }

  def distance(input: Int) = {

    def ceil = Math.ceil(Math.sqrt(input)).intValue()

    def dimension = if (ceil % 2 == 0) ceil + 1 else ceil

    def stepsToOrigin = dimension / 2

    stepsToOrigin + stepsToMiddle(dimension, stepsToOrigin, input)
  }

  private def stepsToMiddle(dimension: Int, stepsToOrigin:Int, number: Int): Int = {

    def centerToCenterStepWidth = stepsToOrigin*2

    def start = dimension*dimension - dimension/2
    def end = (dimension-2)*(dimension-2)

    def centeredSteps = start until end by -  centerToCenterStepWidth
    def stepsToClosestCenter = centeredSteps.map(n => Math.abs(n - number)).min
    stepsToClosestCenter


  }

}