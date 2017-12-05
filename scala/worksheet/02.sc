type Row = Seq[Int]
type Sheet = Seq[Row]

def parseSheet( s: String ): Sheet = {
  s.split( "\n" ).to[Seq]
    .map( row => row.split( "\\s+" )
      .map( _.toInt )
      .to[Seq]
    )
    .filter( _.nonEmpty )
}

def checksumPart1( sheet: Sheet ): Int =
  sheet.foldLeft( 0 ){
    case (acc, row) =>
      acc + row.max - row.min
  }

def checksumPart2( sheet: Sheet ): Int =
  sheet.foldLeft(0){
    case (acc, row) =>
      val rowWithIndexes = row.zipWithIndex
      val x = for{
        (r1, i1) <- rowWithIndexes
        (r2, i2) <- rowWithIndexes
        if i1 != i2
        if r1 % r2 == 0
      } yield r1 / r2
      acc + x.sum
  }

// tests part1
val sheet =
  """5 1 9 5
    |7 5 3
    |2 4 6 8
  """.stripMargin

checksumPart1( parseSheet( sheet ) ) == 18

// tests part2
val sheet2 =
  """5 9 2 8
    |9 4 7 3
    |3 8 6 5
  """.stripMargin

checksumPart2( parseSheet( sheet2 ) ) == 9
