// rotates the sequence x steps right, wrapping around
def rotr[A]( s: Seq[A], x: Int ): Seq[A] =
  s.takeRight( x ) ++ s.dropRight( x )

def checksum( digitTuples: Seq[(Int, Int)] ): Int =
  digitTuples.foldLeft( 0 ){
    case (acc, (d1, d2)) =>
      if ( d1 == d2 ) acc + d1
      else acc
  }

def solver12( s: String, steps: Int ): Int = {
  val digits = s.map( _.asDigit )
  checksum( digits.zip( rotr( digits, steps ) )  )
}

// tests part 1
val p1solver: String => Int =
  solver12( _, 1 )

p1solver( "1122" ) == 3
p1solver( "1111" ) == 4
p1solver( "1234" ) == 0
p1solver( "91212129" ) == 9

// tests part 2
val p2solver: String => Int =
  s => solver12( s, s.length / 2 )

p2solver( "1212" ) == 6
p2solver( "1221" ) == 0
p2solver( "123425" ) == 4
p2solver( "123123" ) == 12
p2solver( "12131415" ) == 4
