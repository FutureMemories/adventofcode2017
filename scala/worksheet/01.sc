// rotates the string one step right, wrapping around
def rotr( s: String ): String =
  s.last + s.init

def checksum( digits: String ): Int =
  digits.zip( rotr( digits ) ).foldLeft( 0 ){
    case (acc, (ch1, ch2)) =>
      if ( ch1 == ch2 ) acc + ch1.asDigit
      else acc
  }

// tests
checksum( "1122" ) == 3
checksum( "1111" ) == 4
checksum( "1234" ) == 0
checksum( "91212129" ) == 9

