import scala.io.Source
import scala.util.control.Breaks
import scala.util.control.Breaks.{break, breakable}

object AdventOfCode {

    def dayOne(input: List[String]): Unit = {
    val amounts = input.map(line => line.toInt).sorted.to(LazyList)
    val amountsRev = amounts.reverse.to(LazyList)

      // Part One
      {
        val solutions1 = for (n1 <- amountsRev;
                              n2 <- amounts if n1 + n2 == 2020)
          yield (n1, n2)
        val (n1, n2) = solutions1.head
        println(s"n1=$n1, n2=$n2, n1*n2=${n1 * n2}")
      }

      // Part Two
      {
        val solutions2 = for (n1 <- amountsRev;
                              n2 <- amounts;
                              n3 <- amounts if n1 + n2 + n3 == 2020)
          yield (n1, n2, n3)
        val (n1, n2, n3) = solutions2.head
        println(s"n1=$n1, n2=$n2, n3=$n3, n1*n2*n3=${n1 * n2 * n3}")
      }
  }

  def main(args: Array[String]): Unit = {
    val day = args(0).toInt
    val input = Source.fromFile(s"input/$day.txt").getLines.toList
    day match {
      case 1 => dayOne(input)
    }
  }
}
