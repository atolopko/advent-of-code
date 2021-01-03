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

  def dayTwo(input: List[String]): Unit = {
    val entry = raw"^(\d+)-(\d+) (.): (.+)".r

    //    // Part One
    {
      val validPasswds =
        input.filter(_ match {
          case entry(min, max, letter, passwd) =>
            (min.toInt to max.toInt).contains(passwd.count(_ == letter(0)))
        })
      println(s"valid passwords=${validPasswds.length}")
    }

    // Part Two
    {
      val validPasswds =
        input.filter(_ match {
          case entry(pos1, pos2, letter, passwd) =>
            println(passwd.length)
            passwd(pos1.toInt - 1) == letter(0) ^ passwd(pos2.toInt - 1) == letter(0)
        })
      println(s"valid passwords=${validPasswds.length}")
    }
  }

  def dayThree(input: List[String]): Unit = {
    val forest = input.toArray
    val height = forest.length
    val width = forest(0).length

    // Part One
    val dy = 1
    val dx = 3
    val trees =
      for (y <- 0 until height by dy;
           x = (y * dx) % width;
           if forest(y)(x) == '#')
        yield (x,y)
    println(s"trees hit=${trees.length}")
  }

  def main(args: Array[String]): Unit = {
    val day = args(0).toInt
    val input = Source.fromFile(s"input/$day.txt").getLines.toList
    day match {
      case 1 => dayOne(input)
      case 2 => dayTwo(input)
      case 3 => dayThree(input)
    }
  }
}
