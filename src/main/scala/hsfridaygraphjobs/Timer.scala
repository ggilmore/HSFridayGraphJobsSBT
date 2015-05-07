package hsfridaygraphjobs

/**
 * Created by erisa on 06/05/15.
 */
object Timer extends App{

  val tiles = "abcdefghijklmnopqr"

  val erisaStart = System.nanoTime()
  val erisaSet = ErisaScrabbleSolver.getWordsAndScores(tiles)
  val erisaEnd = (System.nanoTime() - erisaStart)/1000000000.toFloat

  val gStart = System.nanoTime()
  val gSet = ScrabbleSolver.getWordsAndScores(tiles)
  val gEnd = (System.nanoTime() - gStart)/1000000000.toFloat

  println(s"Erisa's time $erisaEnd, Geoffrey's Time $gEnd")
  println(s"numWord: ${erisaSet.size}")

}
