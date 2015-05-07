package hsfridaygraphjobs

import scala.collection.immutable.HashMap
import scala.io.Source

/**
 * Created by erisa on 05/05/15.
 */
object ErisaScrabbleSolver{

  def getCombinations(letters: List[Char]): Set[String] = {
    //using set to get rid of duplicated strings
    def getCombinationsHelper(letters: List[Char], combinations: Set[String] = Set()): Set[String] = letters match {
      case Nil => combinations
      case head :: tail => {
        val rest = getCombinationsHelper(tail, combinations)
        (Set(head.toString) ++ rest ++ rest.map(substring => head + substring))
      }
    }

    val combinations = getCombinationsHelper(letters)
    combinations.map(string => string.sorted)
  }



  val PATH = "/Users/erisa/hackerschool/HSFridayGraphJobsSBT/src/main/resources/sowpods.txt"


  val dictionary: HashMap[String, List[String]] = createDictionaryMap

def createDictionaryMap: HashMap[String, List[String]] = {
  def addToDictMap(dictMap: HashMap[String, List[String]], line: String): HashMap[String, List[String]] = {
    val word = line.trim.toLowerCase
    val sortedWordHash = word.sorted
    dictMap + (sortedWordHash -> (word :: (if (dictMap.contains(sortedWordHash)) dictMap.get(sortedWordHash).get else List())))
  }
  Source.fromFile(PATH).getLines().toSeq.foldLeft(HashMap[String, List[String]]())(addToDictMap(_,_))
}

  val SCORES: Map[Char, Int] = Map('a' -> 1, 'c' -> 3, 'b' -> 3, 'e' -> 1, 'd' -> 2, 'g' -> 2,
    'f' -> 4, 'i' -> 1, 'h' -> 4, 'k' -> 5, 'j' -> 8, 'm' -> 3,
    'l' -> 1, 'o' -> 1, 'n' -> 1, 'q' -> 10, 'p' -> 3, 's' -> 1,
    'r' -> 1, 'u' -> 1, 't' -> 1, 'w' -> 4, 'v' -> 4, 'y' -> 4,
    'x' -> 8, 'z' -> 10)

  def getAllWords(combinationList: Set[String]): Set[String] = {
    combinationList.filter(combination => dictionary.contains(combination.toLowerCase)) flatMap(validCombination =>
      dictionary.get(validCombination.toLowerCase).get)
  }

  def getWordsAndScores(tiles:String):Map[String, Int] = {
    val words = getAllWords(getCombinations(tiles.toList))
    words.foldLeft(Map[String, Int]()) { case (m, word) => m + (word -> getScore(word)) }
  }

  def getScore(word:String):Int = word.foldLeft(0)(_ + SCORES(_))

}
