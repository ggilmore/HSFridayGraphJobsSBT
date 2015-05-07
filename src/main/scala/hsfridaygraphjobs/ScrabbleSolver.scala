package hsfridaygraphjobs

import java.io.File
import WordLetters._

import scala.io.Source


case class ScrabbleNode(isWord: Boolean = false, children: Map[Char, ScrabbleNode] = Map()) {
  def put(word:String):ScrabbleNode ={
    val lword = word.toLowerCase
    if (lword.nonEmpty){
      if (this.children.contains(lword.charAt(0))) {
        val newNode:ScrabbleNode = this.children(lword.charAt(0)).put(lword.drop(1))
        ScrabbleNode(children = this.children + (lword.charAt(0) -> newNode), isWord =this.isWord )
      }
      else {
        val newNode:ScrabbleNode = ScrabbleNode().put(lword.drop(1))
        ScrabbleNode(children = this.children + (lword.charAt(0) -> newNode), isWord =this.isWord)
      }
    }
    else ScrabbleNode(isWord = true, this.children)
  }

  def contains(word: String): Boolean = {
    val lword = word.toLowerCase
    if (lword.length > 1) {
      if (children.contains(lword.charAt(0))) this.children(lword.charAt(0)).contains(lword.drop(1))
      else false
    }
    else {
      if (children.contains(lword.charAt(0))) children(lword.charAt(0)).isWord else false
    }
  }
}

object ScrabbleSolver {

  val scores: Map[Char, Int] = Map('a' -> 1, 'c' -> 3, 'b' -> 3, 'e' -> 1, 'd' -> 2, 'g' -> 2,
    'f' -> 4, 'i' -> 1, 'h' -> 4, 'k' -> 5, 'j' -> 8, 'm' -> 3,
    'l' -> 1, 'o' -> 1, 'n' -> 1, 'q' -> 10, 'p' -> 3, 's' -> 1,
    'r' -> 1, 'u' -> 1, 't' -> 1, 'w' -> 4, 'v' -> 4, 'y' -> 4,
    'x' -> 8, 'z' -> 10)

  val lines = Source.fromFile(new File("/Users/erisa/hackerschool/HSFridayGraphJobsSBT/src/main/resources/sowpods.txt")).getLines
  var root:ScrabbleNode = ScrabbleNode()
  for (line <- lines) if (line.nonEmpty) {
    root = root.put(line.trim.toLowerCase)
  }


  def getAllWords(tiles:String, root:ScrabbleNode):Set[String] = {
    def loop(tileLC:Map[Char, Int], currentPath:Seq[Char], currentNode:ScrabbleNode, foundWords:Set[String]):Set[String] = {
      if (!canSpellWord(tileLC, letterCount(currentPath.mkString("")))) foundWords
      else{
        if (currentNode.isWord) (foundWords + currentPath.mkString("")) union
          currentNode.children.foldLeft(Set[String]()){case (s, (l, child)) => s ++ loop(tileLC, currentPath :+ l, child, foundWords)}
        else foundWords union
          currentNode.children.foldLeft(Set[String]()){case (s, (l, child)) => s ++ loop(tileLC, currentPath :+ l, child, foundWords)}
      }
    }
    loop(letterCount(tiles), Seq(), root, Set())
  }

  def getWordsAndScores(tiles:String):Map[String, Int] = {
    val words = getAllWords(tiles, root)
    words.foldLeft(Map[String, Int]()) { case (m, word) => m + (word -> getScore(word)) }
  }

  def getScore(word:String):Int = word.foldLeft(0)(_ + scores(_))

  def powSet(word:String):Set[String] = {
    if (word.length == 1) Set(word,"")
    else powSet(word.drop(1)) union powSet(word.drop(1)).map(x => s"${word.charAt(0)}$x")
  }


}
