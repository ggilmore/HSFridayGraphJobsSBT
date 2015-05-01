package hsfridaygraphjobs

import java.io.File

import scala.io.Source

/**
 * Created by gmgilmore on 3/20/15.
 */


object WordLetters extends baseDictionary {

  def letterCount(thisWord: String): Map[Char, Int] = thisWord.foldLeft(baseMap) { case (dict, char) => dict + (char -> (dict(char) + 1)) }

  def canSpellWord(thisWord: String, other: String): Boolean = spellWord(thisWord, other).forall { case (l, c) => c >= 0 }

  def canSpellWord(thisLC: Map[Char, Int], other: Map[Char, Int]): Boolean = spellWord(thisLC, other).forall { case (l, c) => c >= 0 }

  def spellWord(thisWord:String, other: String): Map[Char, Int] = letterCount(thisWord).map {case (l, c) => l -> (letterCount(other)(l) - c)}

  def spellWord(thisLC:Map[Char, Int], other:Map[Char, Int]) = thisLC.map {case (l, c) => l -> (thisLC(l) - other(l))}
}

trait baseDictionary {
  val baseMap = ("abcdefghijljklmnopqurstuvwxyz?" map (l => (l, 0))).toMap
}



