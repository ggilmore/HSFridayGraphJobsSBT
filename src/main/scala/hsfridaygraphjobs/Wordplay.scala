package hsfridaygraphjobs

import java.io.File

import scala.io.Source

/**
 * Created by gmgilmore on 3/20/15.
 */
//object Wordplay extends App{
//
//  val baseMap:Map[Char, Int] = ("abcdefghijljklmnopqurstuvwxyz" map (l => (l,0))).toMap
//
//  val lines = Source.fromFile(new File("src/resources/sowpods.txt")).getLines
//
////  val startTime:Long = System.nanoTime()
////
////
////  val anagramsThing:Map[Map[Char, Int],Set[WordLetters]] = lines.foldLeft(Map[Map[Char, Int], Set[WordLetters]]())(foo)
////
////  println((anagramsThing.filter(_._2.size > 1) map {case (m, wl) => m.foldLeft(0)(_+_._2) -> wl}).toIndexedSeq.sortBy(-_._1).take(1))
////
////  val endTime:Long = System.nanoTime()
////
////  println(s"That took ${(endTime - startTime)/1000000.0} mills.")
//
//  val books = WordLetters("books")
//  val boo = WordLetters("boo")
//  println(books.spellWord(boo))
//
////  val letterDictionary:Map[Char, WordLetters] = lines.foldLeft(Map[Char, WordLetters]())(foo2)
////  println((letterDictionary.toVector.sortBy(-_._2.thisWord.length).take(1)))
//
//  def foo(m: Map[Map[Char, Int], Set[WordLetters]], w:String) =  {
//    if (w.nonEmpty){
//      val trimWord = w.trim.toLowerCase
//      val wl = WordLetters(trimWord)
//      if (m.contains(wl.letterCount)) m + (wl.letterCount -> (m(wl.letterCount) + wl))
//      else m + (wl.letterCount -> Set(wl))
//    }
//    else m
//  }
//
//  def foo2(m: Map[Char, WordLetters], w:String):Map[Char, WordLetters] =  {
//    if (w.nonEmpty){
//      val trimWord = w.trim.toLowerCase
//      val wl = WordLetters(trimWord)
//      var tempMap: Map[Char, WordLetters] = m
//      wl.letterCount.filter(_._2 > 0).foreach { case (l, c) => {
//        if (tempMap.contains(l)) {
//          if (tempMap(l).letterCount(l) < c) tempMap = tempMap + (l -> wl)
//        }
//        else tempMap = tempMap + (l -> wl)
//        }
//      }
//      tempMap
//    }
//    else m
//  }
  

//}
object WordLetters extends baseDictionary {

  def letterCount(thisWord: String): Map[Char, Int] = thisWord.foldLeft(baseMap) { case (dict, char) => dict + (char -> (dict(char) + 1)) }

  def canSpellWord(thisWord: String, other: String): Boolean = spellWord(thisWord, other).forall { case (l, c) => c >= 0 }

  def canSpellWord(thisLC: Map[Char, Int], other: Map[Char, Int]): Boolean = spellWord(thisLC, other).forall { case (l, c) => c >= 0 }

  def spellWord(thisWord:String, other: String): Map[Char, Int] = letterCount(thisWord).map {case (l, c) => l -> (letterCount(thisWord)(l) - c)}

  def spellWord(thisLC:Map[Char, Int], other:Map[Char, Int]) = thisLC.map {case (l, c) => l -> (thisLC(l) - other(l))}
}

trait baseDictionary {
  val baseMap = ("abcdefghijljklmnopqurstuvwxyz" map (l => (l, 0))).toMap
}



