package hsfridaygraphjobs

import scala.annotation.tailrec
import scala.collection.mutable.Set
import scala.io.Source

object CycleFinder extends App{

  val PATH = "src/main/scala/resources/connections.txt"

  class Digraph() {
    var adjacencyList: Map[Int, Set[Int]] = Map()

    def addEdge(v: Int, w: Int) = {
      adjacencyList = adjacencyList + (v -> adjacencyList.getOrElse(v, Set()).+(w))
    }

    def loop(node: Int, evaluating: Set[Int], visited: Set[Int]): (Boolean, Set[Int]) = {
      println(s"loop($node:Option[Int], $evaluating:Set[Int], $visited:Set[Int])")
          if (!visited(node))
                this.adjacencyList.getOrElse(node, Set()).foldLeft(false, Set[Int]()) { case ((bool, newVisitedSet), child) =>
                  println("bool:" + bool)
                println(s"Folding on $node's child: $child")
                val (res, set) = loop(child, evaluating + node, visited + node)
                println(s"Ending Fold on $node's child: $child and result of looping with the child is $res")
                if (!visited(child) && res)  return (true, newVisitedSet union set)
                else if (evaluating(child))  return (true, newVisitedSet)
                else (bool || false, newVisitedSet union set)
          }

      //we will come here either when the node we are evaluating is already visited, or it's a leaf (no children)
      (false, if (!visited(node)) visited + node else visited)
    }

    def hasCycle = (this.adjacencyList.keySet union this.adjacencyList.values.toSeq.flatten.toSet).foldLeft(false, Set[Int]()) { case ((boolean, set), node) => {
      val (res, newSet) = loop(node, Set(), set)
      println(s"res value is $res")
      (boolean || res, newSet union set)
    }
    }
  }

//  class DirectedBFS(directedGraph:Digraph){
//
//    def BFSFindCycle(startingNode:Int, seenBefore:Set[Int] = Set()):Boolean = {
////      def loop(workingNode:Option[Int], evaluating:Set[Int], visited:Set[Int]):(Boolean) = {
////        workingNode match {
////          case Some(node) => {
////            if (!visited(node)){
////              directedGraph.adjacencyList(node).forall{case child =>
////                val (res, newVisited) = loop(Some(child), evaluating + node, visited + node)
////                if (!visited(child) && res) true
////                else if (evaluating(child)) true
////                else false
////              }
////            }
////            else loop(None, evaluating - node, visited)
////          }
////          case None => false
////        }
////      }
////      val result = loop(Some(startingNode), Set(), Set())
////      println(result)
////      result match {
////        case (true, marked) => true
////        case (false, marked) => {
////          val notSeen = (directedGraph.adjacencyList.keySet union directedGraph.adjacencyList.values.flatten.toSet) diff (marked union seenBefore)
////          if (notSeen.isEmpty) false
////          else {
////            println(s"BFSFindCycle(${notSeen.toSeq.head}, ${marked union seenBefore}")
////            BFSFindCycle(notSeen.toSeq.head, marked union seenBefore)
////          }
////        }
////      }
////    }
//
//  }


  val graph = new Digraph
//  graph.addEdge(1, 2)
//  graph.addEdge(1, 3)
//  graph.addEdge(3, 4)
//  graph.addEdge(4, 1)

  createGraphFromFile(graph)
  println(graph.adjacencyList)
  println(graph.hasCycle)

  def createGraphFromFile(graph: Digraph) = {
    Source.fromFile(PATH).getLines().toSeq.foreach(addEdges)
    def addEdges(line: String) = {
      if (line.trim.nonEmpty) {
        val pair = line.split(" ")
        graph.addEdge(pair(0).toInt, pair(1).toInt)
      }

    }
}


}
