package hsfridaygraphjobs

import scala.annotation.tailrec

object CycleFinder extends App{



  class Digraph() {
    var adjacencyList: Map[Int, Set[Int]] = Map()

    def addEdge(v: Int, w: Int) = {
      adjacencyList = adjacencyList + (v -> adjacencyList.getOrElse(v, Set()).+(w))
    }

    def loop(workingNode: Option[Int], evaluating: Set[Int], visited: Set[Int]): (Boolean, Set[Int]) = {
      println(s"loop($workingNode:Option[Int], $evaluating:Set[Int], $visited:Set[Int])")
      workingNode match {
        case Some(node) => {
          if (!visited(node)) {
            this.adjacencyList.getOrElse(node, Set()).foldLeft(false, Set[Int]()) { case ((bool, newVisitedSet), child) =>
              println(s"Folding on $node's child: $child")
              val (res, set) = loop(Some(child), evaluating + node, visited + node + child)
              println(s"Ending Fold on $node's child: $child")
              if (!visited(child) && res) (bool || true, newVisitedSet union set)
              else if (evaluating(child)) (bool || true, newVisitedSet)
              else (bool || false, newVisitedSet union set)
            }
          }
          else loop(None, evaluating - node, visited)
        }
        case None => (false, visited)
      }
    }

    def hasCycle = (this.adjacencyList.keySet union this.adjacencyList.values.toSeq.flatten.toSet).foldLeft(false, Set[Int]()) { case ((boolean, set), node) => {
      val (res, newSet) = loop(Some(node), Set(), set)
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
  graph.addEdge(1, 2)
  graph.addEdge(1, 3)
  graph.addEdge(4, 3)
  graph.addEdge(4, 1)

  println(graph.hasCycle)




}
