package hsfridaygraphjobs

import scala.annotation.tailrec

object CycleFinder extends App{



  class Digraph(){
    var adjacencyList:Map[Int, Set[Int]] = Map()
    def addEdge(v:Int, w:Int) = {
      adjacencyList = adjacencyList + (v -> adjacencyList.getOrElse(v, Set()).+(w))
    }

    def loop(workingNode:Option[Int], evaluating:Set[Int], visited:Set[Int]):Boolean = {
      println(s"loop($workingNode:Option[Int], $evaluating:Set[Int], $visited:Set[Int])")
      workingNode match {
        case Some(node) => {
          if (!visited(node)){
            this.adjacencyList.getOrElse(node, Set()).exists{case child =>
              if (!visited(child) && loop(Some(child), evaluating + node, visited + node)) true
              else if (evaluating(child)) true
              else false
            }
          }
          else loop(None, evaluating - node, visited)
        }
        case None => false
      }
    }

    def hasCycle = (this.adjacencyList.keySet union this.adjacencyList.values.toSeq.flatten.toSet).exists{case node =>loop(Some(node), Set(), Set())}
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
