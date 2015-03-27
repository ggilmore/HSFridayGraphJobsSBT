package hsfridaygraphjobs

import scala.collection.mutable.{Queue, Set}



object test extends App {

  val node2 = new Node("2", 0)
  val node1: Node = new Node("1", 0)
  val node3: Node = new Node("3", 0)
  val node4: Node = new Node("4", 0)
  val node5: Node = new Node("5", 0)
  val node6: Node = new Node("6", 0)
  val node7 = new Node("7", 0)
  val node8 = new Node("8", 0)
  val node9 = new Node("9", 0)
  val node10 = new Node("10", 0)
  val node11: Node = new Node("11", 0)
  val node12: Node = new Node("12", 0)
  val node13 = new Node("13", 0)
  val node14 = new Node("14",0)

//  node2.addNeighbors(node2, node3, node5)
//  node1.addNeighbors(node2, node4)
//  node3.addNeighbors(node2)
//  node4.addNeighbors(node1, node7)
//  node5.addNeighbors(node2, node6, node8)
//  node6.addNeighbors(node5, node10, node9)
//  node7.addNeighbors(node4)
//  node8.addNeighbors(node5)
//  node9.addNeighbors(node6, node12)
//  node10.addNeighbors(node11, node6)
//  node11.addNeighbors(node10)
//  node12.addNeighbors(node9)
  
  node1.addNeighbors(node2, node4)
  node2.addNeighbors(node1, node3, node5)
  node3.addNeighbors(node3, node11)
  node4.addNeighbors(node1, node5, node7)
  node5.addNeighbors(node2, node4, node6, node8)
  node6.addNeighbors(node5, node10, node9)
  node7.addNeighbors(node4, node14)
  node8.addNeighbors(node14, node5)
  node9.addNeighbors(node6, node12)
  node10.addNeighbors(node11, node6)
  node11.addNeighbors(node3, node10)
  node12.addNeighbors(node9)
  node13.addNeighbors(node7, node14)
  node14.addNeighbors(node8, node13)
  
  
  

  def bfs(startNode: Node, target: Node, nodes: Node*): Seq[Node] = {
     var graph = Set() ++nodes
     var nodesToBeVisited:Queue[Node] = new Queue()
     var discoveryMap = scala.collection.mutable.HashMap[Node,Node]()
     val nilNode = new Node("nil", 0)
     nodes.foreach(x=> discoveryMap+=(x -> nilNode))
     discoveryMap += (nilNode -> nilNode)
     nodesToBeVisited +=startNode
     var visitedNodes:Set[Node] = Set()
     var foundTarget = false
     var finalPath:Seq[Node] = Seq()
     while (!graph.isEmpty && !foundTarget && !nodesToBeVisited.isEmpty){
       var currentNode = nodesToBeVisited.dequeue
       graph.remove(currentNode)
       visitedNodes += currentNode
       currentNode.myNeighbors.filter(x => !visitedNodes.contains(x)).foreach { x => 
         discoveryMap(x)= currentNode
         nodesToBeVisited +=x
       } 
       println(nodesToBeVisited)
       if (visitedNodes.contains(target)){
         val parentQueue:Queue[Node] = new Queue()
         parentQueue += target
         var workingNode = discoveryMap(target)
         //walk backwards
         while (workingNode != nilNode){          
             parentQueue += workingNode
             workingNode = discoveryMap(workingNode)
         }        
         finalPath = parentQueue.reverse.toSeq         
         foundTarget = true
       }       
     }
     finalPath
     
  }
  
  println(bfs(node1,node12, node1,node2,node3,node4,node5,node5,node7,node8,node9,node10, node11, node12, node13, node14))
  
   
  
}
  
  
  
