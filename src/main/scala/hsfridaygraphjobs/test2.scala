package hsfridaygraphjobs

import scala.collection.mutable.Set

object test2 extends App {

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
  val node14 = new Node("14", 0)

  node1.addNeighbors(node2, node4)
  node2.addNeighbors(node1, node3, node5)
  node3.addNeighbors(node3, node11)
  node4.addNeighbors(node1, node5, node7)
  node5.addNeighbors(node2, node4, node6, node8)
  node6.addNeighbors(node5, node10, node9)
  node7.addNeighbors(node4, node13)
  node8.addNeighbors(node14, node5)
  node9.addNeighbors(node6, node12)
  node10.addNeighbors(node11, node6)
  node11.addNeighbors(node3, node10)
  node12.addNeighbors(node9)
  node13.addNeighbors(node7, node14)
  node14.addNeighbors(node8, node13)

  val graph = new MyGraph(node1, node2, node3, node4, node5, node6, node7, node8, node9, node10, node11, node12, node13, node14)

  def walkGraph(graph: MyGraph, storage: Storage): String = {
    val graphNodes = Set() ++ graph.nodes
    val startNode = graph.nodes(0)
    graphNodes -= startNode
    var visitedNodes: Set[Node] = Set()
    val nodesToBeVisited = storage
    visitedNodes += startNode
    var path: Vector[Node] = Vector()

    startNode.myNeighbors.foreach(x => nodesToBeVisited.put(x))
    println(storage)
    var count = 0
    while (!graphNodes.isEmpty) {
      val currentNode = storage.get
      path = path :+ currentNode
      visitedNodes += currentNode
      currentNode.myNeighbors.filter(x => !visitedNodes.contains(x)).foreach { x => nodesToBeVisited.put(x) }
      graphNodes.remove(currentNode)

    }
    return path.toString
  }

  println(walkGraph(graph, new MyQueue))

}