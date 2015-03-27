package hsfridaygraphjobs

import scala.collection.mutable.{HashMap, Queue, Set}
import scala.io.Source

object readlargetextfile extends App {
  val nodeNeighborMapping = HashMap[String, Set[String]]()
  for (line <- Source.fromFile("/Users/gmgilmore/Dropbox/ScalaDev/hsfridaygraphjobs/src/resources/connections.txt").getLines) {
    val pair = line.split(" ")
    val name = pair(0)
    val neighbor = pair(1)
    if (nodeNeighborMapping.contains(name)) nodeNeighborMapping(name).add(neighbor)
    else {
      nodeNeighborMapping.put(name, Set())
      nodeNeighborMapping(name).add(neighbor)
    }
  }
  val nameToNodeInstanceMap = HashMap[String, Node]()
  nodeNeighborMapping.keySet.foreach(x => nameToNodeInstanceMap.put(x, new Node(x, 0)))
  nodeNeighborMapping.keySet.foreach(x => nodeNeighborMapping(x).foreach(neighbor => nameToNodeInstanceMap(x).addNeighbors(nameToNodeInstanceMap(neighbor))))
  val node1 = nameToNodeInstanceMap("1")
  val node2894=  nameToNodeInstanceMap("2894")
  println(bfs(node1, node2894, nameToNodeInstanceMap.values))

  def bfs(startNode: Node, target: Node, nodes: Iterable[Node]): Seq[Node] = {
    var graph = Set() ++ nodes
    var nodesToBeVisited: Queue[Node] = new Queue()
    var discoveryMap = scala.collection.mutable.HashMap[Node, Node]()
    val nilNode = new Node("nil", 0)
    nodes.foreach(x => discoveryMap += (x -> nilNode))
    discoveryMap += (nilNode -> nilNode)
    nodesToBeVisited += startNode
    var visitedNodes: Set[Node] = Set()
    var foundTarget = false
    var finalPath: Seq[Node] = Seq()
    var visitCount = 0
    while (!graph.isEmpty && !foundTarget && !nodesToBeVisited.isEmpty) {
      var currentNode = nodesToBeVisited.dequeue
      visitCount += 1
      graph.remove(currentNode)
      visitedNodes += currentNode
      currentNode.myNeighbors.filter(x => !visitedNodes.contains(x)).foreach { x =>
        discoveryMap(x) = currentNode
        nodesToBeVisited += x
      }
      if (visitedNodes.contains(target)) {
        val parentQueue: Queue[Node] = new Queue
        parentQueue += target
        var workingNode = discoveryMap(target)
        //walk backwards
        while (workingNode != nilNode) {
          parentQueue += workingNode
          workingNode = discoveryMap(workingNode)
        }
        finalPath = parentQueue.reverse.toSeq
        foundTarget = true
      }
    }
    println(visitCount)
    finalPath

  }

}