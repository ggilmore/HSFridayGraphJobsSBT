package hsfridaygraphjobs

case class Node(name:String = "Dummy", value:Int = 0, isDummy:Boolean = false){
  var myNeighbors:Vector[Node] = Vector()
  def addNeighbors(neighbors:Node*){
    myNeighbors = myNeighbors ++neighbors
  }
} 