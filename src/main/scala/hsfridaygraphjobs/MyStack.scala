package hsfridaygraphjobs

class MyStack extends Storage {

  def get: Node = {
    val node = contents.last
//    contents(contents.length - 1) = new Node(isDummy = true)
//    println("AFTER: " + contents)
    contents = contents.slice(0, contents.length -1)
    node
  }
  def put(nodeToAdd: Node) {
    contents += nodeToAdd
  }

}