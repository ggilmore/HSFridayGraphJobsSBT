package hsfridaygraphjobs

trait Storage {
  var pointer = 0
  protected[Storage] var contents:scala.collection.mutable.MutableList[Node] = new scala.collection.mutable.MutableList[Node]
  def get:Node
  def put(nodeToAdd:Node)
  override def toString:String ={
    "Storage contents: " + this.contents
  }
}