package hsfridaygraphjobs

class MyQueue extends Storage {

  def get: Node = {
//    val node = contents(pointer)
//    contents(pointer) = new Node(isDummy = true)
//    if (pointer + 1 <= contents.length - 1) pointer += 1
//    node]
    val node = contents(0)
    contents = contents.drop(1)
    node
    
  }

  def put(nodeToAdd: Node) = {
    contents += nodeToAdd
  }

}



sealed trait AQueue
case class ANode(v: Int, next:ANode) extends AQueue
case class AEmpty() extends AQueue

//object AQueue {
//  def get(q:AQueue): (AQueue,Int) = q match {
//    
//      case AEmpty() => (AEmpty(),O)
//      case ANode(v,next) => (next,v)
//    }
//  }
//  
//}
//
//sealed trait AList
//case class Cons(v:Int, rest:AList)
//case class Empty()
//
//object Hmm {
//  
//def addOne(l:List[Int]): List[Int] = l.map(_+1)
//def sillyAddOne(l:List[Int]): List[Int] = l match {
//  case Nil => List()
////  case hd :: tail => hd +1 :: sillyAddOne(tail)
//  case whateverthefuck => ll.head +1 :: sillyAddOne(l.tail)
//  
//}

//}