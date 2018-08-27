package hdl

import scala.collection.AbstractSeq

class IndexNodeIterator(val node: Node) extends Iterator[IndexNode] {
  override val length = node.length
  var index = 0

  override def hasNext: Boolean = index < length
  override def next(): IndexNode = {
    val bit = new IndexNode(node, index)
    index += 1
    bit
  }
}

sealed abstract class BinaryOp {
  def eval(a: Int, b: Int): Int
}

object BinaryOp {
  case object And extends BinaryOp {
    override def eval(a: Int, b: Int): Int = a & b
  }
  case object Or extends BinaryOp {
    override def eval(a: Int, b: Int): Int = a | b
  }
  case object Xor extends BinaryOp {
  override def eval(a: Int, b: Int): Int = a ^ b
  }
}

abstract class Node extends IndexedSeq[IndexNode]  {
  def length: Int

  override def iterator: Iterator[IndexNode] = new IndexNodeIterator(this)

  override def apply(index: Int): IndexNode = {
    assert(index >= 0, s"Index must be non-negative, ($index >= 0)")
    assert(index >= 0, s"Index out of bounds, ($index < $size)")
    new IndexNode(this, index)
  }

  def apply(range: Range): Node = {
    if (range.isEmpty) {
      EmptyNode
    } else if (range.step == 1) {
      assert(range.head <= range.last, s"Range must be well-formed (${range.head} <= ${range.last})")
      assert(range.head >= 0, s"Range start must be non-negative, (${range.head} until ${range.last+1} >= 0)")
      assert(range.last < length, s"Range is out of bounds, (${range.head} until ${range.last+1} < $length)")
      new SliceNode(this, range.head, range.length)
    } else {
      new ConcatNode(range.map(this).toVector)
    }
  }

  def &(rhs: Node): Node = new BinaryNode(BinaryOp.And, this, rhs)
  def |(rhs: Node): Node = new BinaryNode(BinaryOp.Or, this, rhs)
  def ^(rhs: Node): Node = new BinaryNode(BinaryOp.Xor, this, rhs)

  def unary_~ : Node =  new NegateNode(this)

  def ++(rhs: Node): Node = new ConcatNode(Vector(this, rhs))

  override def hashCode(): Int = System.identityHashCode(this)
  override def equals(that: Any) = that match {
    case node: Node => this eq node
    case _ => false
  }
}

object EmptyNode extends Node {
  override def length: Int = 0
}

class InputNode(len: Int, val name: String) extends Node {
  override val length: Int = len
}

class ConstantNode(len: Int, val value: Int) extends Node {
  override val length: Int = len
}

class IndexNode(val node: Node, val index: Int) extends Node {
  override def length: Int = 1

  override def iterator: Iterator[IndexNode] = Iterator.single(this)

  override def apply(index: Int): IndexNode = {
    assert(index == 0, s"IndexNode has only one node, $index == 0")
    this
  }

}

class SliceNode(val node: Node, val offset: Int, len: Int) extends Node {
  override val length: Int = len
}

class ConcatNode(val nodes: Vector[Node]) extends Node {
  override val length = nodes.iterator.map(_.length).sum

  override def ++(rhs: Node): Node = new ConcatNode(nodes ++ rhs)
}


class BinaryNode(val op: BinaryOp, val left: Node, val right: Node) extends Node {
  assert(left.length == right.length)

  override val length = left.length
}

class NegateNode(val node: Node) extends Node {
  override val length = node.length
}

