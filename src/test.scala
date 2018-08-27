package hdl

import scala.collection.mutable

abstract class Pass {
  protected var state = new mutable.HashMap[Node, Node]()

  def map(node: Node): Node = node.mapNodes(a => this(a))

  def apply(node: Node): Node = {
    for (res <- state.get(node)) return res

    val res = map(node)

    state.put(node, res)
    res
  }
}

object Test extends App {

  def maj3(a: Node, b: Node, c: Node): Node = (a & b) | (b & c) | (c & a)
  def addBit(a: Node, b: Node, c: Node): (Node, Node) = (a ^ b ^ c, maj3(a, b, c))

  def adder(a: Node, b: Node, cin: Node): (Node, Node) = {
    assert(a.length == b.length)

    var carry: Node = cin

    val bits = for ((ab, bb) <- a zip b) yield {
      val (rb, cb) = addBit(ab, bb, carry)
      carry = cb
      rb
    }

    (new ConcatNode(bits.toVector), carry)
  }

  def eval(node: Node, state: mutable.HashMap[Node, Int]): Int = {
    for (res <- state.get(node)) return res

    val res = node match {
      case EmptyNode => 0
      case n: BinaryNode =>
        val lhs = eval(n.left, state)
        val rhs = eval(n.right, state)
        n.op.eval(lhs, rhs)
      case n: NegateNode => ~eval(n.node, state)
      case n: ConcatNode =>
        var shift = 0
        var res = 0
        for (c <- n.nodes) {
          res |= eval(c, state) << shift
          shift += c.length
        }
        res
      case n: IndexNode => (eval(n.node, state) >> n.index) & 1
      case n: SliceNode => (eval(n.node, state) >> n.offset) & ((1 << n.length) - 1)
      case n: ConstantNode => n.value
    }

    state.put(node, res)
    res
  }

  val a = new ConstantNode(4, 3)
  val b = new ConstantNode(4, 2)
  val cin = new ConstantNode(1, 0)
  assert(a.length == b.length)
  assert(cin.length == 1)

  val (res, cout) = adder(a, b, cin)
  assert(res.length == a.length)
  assert(cout.length == 1)

  val rval = eval(res, new mutable.HashMap[Node, Int]())
  assert(rval == 5)

}

