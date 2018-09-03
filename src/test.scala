package hdl

import scala.collection.mutable

abstract class Pass extends Function[Node, Node] {
  protected var state = new mutable.HashMap[Node, Node]()

  def map(node: Node): Node = node.mapNodes(a => this(a))

  def apply(node: Node): Node = {
    for (res <- state.get(node)) return res

    val res = map(node)
    if (res != node)
      res.stackTrace = res.stackTrace ::: Node.stackTrace()

    state.put(node, res)
    res
  }
}

abstract class Visitor[T] extends Function[Node, T] {
  protected var state = new mutable.HashMap[Node, T]()

  def visit(node: Node): T

  def apply(node: Node): T = {
    for (res <- state.get(node)) return res

    val res = visit(node)

    state.put(node, res)
    res
  }
}

class IdentityPass extends Pass {
  override def map(node: Node): Node = super.map(node)
}

class IncrementConstants extends Pass {

  override def map(node: Node): Node = node match {
    case n: ConstantNode => new ConstantNode(n.length, n.value + 1)
    case _ => super.map(node)
  }

}

class GateCalculator extends Visitor[Int] {
  var count = 0

  override def visit(node: Node): Int = {
    val cost = node match {
      case n: BinaryNode => 1
      case n: NegateNode => 1
      case _ => 0
    }
    count += cost

    for (n <- node.support)
      this(n)

    count
  }

}

class DepthCalculator extends Visitor[Int] {

  override def visit(node: Node): Int = {
    val cost = node match {
      case n: BinaryNode => 1
      case n: NegateNode => 1
      case _ => 0
    }

    if (node.support.nonEmpty)
      node.support.map(this).max + cost
    else
      cost
  }

}

class InputNode(val module: Module, val len: Int) extends Node {
  var node: Option[Node] = None

  override def length: Int = len
  override def mapNodes(f: Node => Node): Node = this
  override def support: Iterable[Node] = node

  def clear(): Unit = { node = None }

  def :=(n: Node): Unit = {
    assert(node.isEmpty, s"Input $this is already connected to ${node.get}")
    assert(n.length == length, s"Input size mismatch, expected $length got ${n.length}")
    node = Some(n)
  }
}

class OutputNode(val module: Module, val len: Int) extends Node {
  var node: Option[Node] = None

  override def length: Int = len
  override def mapNodes(f: Node => Node): Node = this
  override def support: Iterable[Node] = node

  def :=(n: Node): Unit = {
    assert(node.isEmpty, s"Output $this is already connected to ${node.get}")
    assert(n.length == length, s"Output size mismatch, expected $length got ${n.length}")
    node = Some(n)
  }
}

abstract class Module {
  private var inputsMutable: Vector[InputNode] = Vector[InputNode]()
  private var outputsMutable: Vector[OutputNode] = Vector[OutputNode]()

  protected def input(len: Int): InputNode = {
    val node = new InputNode(this, len)
    inputsMutable :+= node
    node
  }

  protected def output(len: Int): OutputNode = {
    val node = new OutputNode(this, len)
    outputsMutable :+= node
    node
  }

  def inputs: Vector[InputNode] = inputsMutable
  def outputs: Vector[OutputNode] = outputsMutable

  def spec(s: State): Unit = { }
}

trait State extends Function[Node, Int] {
  def apply(node: Node): Int
  def update(node: Node, value: Int): Unit
}

abstract class AddBit extends Module {
  val a = input(1)
  val b = input(1)
  val cin = input(1)

  val r = output(1)
  val cout = output(1)

  override def spec(s: State): Unit = {
    val res = s(a) + s(b) + s(cin)
    s(r) = res & 1
    s(cout) = res >> 1
  }
}

class AddBitImpl extends AddBit {
  r := a ^ b ^ cin
  cout := (a & b) | (b & cin) | (cin & a)
}

abstract class Parity(val length: Int) extends Module {
  val a = input(length)
  val r = output(1)

  override def spec(s: State): Unit = {
    s(r) = a.map(s).reduce(_ ^ _)
  }
}

class LinearParity(length: Int) extends Parity(length) {
  r := a.reduceLeft[Node](_ ^ _)
}

object Util {
  def logReduce(s: Node, f: (Node, Node) => Node): Node = {
    s.length match {
      case 0 => Zero
      case 1 => s(0)
      case _ =>
        val (l, r) = s.splitAt(s.length / 2)
        logReduce(l, f) ^ logReduce(r, f)
    }
  }
}

class LogParity(length: Int) extends Parity(length) {
  r := Util.logReduce(a, (l, r) => l | r)
}

abstract class Adder(val length: Int) extends Module {
  val a = input(length)
  val b = input(length)
  val cin = input(1)

  val r = output(length)
  val cout = output(1)

  override def spec(s: State): Unit = {
    val sum = s(a) + s(b) + s(cin)
    s(r) = sum & ((1 << length) - 1)
    s(cout) = sum >> length
  }
}

class LinearAdder(length: Int) extends Adder(length) {
  {
    var carry: Node = cin
    val bits = for ((ab, bb) <- a zip b) yield {
      val addBit = new AddBitImpl()
      addBit.a := ab
      addBit.b := bb
      addBit.cin := carry
      carry = addBit.cout
      addBit.r
    }

    r := new ConcatNode(bits.toVector)
    cout := carry
  }
}

abstract class Multiplier(val aLength: Int, val bLength: Int) extends Module {
  val a = input(aLength)
  val b = input(bLength)

  val r = output(aLength + bLength)

  override def spec(s: State): Unit = {
    val i = s(a) * s(b)
    s(r) = i
  }
}

class SimpleMultiplier(aLength: Int, bLength: Int) extends Multiplier(aLength, bLength) {
  var carry: Node = Zero
  var result: Node = new ConstantNode(bLength, 0)

  def zeroExtend(node: Node, length: Int): Node = {
    assert(length >= node.length)
    if (length > node.length)
      node ++ new ConstantNode(length - node.length, 0)
    else
      node
  }

  for (bit <- 0 until aLength) {
    val pad = new ConstantNode(bit, 0)
    val aa = pad ++ a
    val bb = zeroExtend(result, bLength + bit)
    val adder: Adder = new LinearAdder(bLength + bit)
    val mask = b(bit)
    val aas = aa.map(_ & mask)
    adder.a := aas
    adder.b := bb
    adder.cin := Zero
    result = adder.r ++ adder.cout
  }

  r := zeroExtend(result, aLength + bLength)
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
      case n: InputNode => eval(n.node.get, state)
      case n: OutputNode => eval(n.node.get, state)
    }

    state.put(node, res)
    res
  }

  class SpecValidator(val state: mutable.HashMap[Node, Int]) extends State {
    def apply(node: Node): Int = eval(node, state)
    def update(node: Node, value: Int): Unit = {
      val ref = this(node)
      assert(ref == value, s"Spec mismatch on node $node, expected $value got $ref")
    }
  }

  val a = new ConstantNode(4, 3)
  val b = new ConstantNode(4, 2)
  val cin = new ConstantNode(1, 0)
  assert(a.length == b.length)
  assert(cin.length == 1)

  val (res, cout) = adder(a, b, cin)
  assert(res.length == a.length)
  assert(cout.length == 1)

  val resI = new IdentityPass()(res)

  assert(resI eq res)

  val res2 = new IncrementConstants()(res)

  val rval = eval(res2, new mutable.HashMap[Node, Int]())
  assert(rval == 8)

  def autoTest(module: Module): Unit = {

    def testInputs(inputs: Vector[InputNode]): Unit = {
      inputs.headOption match {
        case None =>
          val validator = new SpecValidator(new mutable.HashMap[Node, Int]())
          module.spec(validator)
        case Some(input) =>
          val maxValue = 1 << input.len
          for (value <- 0 until maxValue) {
            input.clear()
            input := new ConstantNode(input.length, value)
            testInputs(inputs.tail)
          }
      }
    }

    testInputs(module.inputs)
  }

  autoTest(new AddBitImpl())
  autoTest(new LinearParity(8))
  autoTest(new LogParity(8))
  autoTest(new LinearAdder(4))
  autoTest(new SimpleMultiplier(4, 4))

  println(new GateCalculator()(new LinearParity(32).r))
  println(new GateCalculator()(new LogParity(32).r))
  println(new GateCalculator()(new LinearAdder(32).r))
  println(new GateCalculator()(new SimpleMultiplier(32, 32).r))

  println(new DepthCalculator()(new LinearParity(32).r))
  println(new DepthCalculator()(new LogParity(32).r))
  println(new DepthCalculator()(new LinearAdder(32).r))
  println(new DepthCalculator()(new SimpleMultiplier(32, 32).r))
}

