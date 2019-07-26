package github.soursop.query.parser

object Operation {

  trait Op {
    def unwrap(): Op = this
  }
  trait Reducer[T <: Op] {
    def + (op: Op): T
  }
  type Copyable = {
    def copy(nodes: List[Op]): Ops
  }

  sealed abstract class Ops(val nodes: List[Op]) extends Op {
    this : Copyable =>
    def + (op: Op): Ops = op match {
      case some: Ops if getClass.equals(some.getClass) =>
        copy(nodes ::: some.asInstanceOf[Ops].nodes)
      case Not(_) => copy(nodes :+ op)
      case some: Ops if some.nodes.length == 1 => copy(nodes :+ some.nodes.head)
      case _ => copy(nodes :+ op)
    }
    override def unwrap(): Op = if (nodes.length == 1) nodes.head else this
    implicit def toCopyable( base: Ops ): Ops with Copyable = base.asInstanceOf[Ops with Copyable]
  }

  case class And(override val nodes: List[Op]) extends Ops(nodes)
  case class Or(override val nodes: List[Op]) extends Ops(nodes)
  case class In(override val nodes: List[Op]) extends Ops(nodes)
  case class Not(override val nodes: List[Op]) extends Ops(nodes)
  case class Terminal(text: String) extends Op

}
