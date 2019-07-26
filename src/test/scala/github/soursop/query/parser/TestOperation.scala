package github.soursop.query.parser

import github.soursop.query.parser.Operation.{And, Not, Terminal}
import org.scalatest.FunSuite

class TestOperation extends FunSuite {

  test("commutative") {
    assert(And(Terminal("a") :: Terminal("b") :: Not(Terminal("c") :: Nil) :: Nil)
      == And(Terminal("a") :: Nil) + And(Terminal("b") :: Nil) + Not(Terminal("c") :: Nil))
  }

}
