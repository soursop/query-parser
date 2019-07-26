package github.soursop.query.parser

import github.soursop.query.parser.Operation.Terminal
import org.scalatest.FunSuite

class TestParser extends FunSuite {

  test("terminal") {
    assert(Parser.parsing("a") == Terminal("a"))
  }

  test("break bracket") {
    assert(Parser.parsing("(a)") == Terminal("a"))
  }

  test("query") {
    val query = "user_id='aa' and 2<age or not taxi_id!='cc'"
    println(Parser.parsing(query))
  }
}
