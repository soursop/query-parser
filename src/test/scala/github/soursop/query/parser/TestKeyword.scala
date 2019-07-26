package github.soursop.query.parser

import github.soursop.query.parser.Keyword.{and, or}
import org.scalatest.FunSuite

class TestKeyword extends FunSuite {

  test("match") {
    "AnD  " match {
      case o@or() =>
        assert(false)
      case a@and() =>
        assert(a == "AnD  ")
    }
  }

}
