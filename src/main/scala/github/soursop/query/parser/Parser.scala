package github.soursop.query.parser

import github.soursop.query.parser.Operation._
import github.soursop.query.parser.Keyword._

object Parser {
  type ParseError = (String, Throwable)

  def parsing(input: String): Op = {
    def search(buffer: String, ops: Ops, open: Int, close: Int, idx: Int): Op = {
      def flush(ops: Ops, value: String): Ops = value.trim match {
        case "" => ops
        case trim => ops + (if (open > 0) parsing(trim) else Terminal(trim))
      }
      if (input.length <= idx) flush(ops, buffer)
      else input.substring(idx) match {
        case and() if open == close => search("", And(Nil) + flush(ops, buffer), open, close, idx + 3)
        case or() if open == close => search("", Or(Nil) + flush(ops, buffer), open, close, idx + 2)
        case not() if open == close => flush(ops, buffer) + search("", Not(Nil), open, close, idx + 3)
        case op() => search(if (open == close) buffer else buffer + input(idx), ops, open + 1, close, idx + 1)
        case ed() => search(if (open == close + 1) buffer else buffer + input(idx), ops, open, close + 1, idx + 1)
        case in() if open == close => search("", In(Nil) + flush(ops, buffer), open, close, idx + 1)
        case _ => search(buffer + input(idx), ops, open, close, idx + 1)
      }
    }
    search("", Or(Nil), 0, 0, 0).unwrap()
  }

}
