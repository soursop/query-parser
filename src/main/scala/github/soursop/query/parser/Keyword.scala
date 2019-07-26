package github.soursop.query.parser

trait Keyword

object Keyword {
  object op extends Keyword {
    def unapply(s: String): Boolean = {
      0 < s.length && s(0) == '('
    }
  }

  object ed extends Keyword {
    def unapply(s: String): Boolean = {
      0 < s.length && s(0) == ')'
    }
  }

  object in extends Keyword {
    def unapply(s: String): Boolean = {
      0 < s.length && s(0) == ','
    }
  }

  object & extends Keyword {
    def unapply(s: String): Boolean = {
      0 < s.length && s(0) == '&'
    }
  }

  object and extends Keyword {
    def unapply(s: String): Boolean = {
      2 < s.length && s.substring(0, 3).toUpperCase.equals("AND")
    }
  }

  object or extends Keyword {
    def unapply(s: String): Boolean = {
      1 < s.length && s.substring(0, 2).toUpperCase.equals("OR")
    }
  }

  object not extends Keyword {
    def unapply(s: String): Boolean = {
      2 < s.length && s.substring(0, 3).toUpperCase.equals("NOT")
    }
  }
}