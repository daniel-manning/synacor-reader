package models

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class ValueSpec extends AnyFreeSpec with Matchers {

  "A value" - {
    "when added to another value will be modulo 32768" in {
      Value(32758) + Value(15) mustBe Value(5)
    }

    "must be greater than 0" ignore {
      intercept[IllegalArgumentException](Value(-1))
    }

    "must be smaller than 32768" ignore {
      intercept[IllegalArgumentException](Value(32888))
    }
  }

}
