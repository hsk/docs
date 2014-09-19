import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import thih._

class IdSpec extends FlatSpec {
  it should "enumId 1" in {
    Id.enumId(1) shouldBe "v1"
  }

  it should "enumId 33" in {
    Id.enumId(33) shouldBe "v33"
  }
}