import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import thih._

class PreSpec extends FlatSpec {

  it should "union" in {
    Pre.union(List(1,2))(List(2,3)) shouldBe
    List(1,2,3)
  }

  it should "intersect" in {
    Pre.intersect(List(1,2))(List(2,3)) shouldBe
    List(2)
  }

	it should "union and intersect" in {
    val a = List(5,4,3,2,1)
    val b = List(4,5,6,7)

    Pre.union(a)(b) shouldBe
    List(3, 2, 1, 4, 5, 6, 7)

    Pre.intersect(a)(b) shouldBe
    List(5, 4)
  }

  it should "nub" in {
    val a = List(1,1,2,2,3,4,5,1)
    Pre.nub(a) shouldBe
    List(5, 4, 3, 2, 1)
  }

  it should "is empty 1" in {
    val a = List()
    Pre.isEmpty(a) shouldBe true
  }

  it should "is empty 2" in {
    val a = List(1)
    Pre.isEmpty(a) shouldBe false
  }

  it should "fold_left1" in {
    val a = List(1,2,3,4,5,6,7,8,9,10)
    val b = Pre.fold_left1[Int]{case(a, b) =>
      a + b
    }(a)

    b shouldBe 55
  }

  it should "deleteFirst" in {
    val a = List(1,2,3,4,3,4,5)
    val b = Pre.deleteFirst(3)(a)
    b shouldBe List(1,2,4,3,4,5)
  }

  it should "diff" in {
    val a = List(1,2,3,4)
    val b = List(3,4,5,6)
    val r = Pre.diff(a)(b)
    r shouldBe List(1,2)
  }

  it should "diff 2" in {
    val a = List(1,2,3,4,3,4)
    val b = List(3,4,5,6,4)
    val r = Pre.diff(a)(b)
    r shouldBe List(1,2,3)
  }

  it should "split3" in {
    val a1 = List((1,10,100),(2,20,200),(3,30,300))
    val (a,b,c) = Pre.split3(a1)
    a shouldBe List(1,2,3)
    b shouldBe List(10,20,30)
    c shouldBe List(100,200,300)
  }

}