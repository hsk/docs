package thih

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import thih._

class TypeSpec extends FlatSpec {
  import Type._
  import Kind._

  it should "test" in {

    def tes(t: Type_, k: Kind, t2: Type_) {
      typeKind(t) shouldBe k
      t shouldBe t2
    }
    // パラメータのない型
    tes(tUnit, Star, TCon(Tycon("()", Star)))
    tes(tChar, Star, TCon(Tycon("Char", Star)))
    tes(tInt, Star, TCon(Tycon("Int", Star)))
    tes(tInteger, Star, TCon(Tycon("Integer", Star)))
    tes(tFloat, Star, TCon(Tycon("Float", Star)))
    tes(tDouble, Star, TCon(Tycon("Double", Star)))

    // List[T] のようなパラメータが１つある型
    tes(tList, Kfun(Star, Star),
      TCon(Tycon("List()", Kfun(Star, Star))))

    // T=>F のようなパラメータが２つある型
    tes(tArrow, Kfun(Star, Kfun(Star, Star)),
      TCon(Tycon("(=>)", Kfun(Star, Kfun(Star, Star)))))

    // カンマもT,Fみたいに２つのパラメータが必要
    tes(tTuple2, Kfun(Star, Kfun(Star, Star)),
      TCon(Tycon("(,)", Kfun(Star, Kfun(Star, Star)))))

    // fn関数で２つの型をしていして関数の型を生成出来る
    val fn_int_int = fn(tInt)(tInt)

    // TApが2つある。
    tes(fn_int_int, Star,
      TAp(TAp(TCon(Tycon("(=>)",
        Kfun(Star, Kfun(Star, Star)))),
        TCon(Tycon("Int", Star))), TCon(Tycon("Int", Star))))

    // １つの型を指定してリスト型を生成できる
    val list_int = list(tInt)

    // TApが1つある。
    tes(list_int, Star,
      TAp(TCon(Tycon("List()", Kfun(Star, Star))),
        TCon(Tycon("Int", Star))))

    // tStringはCharのリスト型だ。
    // TApが1つある。
    tes(tString,
      Star,
      TAp(TCon(Tycon("List()", Kfun(Star, Star))),
        TCon(Tycon("Char", Star))))

    // ペアは2つの型をもつのでTApが２つあると。
    val pair_int_char = pair(tInt)(tChar)
    tes(pair_int_char, Star,
      TAp(TAp(TCon(Tycon("(,)", Kfun(Star, Kfun(Star, Star)))),
        TCon(Tycon("Int", Star))), TCon(Tycon("Char", Star))))

    val pair_int = TAp(TCon(Tycon("(,)", Kfun(Star, Kfun(Star, Star)))), TCon(Tycon("Int", Star)))

    tes(pair_int, Kfun(Star, Star),
      TAp(TCon(Tycon("(,)", Kfun(Star, Kfun(Star, Star)))),
        TCon(Tycon("Int", Star))))

  }

}
