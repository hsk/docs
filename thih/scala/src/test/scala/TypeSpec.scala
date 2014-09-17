package thih

import org.scalatest.FlatSpec
import org.scalatest.Matchers._


import thih._

class TypeSpec extends FlatSpec {
  import thih.Type._

  it should "test" in {

    def tes(t:Type.Type_, k:Kind.Kind,t2:Type.Type_) {
      Type.typeKind(t) shouldBe k
      t shouldBe t2
    }
    tes(Type.tUnit, Kind.Star, TCon(Tycon("()",Kind.Star)))
    tes(Type.tChar, Kind.Star, TCon(Tycon("Char",Kind.Star)))
    tes(Type.tInt, Kind.Star, TCon(Tycon("Int", Kind.Star)))
    tes(Type.tInteger, Kind.Star, TCon(Tycon("Integer", Kind.Star)))
    tes(Type.tFloat, Kind.Star, TCon(Tycon("Float", Kind.Star)))
    tes(Type.tDouble, Kind.Star, TCon(Tycon("Double", Kind.Star)))
    tes(Type.tList, Kind.Kfun(Kind.Star, Kind.Star),
      TCon(Tycon("List()",Kind.Kfun(Kind.Star,Kind.Star))))

    tes(Type.tArrow, Kind.Kfun(Kind.Star, Kind.Kfun(Kind.Star, Kind.Star)),
      TCon(Tycon("(=>)",Kind.Kfun(Kind.Star,Kind.Kfun(Kind.Star, Kind.Star)))))

    tes(Type.tTuple2, Kind.Kfun(Kind.Star,Kind.Kfun(Kind.Star,Kind.Star)),
      TCon(Tycon("(,)",Kind.Kfun(Kind.Star,Kind.Kfun(Kind.Star,Kind.Star)))))

    val fn_int_int = Type.fn(Type.tInt)(Type.tInt)

    tes(fn_int_int, Kind.Star,
      TAp(TAp(TCon(Tycon("(=>)",
        Kind.Kfun(Kind.Star,Kind.Kfun(Kind.Star,Kind.Star)))),
      TCon(Tycon("Int",Kind.Star))),TCon(Tycon("Int",Kind.Star))))

    val list_int = Type.list(Type.tInt)

    tes(list_int, Kind.Star,
      TAp(TCon(Tycon("List()",Kind.Kfun(Kind.Star,Kind.Star))),
        TCon(Tycon("Int",Kind.Star))))
    
    tes(Type.tString,
      Kind.Star,
      TAp(TCon(Tycon("List()",Kind.Kfun(Kind.Star,Kind.Star))),
        TCon(Tycon("Char",Kind.Star)))
    )

    val pair_int_char = Type.pair(Type.tInt)(Type.tChar)
    tes(pair_int_char, Kind.Star,
      TAp(TAp(TCon(Tycon("(,)",Kind.Kfun(Kind.Star,Kind.Kfun(Kind.Star,Kind.Star)))),
        TCon(Tycon("Int",Kind.Star))),TCon(Tycon("Char",Kind.Star))))

  }

}
