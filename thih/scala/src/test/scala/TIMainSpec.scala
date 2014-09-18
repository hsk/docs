import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import thih._

/**
 * 11.3 Expressions
 * 11.4 Alternatives
 * 11.5 From Types to Type Schemes
 * 11.6 Binding Groups
 */
class TIMainSpec extends FlatSpec {

  import List._
  import Kind._
  import Type._
  import Pred._
  import Subst._
  import TIMonad._
  import Infer._
  import Lit._
  import Pat._
  import Scheme._
  import Assump._
  import TIMain._

  it should "ambiguities" in {
    val tvs = List(Tyvar("a", Star))
    val preds = List(IsIn("Num", tInt),IsIn("B", tInt))
    val ambs = ambiguities(tvs)(preds)
    printf("ambs %s\n", ambs)
  }

  it should "numClasses" in {
    printf("numClasses = \n")
    numClasses.foreach { id =>
      printf("  %s\n", id)
    }
  }

  it should "stdClasses" in {
    printf("stdClasses = \n")
    stdClasses.foreach { id =>
      printf("  %s\n", id)
    }
  }

  it should "test" in {
    val tv = Tyvar("a", Star)
    val preds = List(IsIn("Num", tInt), IsIn("B", tInt))
    printf("a ----\n")
    val amb = (tv, preds)
    printf("b ----\n")
//    val ce = addNumClasses(initialEnv)
//    printf("c ----\n")
//    val ts = candidates(ce)(amb)
//    printf("ts = %s\n", ts)
  }
}
