package demo

import demo.BestGroupPriceExercise._
import org.scalatest.flatspec.AnyFlatSpec

class BestGroupPriceExerciseTest  extends AnyFlatSpec {

  "it" should "match the best prices among rate groups" in {

    val rates = Seq(
      Rate("M1", "Military"),
      Rate("M2", "Military"),
      Rate("S1", "Senior"),
      Rate("S2", "Senior")
    )

    val prices = Seq(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "M2", 250.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CA", "S2", 260.00),
      CabinPrice("CB", "M1", 230.00),
      CabinPrice("CB", "M2", 260.00),
      CabinPrice("CB", "S1", 245.00),
      CabinPrice("CB", "S2", 270.00)
    )

    assertResult(List(
        BestGroupPrice("CA","M1",200.0, "Military"),
        BestGroupPrice("CA","S1",225.0, "Senior"),
        BestGroupPrice("CB","M1",230.0,"Military"),
        BestGroupPrice("CB","S1",245.0,"Senior")
    ))(getBestGroupPrices(rates, prices))

  }

  "it" should "throw an exception if a rate belongs to more than one rate group" in {

    val rates = Seq(
      Rate("M1", "Military"),
      Rate("M2", "Military"),
      Rate("S1", "Senior"),
      Rate("S2", "Senior"),
      Rate("M1", "Senior"),
    )

    val prices = Seq(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "M2", 250.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CA", "S2", 260.00),
      CabinPrice("CB", "M1", 230.00),
      CabinPrice("CB", "M2", 260.00),
      CabinPrice("CB", "S1", 245.00),
      CabinPrice("CB", "S2", 270.00)
    )

    assertThrows[RuntimeException](getBestGroupPrices(rates, prices))

  }

}
