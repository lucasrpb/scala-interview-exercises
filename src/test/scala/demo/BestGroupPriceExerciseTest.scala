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

    assertThrows[RateCannotBelongToMoreThanOneRateGroupException](getBestGroupPrices(rates, prices))

  }

  "it" should " not have rates empty" in {

    val rates = Seq.empty[Rate]

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

    assertThrows[RatesCannotBeEmptyException](getBestGroupPrices(rates, prices))

  }

  "it" should " throw an exception if prices are negative " in {

    val rates = Seq(
      Rate("M1", "Military"),
      Rate("M2", "Military"),
      Rate("S1", "Senior"),
      Rate("S2", "Senior")
    )

    val prices = Seq(
      CabinPrice("CA", "M1", -200.00),
      CabinPrice("CA", "M2", 250.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CA", "S2", 260.00),
      CabinPrice("CB", "M1", 230.00),
      CabinPrice("CB", "M2", 260.00),
      CabinPrice("CB", "S1", 245.00),
      CabinPrice("CB", "S2", 270.00)
    )

    assertThrows[NegativePricesException](getBestGroupPrices(rates, prices))

  }

  "it" should " not have different prices (conflicting ones) for a particular (cabin code, rate code) combination " in {

    val rates = Seq(
      Rate("M1", "Military"),
      Rate("M2", "Military"),
      Rate("S1", "Senior"),
      Rate("S2", "Senior")
    )

    val prices = Seq(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "M1", 100.00),
      CabinPrice("CA", "M2", 250.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CA", "S2", 260.00),
      CabinPrice("CB", "M1", 230.00),
      CabinPrice("CB", "M2", 260.00),
      CabinPrice("CB", "S1", 245.00),
      CabinPrice("CB", "S2", 270.00)
    )

    assertThrows[ConflictingPricesException](getBestGroupPrices(rates, prices))

  }

  "it" should " not have cabin prices referring a non existing rate code " in {

    val rates = Seq(
      Rate("M1", "Military"),
      Rate("M2", "Military"),
      Rate("S1", "Senior"),
      Rate("S2", "Senior")
    )

    val prices = Seq(
      CabinPrice("CA", "M7", 300.00),
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "M1", 100.00),
      CabinPrice("CA", "M2", 250.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CA", "S2", 260.00),
      CabinPrice("CB", "M1", 230.00),
      CabinPrice("CB", "M2", 260.00),
      CabinPrice("CB", "S1", 245.00),
      CabinPrice("CB", "S2", 270.00)
    )

    assertThrows[NonExistingRateCodeException](getBestGroupPrices(rates, prices))

  }

}
