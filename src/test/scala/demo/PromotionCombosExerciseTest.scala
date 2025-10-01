package demo

import demo.BestGroupPriceExercise._
import demo.PromotionCombosExercise.{Promotion, PromotionCombo, allCombinablePromotions}
import org.scalatest.flatspec.AnyFlatSpec

class PromotionCombosExerciseTest  extends AnyFlatSpec {

  "it" should " match all the promotions combinations for the input promotions provided" in {

    val promotions = Seq(
      Promotion("P1", Seq("P3")), // P1 is not combinable with P3
      Promotion("P2", Seq("P4", "P5")), // P2 is not combinable with P4 and P5
      Promotion("P3", Seq("P1")), // P3 is not combinable with P1
      Promotion("P4", Seq("P2")), // P4 is not combinable with P2
      Promotion("P5", Seq("P2")) // P5 is not combinable with P2
    )

    val result = allCombinablePromotions(promotions)

    val expected = List(PromotionCombo(List("P1", "P2")),
      PromotionCombo(List("P2", "P3")),
      PromotionCombo(List("P1", "P4", "P5")),
      PromotionCombo(List("P3", "P4", "P5")))

    assert(result.forall(s => expected.contains(s)))

  }

  "it" should " produce no combinations for mutually exclusive promotions" in {

    val promotions = Seq(
      Promotion("P1", Seq("P3")),
      Promotion("P3", Seq("P1")),
    )

    assert(allCombinablePromotions(promotions).isEmpty)
  }

  "it" should " not return single promotions as combos " in {

    val promotions = Seq(
      Promotion("P1", Seq("P3")), // P1 is not combinable with P3
      Promotion("P2", Seq("P4", "P5")), // P2 is not combinable with P4 and P5
      Promotion("P3", Seq("P1")), // P3 is not combinable with P1
      Promotion("P4", Seq("P2")), // P4 is not combinable with P2
      Promotion("P5", Seq("P2")) // P5 is not combinable with P2
    )

    assert(!allCombinablePromotions(promotions).exists(_.promotionCodes.length == 1))
  }

  "it" should " return empty for no promotions " in {
    val promotions = Seq.empty[Promotion]
    assert(allCombinablePromotions(promotions).isEmpty)
  }

  "it" should " treat conflicts symmetrically (if P1 is not compatible with P2, P2 is automatically not compatible with P1) " in {
    val promotions = Seq(
      Promotion("P1", Seq("P2")),
      Promotion("P2", Nil),
      Promotion("P3", Nil)
    )

    val results = allCombinablePromotions(promotions)
    val expected = List(PromotionCombo(Seq("P2", "P3")), PromotionCombo(Seq("P1", "P3")))

    assert(results.forall(s => expected.contains(s)))
  }

  "it" should " return combos that are a subset of other combos " in {

    val promotions = Seq(
      Promotion("P1", Seq("P3")), // P1 is not combinable with P3
      Promotion("P2", Seq("P4", "P5")), // P2 is not combinable with P4 and P5
      Promotion("P3", Seq("P1")), // P3 is not combinable with P1
      Promotion("P4", Seq("P2")), // P4 is not combinable with P2
      Promotion("P5", Seq("P2")) // P5 is not combinable with P2
    )

    assert(!allCombinablePromotions(promotions).exists(s => s.promotionCodes == Seq("P1", "P4")))

  }

}
