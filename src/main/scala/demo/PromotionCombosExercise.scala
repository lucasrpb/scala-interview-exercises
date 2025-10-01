package demo

/**
 * This exercise constitutes a backtracking category of problem where we explore all the possibilities
 * It can be represented using a graph on which we have to traverse to get the responses
 */
object PromotionCombosExercise {

  case class Promotion(code: String, notCombinableWith: Seq[String])
  case class PromotionCombo(promotionCodes: Seq[String])

  def allCombinablePromotions(promotions: Seq[Promotion]): Seq[PromotionCombo] = {

    val conflictsMap = promotions.map { p =>
      p.code -> p.notCombinableWith
    }.toMap

    def backtrack(current: Set[Promotion],
                  index: Int,
                  results: scala.collection.mutable.Set[Set[Promotion]]): Unit = {

      if(index == promotions.length){

        // Exclude combinations with 1 element (not a combo)
        if(current.size > 1){
          results += current
        }

        return
      }

      // Get current promotion
      val p = promotions(index)

      // If current promotion p does not conflict with any of the promotions in the combination set, add it and
      // continue combining
      if(current.forall(c => !conflictsMap(c.code).exists(x => x.compareTo(p.code) == 0)
        && !conflictsMap(p.code).exists(x => x.compareTo(c.code) == 0))){
        backtrack(current + p, index + 1, results)
      }

      // backtrack to explore new path
      backtrack(current, index + 1, results)
    }

    val results = scala.collection.mutable.Set.empty[Set[Promotion]]

    backtrack(Set.empty[Promotion], 0, results)

    results.toSeq.map{s => PromotionCombo(s.toSeq.sortBy(_.code).map(_.code))}
  }

  def combinablePromotions(promotionCode: String,
                           allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    // Find all combinable sets that include a given promotion
    val combos = allCombinablePromotions(allPromotions)
    combos.filter(_.promotionCodes.contains(promotionCode))
  }

  def main(args: Array[String]): Unit = {

    val promotions = Seq(
      Promotion("P1", Seq("P3")), // P1 is not combinable with P3
      Promotion("P2", Seq("P4", "P5")), // P2 is not combinable with P4 and P5
      Promotion("P3", Seq("P1")), // P3 is not combinable with P1
      Promotion("P4", Seq("P2")), // P4 is not combinable with P2
      Promotion("P5", Seq("P2")) // P5 is not combinable with P2
    )

    println("All Promotion Combinations:")
    println(allCombinablePromotions(promotions))

    println()

    println("Promotion Combinations for promotionCode=\"P1\"")
    println(combinablePromotions("P1", promotions))

    println()

    println("Promotion Combinations for promotionCode=\"P3\"")
    println(combinablePromotions("P3", promotions))
  }

}
