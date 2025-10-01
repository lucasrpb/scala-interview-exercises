package demo

object BestGroupPriceExercise {

  class RatesCannotBeEmptyException extends RuntimeException("Rates cannot be empty!")
  class RateCannotBelongToMoreThanOneRateGroupException(rates: Seq[String])
    extends RuntimeException(s"One rate cannot belong to more than one rate group: ${rates}")

  class NegativePricesException(prices: Seq[CabinPrice])
    extends RuntimeException(s"Cabin prices cannot be negative: ${prices}")

  class ConflictingPricesException(prices: Map[(String, String), Seq[CabinPrice]])
    extends RuntimeException(s"Conflicting prices: ${prices}")

  class NonExistingRateCodeException(rates: Seq[String])
    extends RuntimeException(s"Non existing rate codes: ${rates}")

  case class Rate(rateCode:
                  String, rateGroup: String)
  case class CabinPrice(cabinCode: String,
                        rateCode: String,
                        price: BigDecimal)
  case class BestGroupPrice(cabinCode: String,
                            rateCode: String,
                            price: BigDecimal,
                            rateGroup: String)

  def getBestGroupPrices(rates: Seq[Rate],
                         prices: Seq[CabinPrice]): Seq[BestGroupPrice] = {
    if(rates.isEmpty){
      throw new RatesCannotBeEmptyException()
    }

    val ratesMap = rates.map{r => r.rateCode -> r}.toMap

    val nonExistingRateCodes = prices.filterNot(p => ratesMap.isDefinedAt(p.rateCode))

    if(!nonExistingRateCodes.isEmpty){
      throw new NonExistingRateCodeException(nonExistingRateCodes.map(_.rateCode))
    }

    val rateCounter = rates.filter(r => rates.count(s => r.rateCode.compareTo(s.rateCode) == 0) > 1)
    if(!rateCounter.isEmpty){
      throw new RateCannotBelongToMoreThanOneRateGroupException(rateCounter.map(_.rateCode))
    }

    val negativeCounter = prices.filter(p => p.price < 0)

    if(!negativeCounter.isEmpty){
      throw new NegativePricesException(negativeCounter)
    }

    val conflictingPrices = prices.groupBy(p => p.rateCode -> p.cabinCode).map { case (tuple, list) =>
      tuple -> list.distinctBy(_.price)
    }.filter(_._2.length > 1)

    if(!conflictingPrices.isEmpty){
      throw new ConflictingPricesException(conflictingPrices)
    }


    // Group prices by cabin code to find the list of rates and its rate groups (like a join between rate and cabin price)
    prices.groupBy(_.cabinCode).flatMap { case (cabinCode, listOfRates) =>

      // Group now by rate group to find all the prices for a rate group
      val pricesByGroup = listOfRates.groupBy(p => ratesMap(p.rateCode).rateGroup)

      // For each group find the minimum price and sort it by cabin code and rate
      pricesByGroup.map { case (group, listOfRates) =>
        val min = listOfRates.minBy(_.price)
        BestGroupPrice(cabinCode, min.rateCode, min.price, group)
      }
    }.toSeq.sortBy(e => e.cabinCode -> e.rateCode)
  }

  def main(args: Array[String]): Unit = {

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

    println(getBestGroupPrices(rates, prices).mkString("\n"))

  }

}
