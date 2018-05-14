object CheckoutSystem {
  val Products: Map[String, BigDecimal] = Map("Apple" -> 0.60, "Orange" -> 0.25)

  def costOfProducts(boughtProducts: List[String]): BigDecimal =
    boughtProducts.map(Products(_)).sum.setScale(2, BigDecimal.RoundingMode.HALF_UP)

  def appleOffer(boughtProducts: List[String]): List[String] = {
    val noOfApples = boughtProducts.count(_ == "Apple")
    noOfApples % 2 match {
      case 0 if(noOfApples > 0) => boughtProducts.filter(_ != "Apple") ++ List.fill(noOfApples/2)("Apple")
      case 1 => boughtProducts.filter(_ != "Apple") ++ List.fill((noOfApples + 1)/2)("Apple")
      case _ => boughtProducts
    }
  }

  def main(args: Array[String]): Unit = {
    val boughtProducts: List[String] = List("Apple", "Apple", "Orange", "Apple")

    val result = "\u00a3" + costOfProducts(boughtProducts)

    println("[" + boughtProducts.mkString(", ") + "] => " + result)
  }
}
