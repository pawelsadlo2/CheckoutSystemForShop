object CheckoutSystem {
  val Products: Map[String, BigDecimal] = Map("Apple" -> 0.60, "Orange" -> 0.25)

  def costOfProducts(boughtProducts: List[String]): BigDecimal =
    boughtProducts.map(Products(_)).sum.setScale(2, BigDecimal.RoundingMode.HALF_UP)

  def main(args: Array[String]): Unit = {
    val boughtProducts: List[String] = List("Apple", "Apple", "Orange", "Apple")

    val result = "\u00a3" + costOfProducts(boughtProducts)

    println("[" + boughtProducts.mkString(", ") + "] => " + result)
  }
}
