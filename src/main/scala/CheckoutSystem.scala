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

  def orangeOffer(boughtProducts: List[String]): List[String] = {
    val noOfOranges = boughtProducts.count(_ == "Orange")
    if(noOfOranges < 3) boughtProducts
    else noOfOranges % 3 match {
      case 0 => boughtProducts.filter(_ != "Orange") ++ List.fill(noOfOranges * 2/3)("Orange")
      case 1 => boughtProducts.filter(_ != "Orange") ++ List.fill((noOfOranges * 2 + 1)/3)("Orange")
      case 2 => boughtProducts.filter(_ != "Orange") ++ List.fill((noOfOranges + 1) * 2 / 3)("Orange")
    }
  }

  def main(args: Array[String]): Unit = {
    val boughtProducts: List[String] = List("Apple", "Apple", "Orange", "Apple")

    val result = "\u00a3" + costOfProducts(boughtProducts)

    println("[" + boughtProducts.mkString(", ") + "] => " + result)
  }
}
