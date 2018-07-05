import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global //za co to odpowiada?
import scala.util.{Failure, Success}

object CheckoutSystem {
  val Products: Map[String, BigDecimal] = Map("Apple" -> 0.60, "Orange" -> 0.25)

  def costOfProducts(boughtProducts: List[String]): Future[BigDecimal] =
    Future(boughtProducts.map(Products(_)).sum.setScale(2, BigDecimal.RoundingMode.HALF_UP))//dlaczego Future? kiedy zostanie obliczona wartość? czy dopiero przy próbie wyświetlenia result, czy może wcześniej?

  def appleOffer(noOfApples: Int): List[String] = {//tutaj np nie ma future? czemu akurat tu nie ma a w innych funkcjach jest?
    val noOfApplesAfterPromo = if(noOfApples == 0) 0
    else noOfApples % 2 match {
      case 0 => noOfApples / 2
      case 1 => (noOfApples + 1) / 2
    }
    List.fill(noOfApplesAfterPromo)("Apple")
  }

  def orangeOffer(noOfOranges: Int): List[String] = {//też bez future
    val noOfOrangesAfterPromo = if(noOfOranges < 3) noOfOranges
    else noOfOranges % 3 match {
      case 0 => noOfOranges * 2 / 3
      case 1 => (noOfOranges * 2 + 1) / 3
      case 2 => (noOfOranges + 1) * 2 / 3
    }
    List.fill(noOfOrangesAfterPromo)("Orange")
  }

  def costWithAppliedOffers(boughtProducts: List[String]): Future[BigDecimal] = {//
    val noOfApples = boughtProducts.count(_ == "Apple")
    val noOfOranges = boughtProducts.count(_ == "Orange")

    val applesAfterOffer = Future(appleOffer(noOfApples))// wywołujesz funkcje której deklaracja jest bez future, ale wywołanie oznaczasz jako future, czy była by różnica jeżeli funkcje zadeklarowałbyś z future ale wywołał ją bez? co by sie stało, jeżeli i tu i tu byłoby future :)?
    val orangesAfterOffer = Future(orangeOffer(noOfOranges))

    val result = for{
      apples <- applesAfterOffer //co oznacza "<-", przypisanie?
      oranges <- orangesAfterOffer
      totalCost <- costOfProducts(apples ++ oranges)// czemu dwa plusy?
    } yield totalCost

    result//jak będziesz miał chwile to na skype sie umowimy zebys mi wytlumaczyl jeszcze to for comprahension
  }

  def printWithPoundSign(value: BigDecimal): String = "\u00a3" + value

  def main(args: Array[String]): Unit = {
    val boughtProducts: List[String] = List("Apple", "Apple", "Orange", "Apple")

    val result = costWithAppliedOffers(boughtProducts)

    result.onComplete{
      case Success(cost) => println("[" + boughtProducts.mkString(", ") + "] => " + printWithPoundSign(cost))
      case Failure(e) => println("Something goes wrong " + e)
    }
  }
}
