import org.scalatest.{Matchers, WordSpec}

import scala.concurrent.ExecutionContext.Implicits.global

class CheckoutSystemSpec extends WordSpec with Matchers {
  def doubleToBigDecimal(value: Double): BigDecimal = BigDecimal(value).setScale(2, BigDecimal.RoundingMode.HALF_UP)

  "Checkout system" should {
    "return correct cost for input list without offers" in {
      val firstList: List[String] = List("Apple", "Apple")
      val firstCost = doubleToBigDecimal(1.20)
      val secondList: List[String] = List("Apple", "Orange")
      val secondCost = doubleToBigDecimal(0.85)
      val thirdList: List[String] = List("Orange", "Orange")
      val thirdCost = doubleToBigDecimal(0.50)

      val taskList: List[String] = List("Apple", "Apple", "Orange", "Apple")
      val taskResult = doubleToBigDecimal(2.05)

      CheckoutSystem.costOfProducts(firstList).map(_ shouldBe firstCost)
      CheckoutSystem.costOfProducts(secondList).map(_ shouldBe secondCost)
      CheckoutSystem.costOfProducts(thirdList).map(_ shouldBe thirdCost)
      CheckoutSystem.costOfProducts(taskList).map(_ shouldBe taskResult)
    }

    "return correct amount with apples after apply offer" in {
      CheckoutSystem.appleOffer(0) shouldBe List()
      CheckoutSystem.appleOffer(1) shouldBe List("Apple")
      CheckoutSystem.appleOffer(2) shouldBe List("Apple")
      CheckoutSystem.appleOffer(3) shouldBe List.fill(2)("Apple")
      CheckoutSystem.appleOffer(4) shouldBe List.fill(2)("Apple")
    }

    "return correct amount with orangees after apply offer" in {
      CheckoutSystem.orangeOffer(0) shouldBe List()
      CheckoutSystem.orangeOffer(1) shouldBe List("Orange")
      CheckoutSystem.orangeOffer(2) shouldBe List.fill(2)("Orange")
      CheckoutSystem.orangeOffer(3) shouldBe List.fill(2)("Orange")
      CheckoutSystem.orangeOffer(4) shouldBe List.fill(3)("Orange")
      CheckoutSystem.orangeOffer(5) shouldBe List.fill(4)("Orange")
    }

    "return cost for input list with offers" in {
      val taskList: List[String] = List("Apple", "Apple", "Orange", "Apple")
      val taskResult = doubleToBigDecimal(1.45)

      CheckoutSystem.costWithAppliedOffers(taskList).map(_ shouldBe taskResult)
    }

    "return cost with pound sign" in {
      val testCost = doubleToBigDecimal(0.50)
      CheckoutSystem.printWithPoundSign(testCost) shouldBe "\u00a30.50"
    }
  }
}
