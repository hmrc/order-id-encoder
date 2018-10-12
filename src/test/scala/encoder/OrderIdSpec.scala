/*
 * Copyright 2018 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package encoder

import encoder.TaxTypes._
import org.apache.commons.lang.StringUtils
import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, OptionValues, TryValues, WordSpecLike}

import scala.collection.JavaConversions._
import scala.util.Success

class OrderIdSpec extends WordSpecLike with Matchers with OptionValues with TryValues with GeneratorDrivenPropertyChecks {

  implicit val disableShrink: Shrink[String] = Shrink(s => Stream.empty)

  "The orderId generator for SA" should {

    implicit val selfAssessmentOrderGenerators: Arbitrary[SelfAssessmentOrder] = Arbitrary((for {
      taxRef <- Gen.containerOfN(10, Gen.numChar)
    } yield OrderId(taxRef.mkString.toUpperCase + "K", SelfAssessment)).suchThat(_.length == 31))


    "render a tax reference in the format we expect" in {
      OrderId("1234567890K", SelfAssessment) should be("1234567890KK1234567890K-AGHQW00")
      OrderId("1234567890", SelfAssessment) should be("1234567890KK1234567890K-AGHQW00")
      OrderId("K1234567890", SelfAssessment) should be("1234567890KK1234567890K-AGHQW00")
    }

    "contain K for the HoD as first character of the transaction reference number" in {
      forAll { id: SelfAssessmentOrder => transactionRefNumber(id) should startWith("K") }
    }

    "contain the tax reference in plain text from character 2 of the transaction reference number onwards" in {
      transactionRefNumber(OrderId("1234567890K", SelfAssessment)).substring(1) should startWith("1234567890K")
    }

    "contain random padding from character 11 till character 14, seperated from the id with a -" in {
      forAll { id: SelfAssessmentOrder => transactionRefNumber(id).substring(12) should startWith regex s"-[A-Z0-9]{2}"}
    }

    "pad with the full 36 random characters available" in {
      val idGenerator = new encoder.OrderIdVersion0(randomSeed, () => 82800000)

      (1 to 360).flatMap { _ =>
        val randomOrderId = idGenerator.generateId("", SelfAssessment)
        randomOrderId.substring(randomOrderId.indexOf("-") + 1, randomOrderId.length - 5)
      }.groupBy(x => x).mapValues(_.length) should have size 36
    }

    "contain time based unique id at character 15 till character 19" in {
      forAll { (time: Time, time2: Time) =>
        whenever ((time / 100) != (time2 / 100)) {
          val orderId1 = OrderIdAtTime(time)("1234567890K", SelfAssessment)
          val orderId2 = OrderIdAtTime(time2)("1234567890K", SelfAssessment)
          timePaddingIn(orderId1) should not be timePaddingIn(orderId2)
        }
      }
    }

    "contain version at character 20 of the transaction reference number" in {
      forAll { id: SelfAssessmentOrder => transactionRefNumber(id).lift(19).value should be('0') }
    }

    "start with the tax reference" in {
      OrderId("1234567890K", SelfAssessment) should be(s"1234567890KK1234567890K-AGHQW00")
    }

    "append K to the tax reference if not provided" in {
      OrderId("1234567890", SelfAssessment) should be(s"1234567890KK1234567890K-AGHQW00")
    }

    "re-generate orderId from Emis Originator reference for SA" in {
      OrderIdGeneratorV0.generateIdFromEmisOriginatorRef("K1097172564K-1L945J0") shouldBe Success("1097172564KK1097172564K-1L945J0")
    }
  }


  "The orderId generator for VAT" should {

    implicit val vatOrders: Arbitrary[VatOrder] = Arbitrary((for {
      taxRef <- Gen.containerOfN(13, Gen.alphaNumChar)
    } yield OrderId(taxRef.mkString.toUpperCase, Vat)).suchThat(_.length >= 13))

    "render a tax reference in the format we expect" in {
      OrderId("1234567891014", Vat) should be("1234567891014V1234567891014-HQW00")
    }

    "contain V for the HoD as first character of the transaction reference number" in {
      forAll { id: VatOrder => transactionRefNumber(id) should startWith("V") }
    }

    "contain the tax reference in plain text from character 2 of the transaction reference number onwards" in {
      transactionRefNumber(OrderId("1234567891014",Vat)).substring(1) should startWith("1234567891014")
    }

    "contain time based unique id at character 15 till character 19" in {
      forAll { (time: Time, time2: Time) =>
        whenever ((time / 100) != (time2 / 100)) {
          val orderId1 = OrderIdAtTime(time)("1234567890123", Vat)
          val orderId2 = OrderIdAtTime(time2)("1234567890123", Vat)
          timePaddingIn(orderId1) should not be timePaddingIn(orderId2)
        }
      }
    }

    "contain version at character 20 of the transaction reference number" in {
      forAll { id: VatOrder => transactionRefNumber(id).lift(19).value should be('0') }
    }

    "start with the taxReference" in {
      OrderId("1234567891014", Vat) should startWith(s"1234567891014")
    }

    "re-generate orderId from Emis Originator reference for VAT" in {
      OrderIdGeneratorV0.generateIdFromEmisOriginatorRef("V9999004491214-93PW0") shouldBe Success("9999004491214V9999004491214-93PW0")
    }

  }

  "The orderId generator for Corporation Tax" should {

    implicit val corporationTaxOrderGenerators: Arbitrary[CorporationTaxOrder] = Arbitrary((for {
      taxRef <- Gen.containerOfN(17, Gen.alphaNumChar)
    } yield OrderId(taxRef.mkString.toUpperCase, CorporationTax)).suchThat(_.length == 37))

    "render a tax reference in the format we expect" in {
      OrderId("1097172564A00108A", CorporationTax) should be("1097172564A00108AA109717256408A-HQW00")
    }

    "contain A for the HoD as first character of the transaction reference number" in {
      forAll { id: CorporationTaxOrder => transactionRefNumber(id) should startWith("A") }
    }

    "contain the squished tax reference in plain text from character 2 of the transaction reference number onwards" in {
      transactionRefNumber(OrderId("1097172564A00108A", CorporationTax)).substring(1) should startWith("109717256408A")
    }

    "contain time based unique id at character 15 till character 19" in {
      forAll { (time: Time, time2: Time) =>
        whenever ((time / 100) != (time2 / 100)) {
          val orderId1 = OrderIdAtTime(time)("1097172564A00108A", CorporationTax)
          val orderId2 = OrderIdAtTime(time2)("1097172564A00108A", CorporationTax)
          timePaddingIn(orderId1) should not be timePaddingIn(orderId2)
        }
      }
    }

    "contain version at character 20 of the transaction reference number" in {
      forAll { id: CorporationTaxOrder => transactionRefNumber(id).lift(19).value should be('0') }
    }

    "start with the taxReference" in {
      OrderId("1097172564A00108A", CorporationTax) should startWith("1097172564A00108A")
    }

    "re-generate orderId from Emis Originator reference for CT" in {
      OrderIdGeneratorV0.generateIdFromEmisOriginatorRef("A109717256408A-tttt0") shouldBe Success("1097172564A00108AA109717256408A-tttt0")
    }


  }

  "order id generator for EPAYE and taxes that use the same reference format (ie P11D, EPAYE penalties...)" should {
    implicit val epayeOrders: Arbitrary[EPayeOrder] = Arbitrary((for {
      districtNumber <- Gen.containerOfN(3, Gen.numChar)
      checkCharater <- Gen.alphaChar
      registerNumber <- Gen.containerOfN(6, Gen.numChar)
      registerAlphaNum <- Gen.containerOfN(2, Gen.alphaNumChar)
      month <- Gen.chooseNum(1, 13) //13 is the magic month for P11D
      year <- Gen.chooseNum(10, 99)
    } yield (Seq.empty[Char] ++ districtNumber ++ Seq('P', checkCharater) ++
        registerNumber ++ registerAlphaNum ++ year.toString.toCharArray ++ StringUtils.leftPad(month.toString, 2, '0').toCharArray).mkString.toUpperCase).suchThat(_.length == 17))

    "starts with plain text tax reference" in {
      forAll { id: EPayeOrder =>
        OrderId(id, EPaye) should startWith(id)
      }
    }

    "contains hod code P as first character of transaction reference number" in {
      forAll { id: EPayeOrder =>
        transactionRefNumber(OrderId(id, EPaye)).head should be('P')
      }
    }

    "contain version at character 20 of the transaction reference number" in {
      forAll { id: EPayeOrder => transactionRefNumber(OrderId(id, EPaye)).lift(19).value should be('0')}
    }

    "contain time based unique id at character 15 till character 19" in {
      forAll { (time: Time, time2: Time) =>
        whenever((time / 100) != (time2 / 100)) {
          val orderId1 = OrderIdAtTime(time)("123PW123456780215", EPaye)
          val orderId2 = OrderIdAtTime(time2)("123PW123456780918", EPaye)
          timePaddingIn(orderId1) should not be timePaddingIn(orderId2)
        }
      }
    }

    "contain the squished tax reference in plain text from character 2 of the transaction reference number onwards" in {
      transactionRefNumber(OrderId("123PW654321781502", EPaye)).substring(1) should startWith("3FWE0VL780F2")
    }

    "contain random padding from character 13 till character 14, separated from the id with a -" in {
      forAll { id: EPayeOrder => transactionRefNumber(OrderId(id, EPaye)).substring(13) should startWith regex s"-[A-Z0-9]{2}"}
    }

    "re-generate orderId from Emis Originator reference for CT" in {
      OrderIdGeneratorV0.generateIdFromEmisOriginatorRef("PRPR823ILT16C-7AYSA0") shouldBe Success("997PR375966LT4212PRPR823ILT16C-7AYSA0")
    }
  }


  "order id (14 characters) generator for Other Taxes and penalties" should {
    implicit val otherTaxOrders: Arbitrary[OtherTaxesOrder] = Arbitrary((for {
      letter <- Gen.alphaChar
      numOrLetter <- Gen.alphaNumChar
      numbers <- Gen.containerOfN(11, Gen.numChar)
    } yield (Seq('X', letter, numOrLetter) ++ numbers).mkString.toUpperCase).suchThat(_.length == 14))

    "starts with plain text tax reference" in {
      forAll { id: OtherTaxesOrder =>
        OrderId(id, OtherTaxes) should startWith(id)
      }
    }
    "contains hod code X as first character of transaction reference number" in {
      forAll { id: OtherTaxesOrder =>
        StringUtils.substringAfter(OrderId(id, OtherTaxes), id).head should be('X')
      }
    }

    "contains the plain tax reference without the first X after hod code" in {
      forAll { id: OtherTaxesOrder =>
        StringUtils.substring(OrderId(id, OtherTaxes), 15) should startWith(id.tail)
      }
    }
    "contain '-' followed by timestamp after 28th character" in {
      forAll { id: OtherTaxesOrder =>
        OrderId(id, OtherTaxes).substring(28) should startWith regex s"-[A-Z0-9]{4}"
      }
    }

    "contain version at character 33 of order id" in {
      forAll { id: OtherTaxesOrder =>
        OrderId(id, OtherTaxes).substring(33).head should be('0')
      }
    }


    "re-generate orderId from Emis Originator reference for CT" in {
      OrderIdGeneratorV0.generateIdFromEmisOriginatorRef("XW512345678901-AYSA0") shouldBe Success("XW512345678901XW512345678901-AYSA0")
    }
  }

  "order id (15 characters) generator for Other Taxes and penalties" should {
    implicit val otherTaxOrders: Arbitrary[OtherTaxesOrder] = Arbitrary((for {
      letter <- Gen.alphaChar
      numbers <- Gen.containerOfN(13, Gen.alphaNumChar)
    } yield (Seq('X', letter) ++ numbers).mkString.toUpperCase).suchThat(_.length == 15))

    "starts with plain text tax reference" in {
      forAll { id: OtherTaxesOrder =>
        OrderId(id, OtherTaxes) should startWith(id)
      }
    }

    "contains hod code X as first character of transaction reference number" in {
      forAll { id: OtherTaxesOrder =>
        StringUtils.substringAfter(OrderId(id, OtherTaxes), id).head should be('X')
      }
    }

    "contains the plain tax reference without the first X after hod code" in {
      forAll { id: OtherTaxesOrder =>
        StringUtils.substring(OrderId(id, OtherTaxes), 16) should startWith(id.tail)
      }
    }

    "contain '-' followed by timestamp after 30th character" in {
      forAll { id: OtherTaxesOrder =>
        OrderId(id, OtherTaxes).substring(30) should startWith regex s"-[A-Z0-9]{3}"
      }
    }

    "re-generate orderId from Emis Originator reference for Other taxes" in {
      OrderIdGeneratorV0.generateIdFromEmisOriginatorRef("XZABCABCABCABCX-BCD0") shouldBe Success("XZABCABCABCABCXXZABCABCABCABCX-BCD0")
    }

    "contain version at OtherTaxesOrder 30 of order id" in {
      forAll { id: OtherTaxesOrder =>
        OrderId(id, OtherTaxes).substring(30) should startWith regex "^-[A-Z0-9]{3}0$"
      }
    }

  }

  "order id generator for SDLT" should {
    implicit val sdltOrders: Arbitrary[SdltReference] = Arbitrary(for {
      numbers <- Gen.containerOfN(9, Gen.numChar)
      letter <- Gen.alphaChar
    } yield numbers.mkString + "M" + letter.toUpper)

    "starts with plain text tax reference, then the HoD, then the reference again (the squished ref is the same as ref) and a hyphen" in {
      forAll { ref: SdltReference =>
        OrderId(ref, StampDutyLandTax) should startWith(ref + "M" + ref + "-")
      }
    }

    "contains random and time-based characters" in {
      OrderId("123456789MA", StampDutyLandTax) should be ("123456789MAM123456789MA-AGHQW00")
    }

    "re-generate orderId from Emis Originator reference" in {
      OrderIdGeneratorV0.generateIdFromEmisOriginatorRef("M123456789MA-AGHQW00") shouldBe Success("123456789MAM123456789MA-AGHQW00")
    }
  }


  type SelfAssessmentOrder = String
  type VatOrder = String
  type EPayeOrder = String
  type OtherTaxesOrder = String
  type CorporationTaxOrder = String
  type SdltReference = String
  type Time = Int
  val OrderIdGeneratorV0 = new encoder.OrderIdVersion0()
  val randomSeed = 4

  // Chosen by roll of a fair dice.
  def OrderIdAtTime(time: Int)(taxReference: String, taxType: TaxType) = new encoder.OrderIdVersion0(randomSeed, () => time).generateId(taxReference, taxType)

  def OrderId = OrderIdAtTime(82800000) _

  def transactionRefNumber(st: String) = st.substring(st.length - 20)

  def timePaddingIn(order: String) = transactionRefNumber(order).substring(15, 19)

  implicit val timeGenerator: Arbitrary[Time] = Arbitrary(Gen.chooseNum(0, 60 * 60 * 24 * 1000))
}
