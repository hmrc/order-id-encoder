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

import java.math.BigInteger

import encoder.Padding._
import org.apache.commons.lang.StringUtils
import org.joda.time.DateTime
import utils.Validations

import scala.util.{Failure, Random, Success, Try}

case class TaxTypeDetails(hodCode: Char)
object TaxTypes extends Enumeration {
  type TaxType = TaxTypeDetails
  val SelfAssessment = TaxTypeDetails('K')
  val Vat = TaxTypeDetails('V')
  val CorporationTax = TaxTypeDetails('A')
  val EPaye = TaxTypeDetails('P')
  val OtherTaxes = TaxTypeDetails('X')
  val StampDutyLandTax = TaxTypeDetails('M')
}


trait OrderGenerator extends Validations {
  def generateId(taxReference: String, taxType: TaxTypes.TaxType): String

  def generateIdFromEmisOriginatorRef(origRef: String): Try[String]
}

class OrderIdVersion0(seed: Int = System.currentTimeMillis().toInt, timeSource: () => Int = () => DateTime.now.millisOfDay().get()) extends OrderGenerator {

  import encoder.TaxTypes._

  val CurrentVersion = 0

  val randomAlphaNumeric = new Random(seed).alphanumeric.filter(!_.isLower).iterator

  def generateId(taxReference: String, taxType: TaxTypes.TaxType) =  taxType match {

    case SelfAssessment =>
      val ref = {
        val refWithoutStartK = if (taxReference.startsWith("K")) taxReference.drop(1) else taxReference
        if (refWithoutStartK.endsWith("K")) refWithoutStartK else refWithoutStartK + "K"
      }
      ref + taxType.hodCode + padRight(ref + "-", 14, randomAlphaNumeric.next) + timestamp(timeSource) + CurrentVersion
    case Vat =>
      taxReference + taxType.hodCode + padRight(taxReference + "-", 14, randomAlphaNumeric.next) + timestamp(timeSource) + CurrentVersion
    case CorporationTax =>
      taxReference + taxType.hodCode + squishCorporationTaxReference(taxReference) + '-' + timestamp(timeSource) + CurrentVersion
    case EPaye =>
      (taxReference + taxType.hodCode + padRight(squishEPayeTaxReference(taxReference) + '-', 14, randomAlphaNumeric.next) + timestamp(timeSource) + CurrentVersion).toUpperCase
    case OtherTaxes =>
      val timePadded = if (taxReference.length == 14) timestamp(timeSource) else timestamp3Char(timeSource)
      taxReference + taxType.hodCode + taxReference.tail + '-' + timePadded + CurrentVersion
    case StampDutyLandTax =>
      taxReference + taxType.hodCode + padRight(taxReference + "-", 14, randomAlphaNumeric.next) + timestamp(timeSource) + CurrentVersion


  }

  def generateIdFromEmisOriginatorRef(emisOrderRef: String): Try[String] = {
    getError(emisOrderRef).map { e => Failure(new IllegalArgumentException(e))}.getOrElse {
      emisOrderRef.head match {
        case SelfAssessment.hodCode => Success(emisOrderRef.substring(1, 12) + emisOrderRef)
        case Vat.hodCode  => Success(emisOrderRef.substring(1, 14) + emisOrderRef)
        case CorporationTax.hodCode => Success(extractOriginalCorporationTaxReference(emisOrderRef) + emisOrderRef)
        case EPaye.hodCode => Success(extractOriginalEPayeTaxReference(emisOrderRef) + emisOrderRef)
        case OtherTaxes.hodCode => Success(extractOriginalOtherTaxReference(emisOrderRef) + emisOrderRef)
        case StampDutyLandTax.hodCode => Success(extractOriginalSdltReference(emisOrderRef) + emisOrderRef)
        case _ => Failure(new IllegalArgumentException(s"Unknown HoD code '${emisOrderRef.head}' for: $emisOrderRef"))
      }
    }
  }

  /**
   * Example: 1097172564A00108A
   * We want to remove the A001, but to be more careful, we'll remove it with an index range.
   */
  def squishCorporationTaxReference(originalTaxReference : String) = originalTaxReference.substring(0, 10) + originalTaxReference.substring(14)


  /**
   * Example: 123PW123456781502
   *
   */
  def squishEPayeTaxReference(originalTaxReference : String) =
                            base36(originalTaxReference.substring(0, 3),2) + originalTaxReference.substring(4,5) +
                              base36(originalTaxReference.substring(5,11), 4) + originalTaxReference.substring(11,13) +
                              base36(originalTaxReference.substring(13,15), 2) + base36(originalTaxReference.substring(15,17), 1)


  /**
   * Example: "A109717256408A-tttt0"  -> "1097172564A00108A"
   */
  def extractOriginalCorporationTaxReference(emisOrderRef : String) = emisOrderRef.substring(1, 11) + "A001" + emisOrderRef.substring(11, 14)

  /**
   * Example: "PRPR823ILT16C-7AYSA0"  -> "997PR375966LT4212"
   */
  def extractOriginalEPayeTaxReference(emisOrderRef : String) = {
    base10(emisOrderRef.substring(1, 3),3) + 'P' + emisOrderRef.substring(3, 4) +
      base10(emisOrderRef.substring(4, 8), 6) + emisOrderRef.substring(8, 10) +
      base10(emisOrderRef.substring(10, 12), 2) + base10(emisOrderRef.substring(12, 13), 2)
  }


  /**
   * Example: "XW512345678901-AYSA0"  -> "XW512345678901"
   * Example: XA1234567890123-YSA0
   */
  def extractOriginalOtherTaxReference(emisOrderRef : String) = 'X' + StringUtils.substringBeforeLast(emisOrderRef, "-").tail

  def extractOriginalSdltReference(emisOrderRef: String) = emisOrderRef.substring(1,12)


  def base36(base10Data:String, targetLength:Int): String = {
    StringUtils.leftPad(new BigInteger(base10Data).toString(36), targetLength, "0")
  }

  def base10(base36Data:String, targetLength:Int): String = {
    StringUtils.leftPad(new BigInteger(base36Data.dropWhile(_=='0').mkString, 36).toString(10), targetLength, "0")
  }
}

object Padding {
  def padRight(st: String, padCount: Int, randomAlphaNumeric: () => Char): String =
    st + List.fill(padCount - st.length)(randomAlphaNumeric()).mkString

  def timestamp(timeSource: () => Int) = {
    val hundredthOfSecondForDay = timeSource() / 100
    java.lang.Long.toString(hundredthOfSecondForDay, 36).padTo(4, '0').toUpperCase
  }

  def timestamp3Char(timeSource: () => Int) = {
    val secondForDay = timeSource() / 10000
    java.lang.Long.toString(secondForDay, 36).padTo(3, '0').toUpperCase
  }
}
