/*
 * Copyright 2017 HM Revenue & Customs
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

package decoder

import encoder.{OrderIdVersion0, TaxTypes}
import utils.Validations

import scala.util.{Success, Failure, Try}

object  TaxReferenceVersion0 extends Validations {
  def apply(orderId: String):Try[String] = {
    getError(orderId).map{ e => Failure(new IllegalArgumentException(e)) }.getOrElse{
      orderId.head match {
        case TaxTypes.SelfAssessment.hodCode  => Success(orderId.substring(1, 12))
        case TaxTypes.Vat.hodCode => Success(orderId.substring(1, 14))
        case TaxTypes.CorporationTax.hodCode => Success(new OrderIdVersion0().extractOriginalCorporationTaxReference(orderId))
        case TaxTypes.EPaye.hodCode => Success(new OrderIdVersion0().extractOriginalEPayeTaxReference(orderId))
        case TaxTypes.OtherTaxes.hodCode => Success(new OrderIdVersion0().extractOriginalOtherTaxReference(orderId))
        case TaxTypes.StampDutyLandTax.hodCode => Success(new OrderIdVersion0().extractOriginalSdltReference(orderId))
        case        _ => Failure(new IllegalArgumentException(s"Unknown HoD code '${orderId.head}' for: $orderId"))
      }
    }
  }

}
