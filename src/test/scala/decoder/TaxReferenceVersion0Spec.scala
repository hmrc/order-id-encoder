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

package decoder

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{TryValues, Matchers, OptionValues, WordSpecLike}

class TaxReferenceVersion0Spec extends WordSpecLike with Matchers with OptionValues with TryValues with GeneratorDrivenPropertyChecks {

  "TaxReference decoder" should {

    "return failure if the length is not 20 characters" in {
      TaxReferenceVersion0("tooshort").failure.exception should have message "given order ID 'tooshort' was not 20 chars long"

      TaxReferenceVersion0("this is way way too long").failure.exception should have message "given order ID 'this is way way too long' was not 20 chars long"
    }

    "return failure if the last character is not '1'" in {
      TaxReferenceVersion0("K1234567890K00000001").failure.exception should have message "Tax Reference version 0 decoder was given version value '1'"
    }

    "return failure for unknown HoD type" in {
      TaxReferenceVersion0("Z1234567890K00000000").failure.exception should have message "Unknown HoD code 'Z' for: Z1234567890K00000000"
    }
  }

  "When decoding an SA reference the TaxReference decoder" should {

    "extract taxReference from OrderId" in {
      TaxReferenceVersion0("K1234567890K-0W8M3R0").success.value shouldBe "1234567890K"
    }
  }

  "When decoding a VAT reference the TaxReference decoder" should {

    "extract taxReference from OrderId" in {
      TaxReferenceVersion0("V1234567891014-Y5RY0").success.value shouldBe "1234567891014"
    }
  }

  "When decoding a CT reference the TaxReference decoder" should {

    "extract taxReference from OrderId" in {
      TaxReferenceVersion0("A109717256408A-TTTT0").success.value shouldBe "1097172564A00108A"
    }
  }

  "When decoding a EPaye reference the TaxReference decoder" should {

    "decode taxReference from OrderId" in {
      TaxReferenceVersion0("PRPR823ILT16C-7AYSA0").success.value shouldBe "997PR375966LT4212"
      TaxReferenceVersion0("PFBO0T5QKG2RC-YAYSA0").success.value shouldBe "551PO037790KG9912"
      TaxReferenceVersion0("P5JJ8J29IN0A1-6AYSA0").success.value shouldBe "199PJ397953IN1001"
    }
  }

  "When decoding a Other Taxes 14 character reference the TaxReference decoder" should {

    "decode taxReference from OrderId" in {
      TaxReferenceVersion0("XW512345678901-AYSA0").success.value shouldBe "XW512345678901"
      TaxReferenceVersion0("XD512345678901-ABCD0").success.value shouldBe "XD512345678901"
    }
  }


  "When decoding a Other Taxes 15 character reference the TaxReference decoder" should {

    "decode taxReference from OrderId" in {
      TaxReferenceVersion0("XA1234567890123-YSA0").success.value shouldBe "XA1234567890123"
      TaxReferenceVersion0("XZABCABCABCABCX-BCD0").success.value shouldBe "XZABCABCABCABCX"
    }
  }

  "When decoding a SDLT order the TaxReference decoder" should {

    "decode taxReference from OrderId" in {
      TaxReferenceVersion0("M123456789MA-AGHQW00").success.value shouldBe "123456789MA"
    }
  }

}
