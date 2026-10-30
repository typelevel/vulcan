/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.generic

import vulcan.AvroValue
import vulcan.BaseSpec
import vulcan.CodecSpecHelpers
import vulcan.Schema
import vulcan.generic.examples.*

final class GenericDerivationCodecSpec extends BaseSpec with CodecSpecHelpers {
  describe("Codec") {

    describe("derive") {
      describe("caseClass") {
        describe("schema") {
          it("should encode value classes as underlying type") {
            assertSchemaIs[CaseClassValueClass] {
              """"int""""
            }
          }

          it("should use parameter schema for record field") {
            assertSchemaIs[CaseClassField] {
              """{"type":"record","name":"CaseClassField","namespace":"vulcan.generic.examples","fields":[{"name":"value","type":"int"}]}"""
            }
          }

          it("should support annotation for record field documentation") {
            assertSchemaIs[CaseClassFieldAvroDoc] {
              """{"type":"record","name":"CaseClassFieldAvroDoc","namespace":"vulcan.generic.examples","fields":[{"name":"name","type":"string","doc":"documentation"}]}"""
            }
          }

          it("should support annotation for record documentation") {
            assertSchemaIs[CaseClassAvroDoc] {
              """{"type":"record","name":"CaseClassAvroDoc","namespace":"vulcan.generic.examples","doc":"documentation","fields":[{"name":"value","type":["null","string"]}]}"""
            }
          }

          it("should support annotation for record name") {
            assertSchemaIs[CaseClassAvroName] {
              """{"type":"record","name":"CaseClassOtherName","namespace":"vulcan.generic.examples","fields":[{"name":"otherValue","type":["null","string"]}]}"""
            }
          }

          it("should support annotations for record aliases") {
            assertSchemaIs[CaseClassAvroAlias] {
              """{"type":"record","name":"CaseClassAvroAlias","namespace":"vulcan.generic.examples","fields":[{"name":"value","type":["null","string"],"aliases":["otherValueAlias","secondValueAlias"]}],"aliases":["CaseClassOtherAlias","CaseClassSecondAlias"]}"""
            }
          }

          it("should support annotation for props") {
            assertSchemaIs[CaseClassAvroProps] {
              """{"type":"record","name":"CaseClassAvroProps","namespace":"vulcan.generic.examples","fields":[{"name":"value","type":["null","string"],"prop1":"A","prop2":"B"}]}"""
            }
          }

          it("should capture errors on invalid names") {
            assertSchemaError[CaseClassFieldInvalidName] {
              """Invalid field "-value" in record vulcan.generic.examples.CaseClassFieldInvalidName: must start with a letter or underscore"""
            }
          }

          it(
            "should support annotation for setting explicit default null values for all nullable fields"
          ) {
            assertSchemaIs[CaseClassAvroNullDefault] {
              """{"type":"record","name":"CaseClassAvroNullDefault","namespace":"vulcan.generic.examples","fields":[{"name":"int","type":["null","int"],"default":null},{"name":"long","type":["null","long"],"default":null},{"name":"string","type":["null","string"],"default":null},{"name":"date","type":["null",{"type":"int","logicalType":"date"}],"default":null},{"name":"map","type":["null",{"type":"map","values":"string"}],"default":null},{"name":"caseClassValueClass","type":["null","int"],"default":null},{"name":"sealedTraitEnumDerived","type":["null",{"type":"enum","name":"SealedTraitEnumDerived","namespace":"com.example","symbols":["first","second"]}],"default":null}]}"""
            }
          }

          it(
            "should support annotation for setting explicit default null value for specific field"
          ) {
            assertSchemaIs[CaseClassFieldAvroNullDefault] {
              """{"type":"record","name":"CaseClassFieldAvroNullDefault","namespace":"vulcan.generic.examples","fields":[{"name":"int","type":["null","int"],"default":null},{"name":"long","type":["null","long"]},{"name":"string","type":["null","string"],"default":null},{"name":"date","type":["null",{"type":"int","logicalType":"date"}]},{"name":"map","type":["null",{"type":"map","values":"string"}],"default":null},{"name":"caseClassValueClass","type":["null","int"]},{"name":"sealedTraitEnumDerived","type":["null",{"type":"enum","name":"SealedTraitEnumDerived","namespace":"com.example","symbols":["first","second"]}],"default":null}]}"""
            }
          }

          it(
            "should support parameter annotation overriding case class annotation when setting explicit default null value for specific field"
          ) {
            assertSchemaIs[CaseClassAndFieldAvroNullDefault] {
              """{"type":"record","name":"CaseClassAndFieldAvroNullDefault","namespace":"vulcan.generic.examples","fields":[{"name":"int","type":["null","int"]},{"name":"long","type":["null","long"],"default":null},{"name":"string","type":["null","string"]},{"name":"date","type":["null",{"type":"int","logicalType":"date"}],"default":null},{"name":"map","type":["null",{"type":"map","values":"string"}]},{"name":"caseClassValueClass","type":["null","int"],"default":null},{"name":"sealedTraitEnumDerived","type":["null",{"type":"enum","name":"SealedTraitEnumDerived","namespace":"com.example","symbols":["first","second"]}]}]}"""
            }
          }

          it(
            "should support optional field of a sum type"
          ) {
            assertSchemaIs[CaseClassOptionOfSumType] {
              """{"type":"record","name":"CaseClassOptionOfSumType","namespace":"vulcan.generic.examples","fields":[{"name":"field1","type":["null",{"type":"record","name":"FirstInSealedTraitCaseClassCustom","fields":[{"name":"value","type":"int"}]},"string"]}]}""".stripMargin
            }
          }

        }

        describe("encode") {
          it("should encode value class using encoder for underlying type") {
            assertEncodeIs[CaseClassValueClass](
              CaseClassValueClass(0),
              Right(unsafeEncode(0))
            )
          }

          it("should encode as record") {
            assertEncodeIs[CaseClassField](
              CaseClassField(0),
              Right(AvroValue.Record(Vector(unsafeEncode(0))))
            )
          }

          it("should encode as record with multiple fields") {
            assertEncodeIs[CaseClassTwoFields](
              CaseClassTwoFields("the-name", 0),
              Right(AvroValue.Record(Vector(unsafeEncode("the-name"), unsafeEncode(0))))
            )
          }
        }

        describe("decode") {
          it("should decode value class using decoder for underlying type") {
            assertDecodeIs[CaseClassValueClass](
              unsafeEncode(123),
              Right(CaseClassValueClass(123))
            )
          }

          it("should error if schema is not record") {
            assertDecodeError[CaseClassField](
              unsafeEncode(CaseClassField(123)),
              unsafeSchema[String],
              "Error decoding vulcan.generic.examples.CaseClassField: Got unexpected schema type string, expected schema type record"
            )
          }

          it("should error if value is not record") {
            assertDecodeError[CaseClassField](
              unsafeEncode(123),
              unsafeSchema[CaseClassField],
              "Error decoding vulcan.generic.examples.CaseClassField: Invalid int value for record vulcan.generic.examples.CaseClassField"
            )
          }

          it("should error if any field is missing in writer schema") {
            assertDecodeError[CaseClassField](
              AvroValue.Record(Vector(unsafeEncode(123))),
              Schema.Record(
                Schema.Name("CaseClassField", Some("vulcan.generic.examples")),
                Vector(Schema.Field("other", unsafeSchema[Int]))
              ),
              "Error decoding vulcan.generic.examples.CaseClassField: Record writer schema is missing field 'value'"
            )
          }

          it("should decode as case class") {
            assertDecodeIs[CaseClassField](
              unsafeEncode(CaseClassField(123)),
              Right(CaseClassField(123))
            )
          }
        }
      }

      describe("sealedTrait") {
        describe("schema") {
          it("should encode case classes as records") {
            assertSchemaIs[SealedTraitCaseClass] {
              """[{"type":"record","name":"CaseClassInSealedTrait","namespace":"vulcan.generic.examples","fields":[{"name":"value","type":"int"}]}]"""
            }
          }

          it("should encode case objects as empty records") {
            assertSchemaIs[SealedTraitCaseObject] {
              """[{"type":"record","name":"CaseObjectInSealedTrait","namespace":"vulcan.generic.examples","fields":[]}]"""
            }
          }

          // Preserving existing behaviour from Magnolia for scala 2
          it("should order alternatives alphabetically by class name") {
            assertSchemaIs[SealedTraitCaseClassAvroNamespace](
              """[{"type":"record","name":"FirstInSealedTraitCaseClassAvroNamespace","namespace":"vulcan.generic.examples","fields":[{"name":"value","type":"int"}]},{"type":"record","name":"SecondInSealedTraitCaseClassAvroNamespace","namespace":"com.example","fields":[{"name":"value","type":"string"}]}]"""
            )
          }

          it("should capture errors on nested unions") {
            assertSchemaError[SealedTraitNestedUnion] {
              """Unions cannot directly contain other unions: [["null","int"]]"""
            }
          }
        }

        describe("encode") {
          it("should error if value is not an alternative") {
            assertEncodeError[SealedTraitCaseClassIncomplete](
              SecondInSealedTraitCaseClassIncomplete(0),
              "Exhausted alternatives for union"
            )
          }

          it("should encode with encoder for subtype") {
            val value = CaseClassInSealedTrait(0)
            assertEncodeIs[SealedTraitCaseClass](
              value,
              Right(AvroValue.Union(0, unsafeEncode(value)))
            )
          }
        }

        describe("decode") {
          it("should error if schema is not in union") {
            assertDecodeError[SealedTraitCaseClass](
              unsafeEncode[SealedTraitCaseClass](CaseClassInSealedTrait(0)),
              unsafeSchema[String],
              "Error decoding vulcan.generic.examples.SealedTraitCaseClass: Missing alternative for string in union"
            )
          }

          it("should decode if schema is part of union") {
            assertDecodeIs[SealedTraitCaseClass](
              unsafeEncode(CaseClassInSealedTrait(0)),
              Right(CaseClassInSealedTrait(0)),
              Some(unsafeSchema[CaseClassInSealedTrait])
            )
          }

          it("should error if value is not a union value") {
            assertDecodeError[SealedTraitCaseClass](
              unsafeEncode(123),
              unsafeSchema[SealedTraitCaseClass],
              "Invalid int value for union"
            )
          }

          it("should error if no schema in union with container name") {
            assertDecodeError[SealedTraitCaseObject](
              unsafeEncode[SealedTraitCaseObject](CaseObjectInSealedTrait),
              unsafeSchema[SealedTraitCaseClass],
              "Error decoding vulcan.generic.examples.SealedTraitCaseObject: Missing alternative for record vulcan.generic.examples.CaseClassInSealedTrait in union"
            )
          }

          it("should error if no subtype with container name") {
            assertDecodeError[SealedTraitCaseClass](
              unsafeEncode[SealedTraitCaseObject](CaseObjectInSealedTrait),
              unsafeSchema[SealedTraitCaseObject],
              "Error decoding vulcan.generic.examples.SealedTraitCaseClass: Missing alternative for record vulcan.generic.examples.CaseObjectInSealedTrait in union"
            )
          }

          it("should decode using schema and decoder for subtype") {
            assertDecodeIs[SealedTraitCaseClass](
              unsafeEncode[SealedTraitCaseClass](CaseClassInSealedTrait(0)),
              Right(CaseClassInSealedTrait(0))
            )
          }
        }
      }
    }
  }
}
