/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.generic

import vulcan.AvroValue
import vulcan.BaseSuite
import vulcan.Codec
import vulcan.Schema
import vulcan.generic.examples.*

object GenericDerivationCodecSpec extends BaseSuite {
  pureTest("Codec derive caseClass schema should encode value classes as underlying type") {
    expectSchemaIs[CaseClassValueClass] {
      """"int""""
    }
  }

  pureTest("Codec derive caseClass schema should use parameter schema for record field") {
    expectSchemaIs[CaseClassField] {
      """{"type":"record","name":"CaseClassField","namespace":"vulcan.generic.examples","fields":[{"name":"value","type":"int"}]}"""
    }
  }

  pureTest("Codec derive caseClass schema should support annotation for record field documentation") {
    expectSchemaIs[CaseClassFieldAvroDoc] {
      """{"type":"record","name":"CaseClassFieldAvroDoc","namespace":"vulcan.generic.examples","fields":[{"name":"name","type":"string","doc":"documentation"}]}"""
    }
  }

  pureTest("Codec derive caseClass schema should support annotation for record documentation") {
    expectSchemaIs[CaseClassAvroDoc] {
      """{"type":"record","name":"CaseClassAvroDoc","namespace":"vulcan.generic.examples","doc":"documentation","fields":[{"name":"value","type":["null","string"]}]}"""
    }
  }

  pureTest("Codec derive caseClass schema should support annotation for record name") {
    expectSchemaIs[CaseClassAvroName] {
      """{"type":"record","name":"CaseClassOtherName","namespace":"vulcan.generic.examples","fields":[{"name":"otherValue","type":["null","string"]}]}"""
    }
  }

  pureTest("Codec derive caseClass schema should support annotations for record aliases") {
    expectSchemaIs[CaseClassAvroAlias] {
      """{"type":"record","name":"CaseClassAvroAlias","namespace":"vulcan.generic.examples","fields":[{"name":"value","type":["null","string"],"aliases":["otherValueAlias","secondValueAlias"]}],"aliases":["CaseClassOtherAlias","CaseClassSecondAlias"]}"""
    }
  }

  pureTest("Codec derive caseClass schema should support annotation for props") {
    expectSchemaIs[CaseClassAvroProps] {
      """{"type":"record","name":"CaseClassAvroProps","namespace":"vulcan.generic.examples","fields":[{"name":"value","type":["null","string"],"prop1":"A","prop2":"B"}]}"""
    }
  }

  pureTest("Codec derive caseClass schema should capture errors on invalid names") {
    expectSchemaError[CaseClassFieldInvalidName] {
      """Invalid field "-value" in record vulcan.generic.examples.CaseClassFieldInvalidName: must start with a letter or underscore"""
    }
  }

  pureTest(
    "Codec derive caseClass schema should support annotation for setting explicit default null values for all nullable fields"
  ) {
    expectSchemaIs[CaseClassAvroNullDefault] {
      """{"type":"record","name":"CaseClassAvroNullDefault","namespace":"vulcan.generic.examples","fields":[{"name":"int","type":["null","int"],"default":null},{"name":"long","type":["null","long"],"default":null},{"name":"string","type":["null","string"],"default":null},{"name":"date","type":["null",{"type":"int","logicalType":"date"}],"default":null},{"name":"map","type":["null",{"type":"map","values":"string"}],"default":null},{"name":"caseClassValueClass","type":["null","int"],"default":null},{"name":"sealedTraitEnumDerived","type":["null",{"type":"enum","name":"SealedTraitEnumDerived","namespace":"com.example","symbols":["first","second"]}],"default":null}]}"""
    }
  }

  pureTest(
    "Codec derive caseClass schema should support annotation for setting explicit default null value for specific field"
  ) {
    expectSchemaIs[CaseClassFieldAvroNullDefault] {
      """{"type":"record","name":"CaseClassFieldAvroNullDefault","namespace":"vulcan.generic.examples","fields":[{"name":"int","type":["null","int"],"default":null},{"name":"long","type":["null","long"]},{"name":"string","type":["null","string"],"default":null},{"name":"date","type":["null",{"type":"int","logicalType":"date"}]},{"name":"map","type":["null",{"type":"map","values":"string"}],"default":null},{"name":"caseClassValueClass","type":["null","int"]},{"name":"sealedTraitEnumDerived","type":["null",{"type":"enum","name":"SealedTraitEnumDerived","namespace":"com.example","symbols":["first","second"]}],"default":null}]}"""
    }
  }

  pureTest(
    "Codec derive caseClass schema should support parameter annotation overriding case class annotation when setting explicit default null value for specific field"
  ) {
    expectSchemaIs[CaseClassAndFieldAvroNullDefault] {
      """{"type":"record","name":"CaseClassAndFieldAvroNullDefault","namespace":"vulcan.generic.examples","fields":[{"name":"int","type":["null","int"]},{"name":"long","type":["null","long"],"default":null},{"name":"string","type":["null","string"]},{"name":"date","type":["null",{"type":"int","logicalType":"date"}],"default":null},{"name":"map","type":["null",{"type":"map","values":"string"}]},{"name":"caseClassValueClass","type":["null","int"],"default":null},{"name":"sealedTraitEnumDerived","type":["null",{"type":"enum","name":"SealedTraitEnumDerived","namespace":"com.example","symbols":["first","second"]}]}]}"""
    }
  }

  pureTest("Codec derive caseClass schema should support optional field of a sum type") {
    expectSchemaIs[CaseClassOptionOfSumType] {
      """{"type":"record","name":"CaseClassOptionOfSumType","namespace":"vulcan.generic.examples","fields":[{"name":"field1","type":["null",{"type":"record","name":"FirstInSealedTraitCaseClassCustom","fields":[{"name":"value","type":"int"}]},"string"]}]}""".stripMargin
    }
  }

  pureTest("Codec derive caseClass encode should encode value class using encoder for underlying type") {
    expectEncodeIs[CaseClassValueClass](
      CaseClassValueClass(0),
      Right(AvroValue.Int(0))
    )
  }

  pureTest("Codec derive caseClass encode should encode as record") {
    expectEncodeIs[CaseClassField](
      CaseClassField(0),
      Right(AvroValue.Record(Vector(AvroValue.Int(0))))
    )
  }

  pureTest("Codec derive caseClass encode should encode as record with multiple fields") {
    expectEncodeIs[CaseClassTwoFields](
      CaseClassTwoFields("the-name", 0),
      Right(AvroValue.Record(Vector(AvroValue.String("the-name"), AvroValue.Int(0))))
    )
  }

  pureTest("Codec derive caseClass decode should decode value class using decoder for underlying type") {
    expectDecodeIs[CaseClassValueClass](
      Codec[Int].encode(123),
      Right(CaseClassValueClass(123))
    )
  }

  pureTest("Codec derive caseClass decode should error if schema is not record") {
    expectDecodeError[CaseClassField](
      Codec[CaseClassField].encode(CaseClassField(123)),
      Codec[String].schema,
      "Error decoding vulcan.generic.examples.CaseClassField: Got unexpected schema type string, expected schema type record"
    )
  }

  pureTest("Codec derive caseClass decode should error if value is not record") {
    expectDecodeError[CaseClassField](
      Codec[Int].encode(123),
      Codec[CaseClassField].schema,
      "Error decoding vulcan.generic.examples.CaseClassField: Invalid int value for record vulcan.generic.examples.CaseClassField"
    )
  }

  pureTest("Codec derive caseClass decode should error if any field is missing in writer schema") {
    expectDecodeError[CaseClassField](
      Right(AvroValue.Record(Vector(AvroValue.Int(123)))),
      Right(
        Schema.Record(
          Schema.Name("CaseClassField", Some("vulcan.generic.examples")),
          Vector(Schema.Field("other", Schema.Int))
        )
      ),
      "Error decoding vulcan.generic.examples.CaseClassField: Record writer schema is missing field 'value'"
    )
  }

  pureTest("Codec derive caseClass decode should decode as case class") {
    expectDecodeIs[CaseClassField](
      Codec[CaseClassField].encode(CaseClassField(123)),
      Right(CaseClassField(123))
    )
  }

  pureTest("Codec derive sealedTrait schema should encode case classes as records") {
    expectSchemaIs[SealedTraitCaseClass] {
      """[{"type":"record","name":"CaseClassInSealedTrait","namespace":"vulcan.generic.examples","fields":[{"name":"value","type":"int"}]}]"""
    }
  }

  pureTest("Codec derive sealedTrait schema should encode case objects as empty records") {
    expectSchemaIs[SealedTraitCaseObject] {
      """[{"type":"record","name":"CaseObjectInSealedTrait","namespace":"vulcan.generic.examples","fields":[]}]"""
    }
  }

  // Preserving existing behaviour from Magnolia for scala 2
  pureTest("Codec derive sealedTrait schema should order alternatives alphabetically by class name") {
    expectSchemaIs[SealedTraitCaseClassAvroNamespace](
      """[{"type":"record","name":"FirstInSealedTraitCaseClassAvroNamespace","namespace":"vulcan.generic.examples","fields":[{"name":"value","type":"int"}]},{"type":"record","name":"SecondInSealedTraitCaseClassAvroNamespace","namespace":"com.example","fields":[{"name":"value","type":"string"}]}]"""
    )
  }

  pureTest("Codec derive sealedTrait schema should capture errors on nested unions") {
    expectSchemaError[SealedTraitNestedUnion] {
      """Unions cannot directly contain other unions: [["null","int"]]"""
    }
  }

  pureTest("Codec derive sealedTrait encode should error if value is not an alternative") {
    expectEncodeError[SealedTraitCaseClassIncomplete](
      SecondInSealedTraitCaseClassIncomplete(0),
      "Exhausted alternatives for union"
    )
  }

  pureTest("Codec derive sealedTrait encode should encode with encoder for subtype") {
    val value = CaseClassInSealedTrait(0)
    expectEncodeIs[SealedTraitCaseClass](
      value,
      Right(AvroValue.Union(0, AvroValue.Record(Vector(AvroValue.Int(0)))))
    )
  }

  pureTest("Codec derive sealedTrait decode should error if schema is not in union") {
    expectDecodeError[SealedTraitCaseClass](
      Codec[SealedTraitCaseClass].encode(CaseClassInSealedTrait(0)),
      Codec[String].schema,
      "Error decoding vulcan.generic.examples.SealedTraitCaseClass: Missing alternative for string in union"
    )
  }

  pureTest("Codec derive sealedTrait decode should decode if schema is part of union") {
    expectDecodeIs[SealedTraitCaseClass](
      Codec[CaseClassInSealedTrait].encode(CaseClassInSealedTrait(0)),
      Right(CaseClassInSealedTrait(0)),
      Codec[CaseClassInSealedTrait].schema
    )
  }

  pureTest("Codec derive sealedTrait decode should error if value is not a union value") {
    expectDecodeError[SealedTraitCaseClass](
      Codec[Int].encode(123),
      Codec[SealedTraitCaseClass].schema,
      "Invalid int value for union"
    )
  }

  pureTest("Codec derive sealedTrait decode should error if no schema in union with container name") {
    expectDecodeError[SealedTraitCaseObject](
      Codec[SealedTraitCaseObject].encode(CaseObjectInSealedTrait),
      Codec[SealedTraitCaseClass].schema,
      "Error decoding vulcan.generic.examples.SealedTraitCaseObject: Missing alternative for record vulcan.generic.examples.CaseClassInSealedTrait in union"
    )
  }

  pureTest("Codec derive sealedTrait decode should error if no subtype with container name") {
    expectDecodeError[SealedTraitCaseClass](
      Codec[SealedTraitCaseObject].encode(CaseObjectInSealedTrait),
      Codec[SealedTraitCaseObject].schema,
      "Error decoding vulcan.generic.examples.SealedTraitCaseClass: Missing alternative for record vulcan.generic.examples.CaseObjectInSealedTrait in union"
    )
  }

  pureTest("Codec derive sealedTrait decode should decode using schema and decoder for subtype") {
    expectDecodeIs[SealedTraitCaseClass](
      Codec[SealedTraitCaseClass].encode(CaseClassInSealedTrait(0)),
      Right(CaseClassInSealedTrait(0))
    )
  }
}
