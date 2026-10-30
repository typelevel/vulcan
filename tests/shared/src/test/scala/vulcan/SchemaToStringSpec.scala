/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import cats.syntax.all.*
import io.circe.Json
import io.circe.JsonObject

object SchemaToStringSpec extends BaseSuite {
  private def record(
    name: String,
    namespace: Option[String],
    fields: Schema.Field*
  ): Schema.Record =
    Schema.Record(Schema.Name(name, namespace), fields.toVector)

  pureTest("Schema toJson should return the schema as JSON") {
    expect.eql(
      Json.arr(Json.fromString("null"), Json.fromString("int")),
      Schema.Union(Vector(Schema.Null, Schema.Int)).toJson
    )
  }

  pureTest("Schema toString should print primitives without props as type names") {
    expect.eql("\"null\"", Schema.Null.toString) &&
    expect.eql("\"boolean\"", Schema.Boolean.toString) &&
    expect.eql("\"int\"", Schema.Int.toString) &&
    expect.eql("\"long\"", Schema.Long.toString) &&
    expect.eql("\"float\"", Schema.Float.toString) &&
    expect.eql("\"double\"", Schema.Double.toString) &&
    expect.eql("\"bytes\"", Schema.Bytes.toString) &&
    expect.eql("\"string\"", Schema.String.toString)
  }

  pureTest("Schema toString should print primitives with props as objects") {
    val props = JsonObject("avro.java.string" -> Json.fromString("String"))
    expect.eql(
      """{"type":"string","avro.java.string":"String"}""",
      Schema.String.withProps(props).toString
    )
  }

  pureTest("Schema toString should print logical types followed by their parameters") {
    expect.eql(
      """{"type":"long","logicalType":"timestamp-millis"}""",
      Schema.Long.withLogicalType(LogicalType.timestampMillis).toString
    ) &&
    expect.eql(
      """{"type":"bytes","logicalType":"decimal","precision":10,"scale":2}""",
      Schema.Bytes.withLogicalType(LogicalType.decimal(10, 2)).toString
    )
  }

  pureTest("Schema toString should print arrays, maps, and unions") {
    expect.eql("""{"type":"array","items":"int"}""", Schema.Array(Schema.Int).toString) &&
    expect.eql(
      """{"type":"map","values":"string","custom":true}""",
      Schema.Map(Schema.String).withProps(JsonObject("custom" -> Json.True)).toString
    ) &&
    expect.eql("""["null","int"]""", Schema.Union(Vector(Schema.Null, Schema.Int)).toString)
  }

  pureTest("Schema toString should print records in the same order as the Java Avro library") {
    val schema =
      record(
        "Person",
        Some("com.example"),
        Schema
          .Field("name", Schema.String)
          .withDoc(Some("The full name"))
          .withAliases(Vector("fullName")),
        Schema
          .Field("age", Schema.Union(Vector(Schema.Null, Schema.Int)))
          .withDefault(Some(Json.Null))
          .withOrder(Schema.Field.Order.Descending)
          .withProps(JsonObject("custom" -> Json.True))
      )
        .withDoc(Some("A person"))
        .withAliases(Vector("Human"))
        .withProps(JsonObject("custom" -> Json.fromInt(1)))

    expect.eql(
      """{"type":"record","name":"Person","namespace":"com.example","doc":"A person","fields":[{"name":"name","type":"string","doc":"The full name","aliases":["fullName"]},{"name":"age","type":["null","int"],"default":null,"order":"descending","custom":true}],"custom":1,"aliases":["Human"]}""",
      schema.toString
    )
  }

  pureTest("Schema toString should print enums in the same order as the Java Avro library") {
    val schema =
      Schema
        .Enum(Schema.Name("Suit", Some("com.example")), Vector("hearts", "spades"))
        .withDefault(Some("spades"))
        .withDoc(Some("A suit"))
        .withAliases(Vector("Color"))
        .withProps(JsonObject("custom" -> Json.True))

    expect.eql(
      """{"type":"enum","name":"Suit","namespace":"com.example","doc":"A suit","symbols":["hearts","spades"],"default":"spades","custom":true,"aliases":["Color"]}""",
      schema.toString
    )
  }

  pureTest("Schema toString should print fixed in the same order as the Java Avro library") {
    val schema =
      Schema
        .Fixed(Schema.Name("Id", Some("com.example")), 16)
        .withLogicalType(LogicalType.uuid)
        .withDoc(Some("An id"))
        .withAliases(Vector("Identifier"))

    expect.eql(
      """{"type":"fixed","name":"Id","namespace":"com.example","doc":"An id","size":16,"logicalType":"uuid","aliases":["Identifier"]}""",
      schema.toString
    )
  }

  pureTest("Schema toString should only include namespaces which differ from the enclosing namespace") {
    val sameNamespace =
      record(
        "Outer",
        Some("com.example"),
        Schema.Field("inner", record("Inner", Some("com.example")))
      )

    val otherNamespace =
      record(
        "Outer",
        Some("com.example"),
        Schema.Field("inner", record("Inner", Some("org.other")))
      )

    expect.eql(
      """{"type":"record","name":"Outer","namespace":"com.example","fields":[{"name":"inner","type":{"type":"record","name":"Inner","fields":[]}}]}""",
      sameNamespace.toString
    ) &&
    expect.eql(
      """{"type":"record","name":"Outer","namespace":"com.example","fields":[{"name":"inner","type":{"type":"record","name":"Inner","namespace":"org.other","fields":[]}}]}""",
      otherNamespace.toString
    )
  }

  pureTest("Schema toString should use an empty namespace for the null namespace inside another namespace") {
    val schema =
      record("Outer", Some("com.example"), Schema.Field("inner", record("Inner", None)))

    expect.eql(
      """{"type":"record","name":"Outer","namespace":"com.example","fields":[{"name":"inner","type":{"type":"record","name":"Inner","namespace":"","fields":[]}}]}""",
      schema.toString
    )
  }

  pureTest("Schema toString should reference named schemas by name after their definition") {
    val fixed = Schema.Fixed(Schema.Name("Hash", Some("com.example")), 4)
    val schema =
      record(
        "Hashes",
        Some("com.example"),
        Schema.Field("first", fixed),
        Schema.Field("second", fixed)
      )

    expect.eql(
      """{"type":"record","name":"Hashes","namespace":"com.example","fields":[{"name":"first","type":{"type":"fixed","name":"Hash","size":4}},{"name":"second","type":"Hash"}]}""",
      schema.toString
    )
  }

  pureTest("Schema toString should reference named schemas by full name in other namespaces") {
    val fixed = Schema.Fixed(Schema.Name("Hash", Some("org.other")), 4)
    val schema =
      record(
        "Hashes",
        Some("com.example"),
        Schema.Field("first", fixed),
        Schema.Field("second", fixed)
      )

    expect.eql(
      """{"type":"record","name":"Hashes","namespace":"com.example","fields":[{"name":"first","type":{"type":"fixed","name":"Hash","namespace":"org.other","size":4}},{"name":"second","type":"org.other.Hash"}]}""",
      schema.toString
    )
  }

  pureTest("Schema toString should reference named schemas by full name when named after Avro types") {
    val fixed = Schema.Fixed(Schema.Name("int", Some("com.example")), 4)
    val schema =
      record(
        "Hashes",
        Some("com.example"),
        Schema.Field("first", fixed),
        Schema.Field("second", fixed)
      )

    expect.eql(
      """{"type":"record","name":"Hashes","namespace":"com.example","fields":[{"name":"first","type":{"type":"fixed","name":"int","size":4}},{"name":"second","type":"com.example.int"}]}""",
      schema.toString
    )
  }

  pureTest("Schema toString should print references by name") {
    val name = Schema.Name("Node", Some("com.example"))
    val schema =
      Schema.Record(
        name,
        Vector(
          Schema.Field("value", Schema.Int),
          Schema.Field("next", Schema.Union(Vector(Schema.Null, Schema.Ref(name))))
        )
      )

    expect.eql(
      """{"type":"record","name":"Node","namespace":"com.example","fields":[{"name":"value","type":"int"},{"name":"next","type":["null","Node"]}]}""",
      schema.toString
    )
  }

  pureTest("Schema toString should qualify aliases relative to the namespace and remove duplicates") {
    val schema =
      record("Person", Some("com.example")).withAliases(
        Vector("Human", "com.example.Human", "org.other.Human", "com.example.Being")
      )

    expect.eql(
      """{"type":"record","name":"Person","namespace":"com.example","fields":[],"aliases":["Human","org.other.Human","Being"]}""",
      schema.toString
    )
  }

  pureTest("Schema toString should not print reserved properties") {
    val props = JsonObject("type" -> Json.fromString("long"), "custom" -> Json.True)

    val field =
      Schema.Field("value", Schema.Int).withProps(JsonObject("default" -> Json.fromInt(1)))

    expect.eql("""{"type":"int","custom":true}""", Schema.Int.withProps(props).toString) &&
    expect.eql(
      """{"type":"record","name":"Test","fields":[{"name":"value","type":"int"}]}""",
      record("Test", None, field).toString
    )
  }

  pureTest("Schema toString should escape strings in the same way as the Java Avro library") {
    val backslash = "\\"
    val quote = "\""
    val unescaped = List(0x7f.toChar, 0x80.toChar, '/').mkString

    val doc =
      List(quote, backslash, "\b", "\t", "\n", "\f", "\r").mkString +
        List(0x01.toChar, 0x1f.toChar).mkString + unescaped

    val escaped =
      List(
        backslash + quote,
        backslash + backslash,
        backslash + "b",
        backslash + "t",
        backslash + "n",
        backslash + "f",
        backslash + "r",
        backslash + "u0001",
        backslash + "u001F"
      ).mkString + unescaped

    val schema = Schema.Enum(Schema.Name("Escaped"), Vector("A")).withDoc(Some(doc))

    expect.eql(s"""{"type":"enum","name":"Escaped","doc":"$escaped","symbols":["A"]}""", schema.toString)
  }

  pureTest("Schema show should have a Show instance consistent with toString") {
    val schema: Schema = Schema.Union(Vector(Schema.Null, Schema.Int))
    expect.eql(schema.toString, schema.show)
  }
}
