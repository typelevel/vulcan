/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.generic

import io.circe.JsonObject
import scala.annotation.StaticAnnotation

/**
  * Annotation which can be used to include props for record fields in derived schemas.
  *
  * Annotate a `case class` parameter for record field props when using `Codec.derive`
  * from the generic module.
  */
final class AvroProps(final val props: JsonObject) extends StaticAnnotation {
  override final def toString: String =
    s"AvroProps($props)"
}

private[vulcan] object AvroProps {
  final def unapply(avroProps: AvroProps): Some[JsonObject] =
    Some(avroProps.props)
}
