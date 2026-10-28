/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.generic

import io.circe.JsonObject
import scala.annotation.StaticAnnotation

/**
  * Annotation which can be used to alter props in derived schemas.
  *
  * The annotation can be used in the following situations.<br>
  *   - Annotate a type for enum props when using [[deriveEnum]].<br>
  *   - Annotate a type for fixed props when using [[deriveFixed]].<br>
  *   - Annotate a `case class` for record props when using `Codec.derive` from the generic
  *     module.<br>
  *   - Annotate a `case class` parameter for record field props when using `Codec.derive` from the
  *     generic module.
  */
final class AvroProps(final val props: JsonObject) extends StaticAnnotation {
  override final def toString: String =
    s"AvroProps($props)"
}

private[vulcan] object AvroProps {
  final def unapply(avroProps: AvroProps): Some[JsonObject] =
    Some(avroProps.props)
}
