package io.plasmap.serializer

import io.plasmap.model._
import io.plasmap.model.geometry._

object OsmSerializer {

  import scala.pickling._
  import binary._

  def fromBinary(encoded: Array[Byte]): OsmObject = encoded.unpickle[OsmObject]

  def toBinary(decoded: OsmObject): Array[Byte] = decoded.pickle.value

}