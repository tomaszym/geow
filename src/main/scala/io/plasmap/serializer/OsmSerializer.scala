package io.plasmap.serializer

import io.plasmap.model._
import io.plasmap.model.geometry._

import scala.util.Try
import scalaz.\/

object OsmSerializer {

  import scala.pickling._
  import binary._

  def fromBinary(encoded: Array[Byte]):Option[OsmObject] = Try(encoded.unpickle[OsmObject]).toOption

  def toBinary(decoded: OsmObject): Array[Byte] = decoded.pickle.value

}