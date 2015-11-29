package io.plasmap.serializer

import io.plasmap.model._
import io.plasmap.model.geometry._

import scala.util.Try
import scalaz.\/

object OsmSerializer {

  import scala.pickling._
  import scala.pickling.shareNothing._
  import binary._

  def fromBinary(encoded: Array[Byte]):Try[OsmObject] = Try(encoded.unpickle[OsmObject])

  def toBinary(decoded: OsmObject): Array[Byte] = decoded.pickle.value

}