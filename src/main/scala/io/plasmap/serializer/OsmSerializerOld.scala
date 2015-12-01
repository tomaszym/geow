package io.plasmap.serializer

import com.twitter.chill.KryoInjection
import io.plasmap.model._

import scala.util.Try

object OsmSerializerOld {

  /*import scala.pickling._
  import scala.pickling.Defaults._
  import scala.pickling.binary._*/

  //def fromBinary(encoded: Array[Byte]):Try[OsmObject] = Try(encoded.unpickle[OsmObject])
  def fromBinary(bytes: Array[Byte]):Try[OsmObject] =  KryoInjection.invert(bytes).map(_.asInstanceOf[OsmObject])

  //def toBinary(decoded: OsmObject): Array[Byte] = decoded.pickle.value
  def toBinary(item: OsmObject): Array[Byte] = KryoInjection(item)

}