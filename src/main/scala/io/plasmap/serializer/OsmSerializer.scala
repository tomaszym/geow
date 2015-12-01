package io.plasmap.serializer

/**
  *
  * Created by mark on 01.12.15.
  */

import io.plasmap.model.{OsmObject, OsmDenormalizedObject}
import scodec.bits.BitVector

import scala.util.Try

object OsmSerializer {

  import Codecs._

  def fromBinary(bytes:Array[Byte]): Try[OsmObject] = {
    Try{
      val bv:BitVector = BitVector(bytes)
      osmObjectCodec
        .decode(bv)
        .require
        .value
    }
  }

  def toBinary(obj:OsmObject):Array[Byte] = {
    osmObjectCodec
      .encode(obj)
      .require
      .toByteArray
  }

}
