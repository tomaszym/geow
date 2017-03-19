package io.plasmap.serializer

/**
  *
  * Created by mark on 01.12.15.
  */

import io.plasmap.model.OsmDenormalizedObject
import scodec._
import scodec.bits.BitVector

import scala.util.Try

object OsmDenormalizedSerializer {

  import Codecs._

  def fromBinary(bytes:Array[Byte]): Try[OsmDenormalizedObject] = {
    Try{
      val bv:BitVector = BitVector(bytes)
      osmDenormalizedObjectCodec
        .decode(bv)
        .require
        .value
    }
  }

  def toBinary(obj:OsmDenormalizedObject):Array[Byte] = {
    osmDenormalizedObjectCodec
      .encode(obj)
      .require
      .toByteArray
  }

  def toGeoJsonString(osmDenormalizedObject:OsmDenormalizedObject):String =
    GeoJsonSerialiser.jsonFromFeature(
      OsmDenormalisedGeoJSONBijections.denormalizedToGeoJson(osmDenormalizedObject)
    )

  def fromGeoJsonString(geojson: String):List[OsmDenormalizedObject] =
    GeoJsonSerialiser
      .featureCollectionFromJSON(geojson)
      .map(OsmDenormalisedGeoJSONBijections.geoJsonToDenormalized)
      .getOrElse(Nil)
}
