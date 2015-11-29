package io.plasmap.serializer

import io.plasmap.model._
import com.twitter.chill.KryoInjection

import scala.util.Try

object OsmDenormalizedSerializer {

  /*import scala.pickling._
  import scala.pickling.Defaults._
  import scala.pickling.binary._*/

  //def fromBinary(encoded: Array[Byte]): OsmDenormalizedObject = encoded.unpickle[OsmDenormalizedObject]
  def fromBinary(bytes: Array[Byte]): Try[OsmDenormalizedObject] = KryoInjection.invert(bytes).map(_.asInstanceOf[OsmDenormalizedObject])

  //def toBinary(decoded: OsmDenormalizedObject): Array[Byte] = decoded.pickle.value
  def toBinary(item: OsmDenormalizedObject): Array[Byte] = KryoInjection(item)

  def toGeoJsonString(osmDenormalizedObjects:List[OsmDenormalizedObject]):String = {
    GeoJsonSerialiser.jsonFromFeatureCollection(
      OsmDenormalisedGeoJSONBijections.denormalizedToGeoJson(osmDenormalizedObjects)
    )
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