package io.plasmap.serializer

import io.plasmap.model._

object OsmDenormalizedSerializer {

  import scala.pickling._
  import scala.pickling.Defaults._
  import scala.pickling.binary._
  import scala.pickling.shareNothing._

  def fromBinary(encoded: Array[Byte]): OsmDenormalizedObject = encoded.unpickle[OsmDenormalizedObject]

  def toBinary(decoded: OsmDenormalizedObject): Array[Byte] = decoded.pickle.value

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