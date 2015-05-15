package org.geow.serializer

import org.geow.model._

object OsmDenormalizedSerializer {

  import scala.pickling._
  import binary._

  def fromBinary(encoded: Array[Byte]): OsmDenormalizedObject = encoded.unpickle[OsmDenormalizedObject]

  def toBinary(decoded: OsmDenormalizedObject): Array[Byte] = decoded.pickle.value

  def toGeoJsonString(osmDenormalizedObjects:List[OsmDenormalizedObject]):String = {
    val inner = (for (osm ← osmDenormalizedObjects) yield toGeoJsonString(osm)).mkString(",")
    s"""
       |{
       |  "type": "FeatureCollection",
       |  "features": [$inner]
       |}
     """.stripMargin
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