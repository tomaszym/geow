package io.plasmap.parser.impl

import java.io.InputStream

import io.plasmap.model.OsmDenormalizedObject
import io.plasmap.parser.OsmDenormalizedParser
import io.plasmap.serializer.GeoJsonSerialiser
import io.plasmap.model._
import io.plasmap.model.geometry._
import io.plasmap.parser.OsmDenormalizedParser
import io.plasmap.serializer.GeoJsonSerialiser
import io.plasmap.serializer.OsmDenormalisedGeoJSONBijections._
import argonaut._, Argonaut._
import scala.io.Source
import scala.reflect.io.File

/**
 * Created by janschulte on 06/05/15.
 */
case class OsmGeoJSONParser(is:InputStream) extends OsmDenormalizedParser {
  import GeoJsonSerialiser._

  val json = Source.fromInputStream(is).mkString

  private val iterator = GeoJsonSerialiser.featureCollectionFromJSON(json)
    .getOrElse(FeatureCollection(Nil))
    .features
    .iterator

  override def hasNext: Boolean = iterator.hasNext
  override def next(): Option[OsmDenormalizedObject] = geoJsonToDenormalized(iterator.next())

}

object OsmGeoJSONParser {
  def apply(source: String): OsmGeoJSONParser = OsmGeoJSONParser(File(source).inputStream())
}