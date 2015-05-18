package org.geow.parser.impl

import java.io.InputStream

import org.geow.model._
import org.geow.model.geometry._
import org.geow.parser.OsmDenormalizedParser
import org.geow.serializer.GeoJsonSerialiser
import org.geow.serializer.OsmDenormalisedGeoJSONBijections._
import argonaut._, Argonaut._
import scala.io.Source
import scala.reflect.io.File

/**
 * Created by janschulte on 06/05/15.
 */
case class OsmGeoJSONParser(is:InputStream) extends OsmDenormalizedParser {
  import org.geow.serializer.GeoJsonSerialiser._

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