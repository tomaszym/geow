package io.plasmap.parser.impl

import io.plasmap.model.OsmDenormalizedObject
import io.plasmap.model.geometry._
import io.plasmap.parser.OsmDenormalizedParser
import io.plasmap.serializer.GeoJsonSerialiser
import io.plasmap.serializer.OsmDenormalisedGeoJSONBijections._

import scala.io.{Codec, Source}

/**
 * Created by janschulte on 06/05/15.
 */
case class OsmGeoJSONParser(source:Source) extends OsmDenormalizedParser {

  val json = source.mkString

  private val iterator = GeoJsonSerialiser.featureCollectionFromJSON(json)
    .getOrElse(FeatureCollection(Nil))
    .features
    .iterator

  override def hasNext: Boolean = iterator.hasNext
  override def next(): Option[OsmDenormalizedObject] = geoJsonToDenormalized(iterator.next())

}

object OsmGeoJSONParser{

  def apply(fileName:String)(implicit codec:Codec) = new OsmGeoJSONParser(Source.fromFile(fileName)(codec))
}