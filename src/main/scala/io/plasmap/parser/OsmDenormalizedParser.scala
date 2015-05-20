package io.plasmap.parser

import java.io.FileInputStream

import io.plasmap.model.OsmDenormalizedObject
import io.plasmap.parser.impl._

import scala.io.Codec

trait OsmDenormalizedParser extends Iterator[Option[OsmDenormalizedObject]]

object OsmDenormalizedParser {

  implicit val codec = Codec.UTF8

  def apply(fileName: String)(implicit codec: Codec): OsmDenormalizedParser = fileName match {
    case json if fileName.endsWith(".geojson") =>
      val inputStream = new FileInputStream(json)
      OsmGeoJSONParser(inputStream)

    case _ =>  throw new Error("Unknown file type.")
  }

  //def apply(source: Source): OsmDenormalizedParser = new OsmXmlParser(source)

}