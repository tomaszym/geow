package org.geow.serializer

import org.geow.model._
import org.geow.model.geometry._

import scala.util.{Random, Try}

/**
 * Created by mark on 13.05.15.
 */
object OsmDenormalisedGeoJSONBijections {
  type TagMap = Map[String, String]
  object OsmPropertyNames {
    val sep = "_:_"
    val osmId = "_osm_id"
    val osmUserId = "_osm_user_id"
    val osmUserName = "_osm_user_name"
    val osmVersionChangeset = "_osm_version_changeset"
    val osmVersionNumber = "_osm_version_changeset"
    val osmVersionTimestamp = "_osm_version_timestamp"
    val osmVersionVisible = "_osm_version_visible"
  }
  
  private def osmProperties(osmObj:OsmDenormalizedObject):TagMap = {
    import OsmPropertyNames._
    Map(
      osmId → osmObj.id.value.toString,
      osmUserId → osmObj.user.map(_.uid.toString).getOrElse("None"),
      osmUserName → osmObj.user.map(_.username).getOrElse("None"),
      osmVersionChangeset → osmObj.version.changeset.toString,
      osmVersionNumber → osmObj.version.versionNumber.toString,
      osmVersionTimestamp → osmObj.version.timestamp.toString,
      osmVersionVisible → osmObj.version.visible.toString
    )
  }

  private case class PartialOsmObject(id:OsmId, user:Option[OsmUser], version:OsmVersion, tags:List[OsmTag]) {
    def toOsmDenormalisedNode(point:Point):OsmDenormalizedNode =
      OsmDenormalizedNode(id, user, version, tags, point)
    def toOsmDenormalisedWay(lineString:LineString):OsmDenormalizedWay =
      OsmDenormalizedWay(id, user, version, tags, lineString)
    def toOsmDenormalisedRelation(geometryCollection:GeometryCollection):OsmDenormalizedRelation =
      OsmDenormalizedRelation(id, user, version, tags, geometryCollection)
  }

  private def partialOsmObjectFromTags(tags:TagMap):PartialOsmObject = { //TODO: Throw this out, should not be necessary anymore
    val rand = new Random()
    import OsmPropertyNames._
    val id:OsmId = OsmId(tags.get(osmId).map(_.toLong).getOrElse(rand.nextInt(Int.MaxValue)))
    val user:Option[OsmUser] =
      for {
        idS ← tags.get(osmUserId)
        id ← Try(idS.toLong).toOption
        name ← tags.get(osmUserName)
      } yield OsmUser(name, id)

    val version:OsmVersion = {
      val versionOpt:Option[OsmVersion] = for {
        timestamp ← tags.get(osmVersionTimestamp).flatMap(t ⇒ Try(t.toLong).toOption)
        changeset ← tags.get(osmVersionChangeset).flatMap(c ⇒ Try(c.toInt).toOption)
        number ← tags.get(osmVersionNumber).flatMap(n ⇒ Try(n.toInt).toOption)
        versionVisible ← tags.get(osmVersionVisible).flatMap(v ⇒ Try(v.toBoolean).toOption)
      } yield OsmVersion(timestamp, number, changeset, versionVisible)
     versionOpt.getOrElse( OsmVersion() )
    }

    val oTags:List[OsmTag] = (tags - osmId - osmUserName - osmUserId - osmVersionTimestamp - osmVersionChangeset - osmVersionNumber).toList.map{
      case (k, v) ⇒ OsmTag(k, v)
    }
    PartialOsmObject(id, user, version, oTags)
  }

  private def propertiesFromOsmObject(osmObj:OsmDenormalizedObject) = {
    Map(osmObj.tags.map(osmTag ⇒ osmTag.key → osmTag.value):_*) ++ osmProperties(osmObj)
  }

  private def makeGeoJSONFeature(osmObj:OsmDenormalizedObject) = {
    val properties:TagMap = propertiesFromOsmObject(osmObj)
    Feature(osmObj.geometry, Map(properties.toList:_*))
  }

  private def osmDenormalisedNodeToGeoJSONFeature(osmNode:OsmDenormalizedNode):Feature =
    makeGeoJSONFeature(osmNode)

  private def osmDenormalisedWayToGeoJSONFeature(osmWay:OsmDenormalizedWay):Feature =
    makeGeoJSONFeature(osmWay)

  private def osmDenormalisedRelationToGeoJSONFeature(osmRelation:OsmDenormalizedRelation):Feature =
    makeGeoJSONFeature(osmRelation)

  private def geoJSONFeatureToOsmDenormalisedNode(feature:Feature):Option[OsmDenormalizedNode] = {
    val partial = partialOsmObjectFromTags(feature.properties)
    feature.geometry match {
      case p:Point ⇒ Some(partial.toOsmDenormalisedNode(p))
      case _ ⇒ None
    }
  }

  private def geoJSONFeatureToOsmDenormalisedWay(feature:Feature):Option[OsmDenormalizedWay] = {
    val partial = partialOsmObjectFromTags(feature.properties)
    feature.geometry match {
      case ls:LineString ⇒ Some(partial.toOsmDenormalisedWay(ls))
      case _ ⇒ None
    }
  }

  private def geoJSONFeatureToOsmDenormalisedRelation(feature:Feature):Option[OsmDenormalizedRelation] = {
    val partial = partialOsmObjectFromTags(feature.properties)
    feature.geometry match {
      case gc:GeometryCollection ⇒ Some(partial.toOsmDenormalisedRelation(gc))
      case _ ⇒ None
    }
  }

  def geoJsonToDenormalized(feature:Feature):Option[OsmDenormalizedObject] =
    feature.geometry match {
      case _:Point ⇒ geoJSONFeatureToOsmDenormalisedNode(feature)
      case _:LineString ⇒ geoJSONFeatureToOsmDenormalisedWay(feature)
      case _:GeometryCollection ⇒ geoJSONFeatureToOsmDenormalisedRelation(feature)
      case _ ⇒ None
    }

  def geoJsonToDenormalized(featureCollection:FeatureCollection):List[OsmDenormalizedObject] =
    featureCollection.features.foldLeft(List.empty[OsmDenormalizedObject])(
      (objs, f) ⇒ geoJsonToDenormalized(f).map(_ :: objs).getOrElse(objs)
    )

  def denormalizedToGeoJson(osmDenObj:OsmDenormalizedObject):Feature = osmDenObj match {
      case n:OsmDenormalizedNode     ⇒ osmDenormalisedNodeToGeoJSONFeature(n)
      case w:OsmDenormalizedWay      ⇒ osmDenormalisedWayToGeoJSONFeature(w)
      case r:OsmDenormalizedRelation ⇒ osmDenormalisedRelationToGeoJSONFeature(r)
    }

  def denormalizedToGeoJson(osmDenObjs:List[OsmDenormalizedObject]):FeatureCollection =
    osmDenObjs.foldLeft(FeatureCollection(Nil))( (fc, osm) ⇒
      fc.copy(features = denormalizedToGeoJson(osm) :: fc.features) )
}
