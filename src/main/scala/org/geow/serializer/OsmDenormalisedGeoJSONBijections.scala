package org.geow.serializer

import org.geow.geojson.{FeatureCollection, Geometry ⇒ GeoJSONGeometry, GeometryCollection ⇒ GeoJSONGeometryCollection, LineString ⇒ GeoJSONLineString, Point ⇒ GeoJSONPoint, Feature ⇒ GeoJSONFeature}
import org.geow.model._
import org.geow.model.geometry._

import scala.util.Try

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

  implicit def geowPointToGeoJSONPoint(p:Point):GeoJSONPoint =
    GeoJSONPoint((p.lon, p.lat))

  implicit def geoJSONPointToGeowPoint(gjp:GeoJSONPoint):Point =
    Point(gjp.coordinates._1, gjp.coordinates._2)

  implicit def geowLinestringToGeoJSONLineString(ls:Linestring):GeoJSONLineString =
    GeoJSONLineString(ls.points.map(p ⇒ (p.lon, p.lat)))

  implicit def geoJSONLineStringToGeowLinestring(gjls:GeoJSONLineString):Linestring =
    Linestring(gjls.coordinates.map(p ⇒ Point(p._1, p._2)))

  private def geowGeometryCollectionToGeoJSONGeometryCollection(gc:GeometryCollection):(TagMap, GeoJSONGeometryCollection) = {
    import OsmPropertyNames._
    val (_, geometries, tagmap) = gc.members.foldLeft((0, List.empty[GeoJSONGeometry], Map.empty[String, String])){
      case ((i, geojson, tags), wrapped) ⇒
        val (moreTags:Map[String, String], geo:GeoJSONGeometry) = wrapped.geometry match {
          case p: Point ⇒ (Map(), p)
          case ls: Linestring ⇒ (Map(), ls)
          case c: GeometryCollection ⇒ geowGeometryCollectionToGeoJSONGeometryCollection(c)
        }
        (i+1, geo :: geojson, moreTags ++ tags + (i.toString → s"${wrapped.ref}$sep${wrapped.role}$sep${wrapped.typ}"))
    }
    (tagmap, GeoJSONGeometryCollection(geometries))
  }

  private case class PartialGeometryMember(typ:OsmType, ref:OsmId, role:OsmRole) {
    def pointToGeometryMember(point:Point):GeometryMember = GeometryMember(typ, ref, role, point)
    def linestringToGeometryMember(linestring:Linestring):GeometryMember = GeometryMember(typ, ref, role, linestring)
    def geometryCollectionToGeometryMember(gc:GeometryCollection):GeometryMember = GeometryMember(typ, ref, role, gc)
  }

  private def unwrapTag(tag:String):PartialGeometryMember = {
    val Array(refS:String, roleS:String, typS:String) = tag.split(OsmPropertyNames.sep)
    val typ = typS match {
      case n if n == OsmTypeNode.toString     ⇒ OsmTypeNode
      case w if w == OsmTypeWay.toString      ⇒ OsmTypeWay
      case r if r == OsmTypeRelation.toString ⇒ OsmTypeRelation
    }
    val ref = OsmId(refS.toLong)
    val roleRegex = """OsmRoleOther\((.*)\)""".r
    val role = roleS match {
      case inner if inner == OsmRoleInner.toString     ⇒ OsmRoleInner
      case outer if outer == OsmRoleOuter.toString     ⇒ OsmRoleOuter
      case empty if empty == OsmRoleEmpty.toString     ⇒ OsmRoleEmpty
      case roleRegex(value)                            ⇒ OsmRoleOther(value)
    }
    PartialGeometryMember(typ, ref, role)
  }

  private def geoJSONGeometryCollectionToGeowGeometryCollection(tags:TagMap, gc:GeoJSONGeometryCollection):GeometryCollection = {
    gc.geometries.zipWithIndex.foldLeft(GeometryCollection(List.empty[GeometryMember])){
      case (c, (geo, i)) ⇒
        val partial = unwrapTag(tags("i"))
        val geoMember:GeometryMember = geo match {
          case p:GeoJSONPoint ⇒ partial.pointToGeometryMember(p)
          case ls:GeoJSONLineString ⇒ partial.linestringToGeometryMember(ls)
          case geocoll:GeoJSONGeometryCollection ⇒
            partial.geometryCollectionToGeometryMember(
              geoJSONGeometryCollectionToGeowGeometryCollection(tags, geocoll)
            )
        }
        c.copy(members = geoMember :: c.members)
    }
  }

  private case class PartialOsmObject(id:OsmId, user:Option[OsmUser], version:OsmVersion, tags:List[OsmTag]) {
    def toOsmDenormalisedNode(point:Point):OsmDenormalizedNode =
      OsmDenormalizedNode(id, user, version, tags, point)
    def toOsmDenormalisedWay(lineString:Linestring):OsmDenormalizedWay =
      OsmDenormalizedWay(id, user, version, tags, lineString)
    def toOsmDenormalisedRelation(geometryCollection:GeometryCollection):OsmDenormalizedRelation =
      OsmDenormalizedRelation(id, user, version, tags, geometryCollection)
  }

  private def partialOsmObjectFromTags(tags:TagMap):PartialOsmObject = {
    import OsmPropertyNames._
    val id:OsmId = OsmId(tags.get(osmId).get.toLong)
    val user:Option[OsmUser] =
      for {
        idS ← tags.get(osmUserId)
        id ← Try(idS.toLong).toOption
        name ← tags.get(osmUserName)
      } yield OsmUser(name, id)

    val version:OsmVersion = {
      val timestamp = tags.get(osmVersionTimestamp).map(_.toLong).get
      val changeset = tags.get(osmVersionChangeset).map(_.toInt).get
      val number = tags.get(osmVersionNumber).map(_.toInt).get
      val versionVisible = tags.get(osmVersionVisible).map(_.toBoolean).get
      OsmVersion(timestamp, number, changeset, versionVisible)
    }
    val oTags:List[OsmTag] = (tags - osmId - osmUserName -osmUserId - osmVersionTimestamp - osmVersionChangeset - osmVersionNumber).toList.map{
      case (k, v) ⇒ OsmTag(k, v)
    }
    PartialOsmObject(id, user, version, oTags)
  }

  private def propertiesFromOsmObject(osmObj:OsmDenormalizedObject) = {
    Map(osmObj.tags.map(osmTag ⇒ osmTag.key → osmTag.value):_*) ++ osmProperties(osmObj)
  }

  private def makeGeoJSONFeature(osmObj:OsmDenormalizedObject, geometry:GeoJSONGeometry) = {
    val properties:TagMap = propertiesFromOsmObject(osmObj)
    GeoJSONFeature(geometry, Map(properties.toList:_*))
  }

  private def osmDenormalisedNodeToGeoJSONFeature(osmNode:OsmDenormalizedNode):GeoJSONFeature = {
    val geometry:GeoJSONPoint = geowPointToGeoJSONPoint(osmNode.geometry)
    makeGeoJSONFeature(osmNode, geometry)
  }

  private def osmDenormalisedWayToGeoJSONFeature(osmWay:OsmDenormalizedWay):GeoJSONFeature = {
    val geometry:GeoJSONLineString = geowLinestringToGeoJSONLineString(osmWay.geometry)
    makeGeoJSONFeature(osmWay, geometry)
  }

  private def osmDenormalisedRelationToGeoJSONFeature(osmRelation:OsmDenormalizedRelation):GeoJSONFeature = {
    val (tm, geometry:GeoJSONGeometry) = geowGeometryCollectionToGeoJSONGeometryCollection(osmRelation.geometry)
    val props = propertiesFromOsmObject(osmRelation) ++ tm
    GeoJSONFeature(geometry, Map(props.toList:_*))
  }

  private def geoJSONFeatureToOsmDenormalisedNode(feature:GeoJSONFeature):Option[OsmDenormalizedNode] = {
    val partial = partialOsmObjectFromTags(feature.properties)
    feature.geometry match {
      case p:GeoJSONPoint ⇒ Some(partial.toOsmDenormalisedNode(p))
      case _ ⇒ None
    }
  }

  private def geoJSONFeatureToOsmDenormalisedWay(feature:GeoJSONFeature):Option[OsmDenormalizedWay] = {
    val partial = partialOsmObjectFromTags(feature.properties)
    feature.geometry match {
      case ls:GeoJSONLineString ⇒ Some(partial.toOsmDenormalisedWay(ls))
      case _ ⇒ None
    }
  }

  private def geoJSONFeatureToOsmDenormalisedRelation(feature:GeoJSONFeature):Option[OsmDenormalizedRelation] = {
    val partial = partialOsmObjectFromTags(feature.properties)
    feature.geometry match {
      case gc:GeoJSONGeometryCollection ⇒ Some(partial.toOsmDenormalisedRelation(
        geoJSONGeometryCollectionToGeowGeometryCollection(feature.properties, gc)
      ))
      case _ ⇒ None
    }
  }

  def geoJsonToDenormalized(feature:GeoJSONFeature):Option[OsmDenormalizedObject] =
    feature.geometry match {
      case _:GeoJSONPoint ⇒ geoJSONFeatureToOsmDenormalisedNode(feature)
      case _:GeoJSONLineString ⇒ geoJSONFeatureToOsmDenormalisedWay(feature)
      case _:GeoJSONGeometryCollection ⇒ geoJSONFeatureToOsmDenormalisedRelation(feature)
      case _ ⇒ None
    }

  def geoJsonToDenormalized(featureCollection:FeatureCollection):List[OsmDenormalizedObject] =
    featureCollection.features.foldLeft(List.empty[OsmDenormalizedObject])(
      (objs, f) ⇒ geoJsonToDenormalized(f).map(_ :: objs).getOrElse(objs)
    )

  def denormalizedToGeoJson(osmDenObj:OsmDenormalizedObject):GeoJSONFeature = osmDenObj match {
      case n:OsmDenormalizedNode     ⇒ osmDenormalisedNodeToGeoJSONFeature(n)
      case w:OsmDenormalizedWay      ⇒ osmDenormalisedWayToGeoJSONFeature(w)
      case r:OsmDenormalizedRelation ⇒ osmDenormalisedRelationToGeoJSONFeature(r)
    }

  def denormalizedToGeoJson(osmDenObjs:List[OsmDenormalizedObject]):FeatureCollection =
    osmDenObjs.foldLeft(FeatureCollection(Nil))( (fc, osm) ⇒
      fc.copy(features = denormalizedToGeoJson(osm) :: fc.features) )
}
