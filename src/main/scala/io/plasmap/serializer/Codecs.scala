package io.plasmap.serializer

import io.plasmap.model.geometry._
import io.plasmap.model._
import scodec._
import scodec.bits.BitVector
import scodec.codecs._
import scodec.codecs.literals._

/**
  * Created by mark on 30.11.15.
  */
object Codecs {

  implicit val osmVersionCodec:Codec[OsmVersion] = (
      ("timestamp"     | int64) ::
      ("versionNumber" | uint16) ::
      ("changeset"     | int32) ::
      ("visible"       | bool)
    ).as[OsmVersion]

  implicit val osmIdCodec:Codec[OsmId] =
    ("value" | int64).as[OsmId]

  implicit val osmUserCodec:Codec[OsmUser] = (
    ("username" | utf8_32 ) ::
    ("uid"      | int64)
  ).as[OsmUser]

  implicit val osmTagCodec:Codec[OsmTag] = (
    ("key"   | utf8_32) ::
    ("value" | utf8_32)
  ).as[OsmTag]

  implicit val geometryHashPointCodec:Codec[HashPoint] =
    ("hash" | int64).as[HashPoint]

  implicit val geometryLonLatPointCodec:Codec[LonLatPoint] = (
    ("lon" | double) ::
    ("lat" | double)
  ).as[LonLatPoint]

  import scodec.bits._


  implicit val osmTypeCodec:Codec[OsmType] = mappedEnum(
    uint2, OsmTypeNode -> 0, OsmTypeWay -> 1, OsmTypeRelation -> 2
  )

  //OSM Roles
  implicit val fixedOsmRolesCodec:Codec[OsmRole] = mappedEnum(
    uint2,
    OsmRoleEmpty     -> 0,
    OsmRoleInner     -> 1,
    OsmRoleOuter     -> 2
  )
  implicit val roleOtherCodec:Codec[OsmRoleOther] = ("value" | utf8_32).as[OsmRoleOther]

  implicit val osmRoleCodec:Codec[OsmRole] = discriminatorFallback(
    (constant(bin"11") ~> utf8_32).as[OsmRoleOther], fixedOsmRolesCodec)
    .xmapc(_.fold(identity, identity)){
      case o:OsmRoleOther => Left(o);
      case r => Right(r)
    }

  implicit val osmMemberCodec:Codec[OsmMember] = (
      ("typ"  | osmTypeCodec) ::
      ("ref"  | osmIdCodec)   ::
      ("role" | osmRoleCodec)
    ).as[OsmMember]


  implicit val geometryPointCodec:Codec[Point] =
    discriminated[Point].by(bool)
      .| (true)  {case h:HashPoint   => h} (identity) (geometryHashPointCodec)
      .| (false) {case l:LonLatPoint => l} (identity) (geometryLonLatPointCodec)

  implicit val geometryLineStringCodec:Codec[LineString] =
    ("coordinates" | listOfN(int32, double ~ double))
      .as[LineString]

  implicit def geometryCodec:Codec[Geometry] = lazily {
    discriminated[Geometry].by(uint4)
      .| (0) { case GeometryCollection(geos) =>  geos } (GeometryCollection.apply) (listOfN(int32, geometryCodec))
      .| (1) { case LonLatPoint(lon, lat)   => (lon, lat) } { case (lon, lat) => LonLatPoint(lon, lat)} (double ~ double)
      .| (2) { case HashPoint(hash)         => hash   }  (HashPoint.apply)         (int64)
      .| (3) { case MultiPoint(coords)      => coords }  (MultiPoint.apply)        (listOfN(int32, double ~ double))
      .| (4) { case LineString(coords)      => coords }  (LineString.apply)        (listOfN(int32, double ~ double))
      .| (5) { case MultiLineString(coords) => coords }  (MultiLineString.apply)   (listOfN(int32, listOfN(int32, double ~ double)))
      .| (6) { case Polygon(coords)         => coords }  (Polygon.apply)           (listOfN(int32, listOfN(int32, double ~ double)))
      .| (7) { case MultiPolygon(coords)    => coords }  (MultiPolygon.apply)      (listOfN(int32, listOfN(int32, listOfN(int32, double ~ double))))
  }

  implicit def geometryCollectionCodec:Codec[GeometryCollection] =
    ("coordinates" | listOfN(int32, geometryCodec)).as[GeometryCollection]


  implicit val osmNodeCodec:Codec[OsmNode] = (
      ("id"         | osmIdCodec                  ) ::
      ("user"       | optional(bool, osmUserCodec)) ::
      ("version"    | osmVersionCodec             ) ::
      ("tags"       | listOfN(uint16, osmTagCodec)  ) ::
      ("point"      | geometryPointCodec)
    ).as[OsmNode]

  implicit val osmWayCodec:Codec[OsmWay] = (
      ("id"         | osmIdCodec                  ) ::
      ("user"       | optional(bool, osmUserCodec)) ::
      ("version"    | osmVersionCodec             ) ::
      ("tags"       | listOfN(uint16, osmTagCodec)  ) ::
      ("nds"        | listOfN(int32, osmIdCodec)  )
    ).as[OsmWay]

  implicit val osmRelationCodec:Codec[OsmRelation] = (
      ("id"         | osmIdCodec                  ) ::
      ("user"       | optional(bool, osmUserCodec)) ::
      ("version"    | osmVersionCodec             ) ::
      ("tags"       | listOfN(uint16, osmTagCodec)  ) ::
      ("refs"       | listOfN(int32, osmMemberCodec)  )
    ).as[OsmRelation]

  implicit val osmObjectCodec:Codec[OsmObject] =
    discriminated[OsmObject].by(uint2)
       .| (0) { case n:OsmNode     => n } (identity) (osmNodeCodec)
       .| (1) { case w:OsmWay      => w } (identity) (osmWayCodec)
       .| (2) { case r:OsmRelation => r } (identity) (osmRelationCodec)

  implicit val osmDenormalizedNodeCodec:Codec[OsmDenormalizedNode] = (
      ("id"         | osmIdCodec                  ) ::
      ("user"       | optional(bool, osmUserCodec)) ::
      ("version"    | osmVersionCodec             ) ::
      ("tags"       | listOfN(uint16, osmTagCodec)  ) ::
      ("geometry"   | geometryPointCodec)
    ).as[OsmDenormalizedNode]

  implicit val osmDenormalizedWayCodec:Codec[OsmDenormalizedWay] = (
      ("id"         | osmIdCodec                  ) ::
      ("user"       | optional(bool, osmUserCodec)) ::
      ("version"    | osmVersionCodec             ) ::
      ("tags"       | listOfN(uint16, osmTagCodec)  ) ::
      ("geometry"   | geometryLineStringCodec     )
    ).as[OsmDenormalizedWay]

  implicit val osmDenormalizedRelationCodec:Codec[OsmDenormalizedRelation] = (
      ("id"         | osmIdCodec                  ) ::
      ("user"       | optional(bool, osmUserCodec)) ::
      ("version"    | osmVersionCodec             ) ::
      ("tags"       | listOfN(uint16, osmTagCodec)  ) ::
      ("geometry"   | geometryCollectionCodec     )
    ).as[OsmDenormalizedRelation]

  implicit val osmDenormalizedObjectCodec:Codec[OsmDenormalizedObject] =
    discriminated[OsmDenormalizedObject].by(uint2)
      .| (0) { case n:OsmDenormalizedNode     => n } (identity) (osmDenormalizedNodeCodec)
      .| (1) { case w:OsmDenormalizedWay      => w } (identity) (osmDenormalizedWayCodec)
      .| (2) { case r:OsmDenormalizedRelation => r } (identity) (osmDenormalizedRelationCodec)

}
