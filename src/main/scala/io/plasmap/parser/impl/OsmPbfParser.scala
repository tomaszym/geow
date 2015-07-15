package io.plasmap.parser.impl

/**
 * Created by jm on 27/06/15.
 */

import io.plasmap.model.geometry.Point
import io.plasmap.model._
import io.plasmap.parser.OsmParser
import pbfbinaryparser.genclasses.Osmformat.{Relation, Node,DenseNodes, PrimitiveGroup, Way}
import pbfbinaryparser.genclasses.{Osmformat, Fileformat}
import java.util.zip.InflaterInputStream
import shapeless.HList

import scala.annotation.tailrec
import scala.collection.mutable

import java.io.{DataInputStream, File, FileInputStream}


import scala.collection.JavaConverters._


case class OsmPbfParser (fileName: String)  extends OsmParser{
  val dis: DataInputStream = reachFirstBlob(fileName)
  val osmObjects = mutable.Queue[Option[OsmObject]]()

  override def hasNext: Boolean =
    dis.available() != 0 || osmObjects.nonEmpty

  override def next(): Option[OsmObject] = {
    if( osmObjects.isEmpty )
      repopulateQueue(osmObjects, dis)

    osmObjects.dequeue()
  }

  def reachFirstBlob(fileName: String) : DataInputStream = {
    val dis = new DataInputStream(new FileInputStream(new File(fileName)))
    val bhSize: Int = dis.readInt
    val rawBH: Array[Byte] = new Array[Byte](bhSize)
    dis.read(rawBH)
    val bh: Fileformat.BlobHeader = Fileformat.BlobHeader.parseFrom(rawBH)
    val blobSize = bh.getDatasize

    val rawBlob = new Array[Byte](blobSize)
    dis.read(rawBlob)
    val blob: Fileformat.Blob = Fileformat.Blob.parseFrom(rawBlob)

    val hb: Osmformat.HeaderBlock = Osmformat.HeaderBlock.parseFrom(blob.getRaw)
    dis
  }

  def nextBlob(dis : DataInputStream): Fileformat.Blob = {

    val bhSize = dis.readInt()
    val rawBH = new Array[Byte](bhSize)

    dis.read(rawBH)

    val bh = Fileformat.BlobHeader.parseFrom(rawBH)
    val blobSize = bh.getDatasize
    val rawBlob = new Array[Byte](blobSize)

    dis.read(rawBlob)
    Fileformat.Blob.parseFrom(rawBlob)
  }

  case class ForTheMaths(latOffset:Long, lonOffset:Long, geoGranularity:Int, dateGranularity:Int)

  def repopulateQueue(queue: mutable.Queue[Option[OsmObject]], dis: DataInputStream): Unit = {
    val pb = uncompressBlob( nextBlob(dis) )

    //The string table contains all the string of the blob
    val stringTable = pb.getStringtable.getSList.asScala.map(_.toStringUtf8).toArray

    //Necessary to compute a geographical data from the raw lat and lon values of the files + same for the date
    val forTheMaths = ForTheMaths(pb.getLatOffset, pb.getLonOffset, pb.getGranularity, pb.getDateGranularity)


    val primitiveGroups = pb.getPrimitivegroupList.asScala

    primitiveGroups.foreach(
      (pg :PrimitiveGroup) => {

        getDenseNodes(stringTable, forTheMaths, pg.getDense)
          .foreach( n => osmObjects.enqueue(Option(n)) )

        pg.getNodesList.asScala
          .map( getSingleNode(stringTable, forTheMaths, _) )
          .foreach( n => osmObjects.enqueue(Option(n)))

        pg.getRelationsList.asScala
          .map( getRelations(stringTable, _) )
          .foreach( r => osmObjects.enqueue(Option(r)) )

        pg.getWaysList.asScala
          .map( getWays(stringTable , _) )
          .foreach( w => osmObjects.enqueue(Option(w)) )
      }
    )
  }

  def uncompressBlob( blob: Fileformat.Blob): Osmformat.PrimitiveBlock = {
      val unpacker = new InflaterInputStream (blob.getZlibData.newInput() )
      val pb = Osmformat.PrimitiveBlock.parseFrom (unpacker)
      unpacker.close()
      pb
  }

  def getWays(stringTable: Array[String], way: Way): OsmWay = {
    val id = OsmId( way.getId )
    val osmTags = List(
      way.getKeysList.asScala,
      way.getValsList.asScala
    )
      .transpose
      .map( t  => OsmTag(stringTable(t(0).asInstanceOf[Int] ), stringTable(t(1).asInstanceOf[Int]) ) )

    val osmRefs = valFromDelta( way.getRefsList.asScala.toList ).map(OsmId)
    val user = OsmUser( stringTable( way.getInfo.getUserSid ), way.getInfo.getUid)
    val version = way.getInfo.getVersion

    OsmWay( id, Option(user), OsmVersion(version), osmTags, osmRefs)
  }

  def getRelations(stringTable: Array[String], relation: Relation): OsmRelation = {
    val osmId = OsmId(relation.getId)
    val osmUser = OsmUser( stringTable( relation.getInfo.getUserSid ), relation.getInfo.getUid )
    val osmVersion = OsmVersion(  relation.getInfo.getVersion )
    val osmTags =
      List( relation.getKeysList.asScala.toList, relation.getValsList.asScala )
      .transpose
      .map( t  => OsmTag(stringTable(t(0).asInstanceOf[Int] ), stringTable(t(1).asInstanceOf[Int]) ) )

    def getOsmType( sType : Int ): OsmType = stringTable(sType) match {
      case "way"      => OsmTypeWay
      case "relation" => OsmTypeRelation
      case _ => OsmTypeNode
    }

    def getOsmRole( sRole : Int ) : OsmRole = stringTable(sRole) match {
      case "inner" => OsmRoleInner
      case "outer" => OsmRoleOuter
      case ""      => OsmRoleEmpty
      case r       => OsmRoleOther(r)
    }

    val osmMembers = List(
      relation.getTypesList.asScala.map(_.getNumber),
      valFromDelta(relation.getMemidsList.asScala.toList),
      relation.getRolesSidList.asScala
    )
      .transpose
      .map( m => OsmMember(
          getOsmType(m(0).asInstanceOf[Int]),
          OsmId(m(1).asInstanceOf[Long]),
          getOsmRole( m(2).asInstanceOf[Int])
        )
      )

    OsmRelation(osmId, Option(osmUser), osmVersion, osmTags, osmMembers)
  }

  def coordinate(geoGran:Int)(pos:Long, offset:Long):Double = {
    val scaleFactor:Double = 0.000000001
    (pos * geoGran + offset) * scaleFactor
  }


  def getSingleNode(stringTable: Array[String], forTheMaths: ForTheMaths, node: Node): OsmNode = {
    val osmId = OsmId(node.getId)
    val osmUser = OsmUser( stringTable( node.getInfo.getUserSid ), node.getInfo.getUid )
    val osmVersion = OsmVersion(  node.getInfo.getVersion )

    val osmTags =
      List( valFromDelta(node.getKeysList.asScala.toList), valFromDelta(node.getValsList.asScala.toList) )
      .transpose
      .map( t  => OsmTag(stringTable(t(0).asInstanceOf[Int] ), stringTable(t(1).asInstanceOf[Int]) ) )

    val latOffset: Long = forTheMaths.latOffset
    val lonOffset: Long = forTheMaths.lonOffset
    val geoGran: Int = forTheMaths.geoGranularity

    def c = coordinate(geoGran) _

    val lon = c(node.getLon, lonOffset)
    val lat = c(node.getLat, latOffset)
    val geoPoint = Point(lon, lat)

    OsmNode(osmId, Option(osmUser), osmVersion, osmTags, geoPoint )
  }

  def getDenseNodes( stringTable:Array[String], forTheMaths:ForTheMaths, dns:DenseNodes): List[OsmNode] ={
    val valIds: List[Long] = valFromDelta(dns.getIdList.asScala.toList)
    val tags: List[(Int, Int, Long)] = getTagsNodeIds(dns.getKeysValsList.asScala.toList.map(_.toInt), valIds)

    val latOffset: Long = forTheMaths.latOffset
    val lonOffset: Long = forTheMaths.lonOffset
    val geoGran: Int = forTheMaths.geoGranularity
    val dateGran: Int = forTheMaths.dateGranularity


    val grouped: List[(Long, List[(Int, Int, Long)])] =
      tags
        .groupBy({case (key, v, id) => id})
        .toList
        .sortWith( (x,y) => x._1.compareTo(y._1) < 0 )

    def c = coordinate(geoGran) _

    //TODO: This is not nice. There's no reason for Any to be here. This can be inside of a data structure
    val nodes: List[List[Any]] = List(
      grouped,
      valFromDelta(dns.getLonList.asScala.toList).map( (lon : Long) => c(lon, lonOffset)),
      valFromDelta(dns.getLatList.asScala.toList).map( (lat : Long) => c(lat, latOffset)),
      valFromDelta(dns.getDenseinfo.getChangesetList.asScala.toList),
      valFromDelta(dns.getDenseinfo.getTimestampList.asScala.toList).map( (ts : Long) => ts * dateGran),
      valFromDelta(dns.getDenseinfo.getUidList.asScala.toList),
      valFromDelta(dns.getDenseinfo.getUserSidList.asScala.toList),
      dns.getDenseinfo.getVersionList.asScala.map(_.longValue())
    ).transpose

    //TODO: I really don't like this. I propose making a function that returns an Option of a case class.
    //All the casting is bad enough. It should be wrapped inside of a Try.
    //Somewhat like this:
    //case class AlmostOsmNode(id:Long, tags:List[(String, String, Long)],....)
    //for {
    //  id ← Try(xyz.asInstanceOf[Double]).toOption.map(_._1)
    //  tags ← Try(uvw.asInstanceOf[(Long...]).toOption.map(_.2).map(_.map(kvid: (Int, Int, Long))
    // ...
    // } yield AlmostOsmNode(id, tags, ...)
    // So basically wrap anything that could fail in Try(...).toOption or \/.fromTryCatchNonFatal(...).toOption
    val osmNodes = nodes.map(n => { toOsmNode(
        n(0).asInstanceOf[(Long, List[(Int, Int, Long)])]._1,
        n(0).asInstanceOf[(Long, List[(Int, Int, Long)])]._2.map( (kvid : (Int,Int,Long)) => (stringTable(kvid._1), stringTable(kvid._2), kvid._3)),
        n(1).asInstanceOf[Double], //lon
        n(2).asInstanceOf[Double], //lat
        n(3).asInstanceOf[Long],
        n(4).asInstanceOf[Long],
        n(5).asInstanceOf[Long],
        stringTable( n(6).asInstanceOf[Long].toInt ),
        n(7).asInstanceOf[Long])
    })
    osmNodes
  }

  def toOsmNode( id : Long, tags :List[(String,String,Long)], lon : Double, lat: Double, changeSet: Long, timeStamp: Long, uid : Long, user :String, version: Long): OsmNode = {
    val osmUser = OsmUser(user, uid)
    val osmTags = tags.filter(_._1 != "").map( {
      case (key : String, v :String, id :Long) => OsmTag(key, v)
    })
    val osmId = OsmId(id)
    val point = Point(lon, lat)

    OsmNode(osmId, Option(osmUser), OsmVersion(version), osmTags, point)

  }


  def valFromDelta(l : List[Number]): List[Long] = {
    //Nice implementation of scanLeft
//    @tailrec
//    def go( l : List[Number] , delta : Long, acc : List[Long]):  List[Long] = l match {
//      case Nil => acc
//      case h :: t =>
//        val x = h.longValue + delta
//        go( t, x, x :: acc)
//    }
//
//    go(l, 0, List.empty[Long]).reverse
    l.scanLeft(0L)( (delta, number) => number.longValue() + delta).tail
  }
  /*
   * Two succeeding integers in the key value list kv represent the key=value pair of a single <tag> element.
   * The value zero in the kv list represent the move to a new node that can contains several tags or none.
   */
  def getTagsNodeIds( kv : List[Int], ids: List[Long]): List[(Int, Int, Long)] = {
    @tailrec
    def go(kv : List[Int], ids: List[Long], acc :List[(Int, Int, Long)]) : List[(Int, Int, Long)] =
    if (ids.isEmpty) acc
    else kv match {
        case (h :: t) if h == 0 => go(t,      ids.tail,  (0, 0, ids.head) :: acc)
        case (h :: t) if h != 0 => go(t.tail, ids,       (h, t.head, ids.head) :: acc)
    }

    go(kv, ids, List.empty[(Int, Int, Long)])
  }

}
