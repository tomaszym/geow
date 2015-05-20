# Scala OpenStreetMap API
[![Build Status](https://travis-ci.org/plasmap/geow.svg?branch=master)](https://travis-ci.org/plasmap/geow)

Geow is a lightweight API for processing [OpenStreetMap](http://wiki.openstreetmap.org/wiki/Main_Page) elements.

**Features**:
* Lightweight domain model
* Parsing of Osm files (currently xml is supported)
* High-performance binary serialization using scala pickling
* Support for geometric denormalization (i.e. Osm objects contain the full geometry)
* GeoJSON serialization
* Efficient and flexible geo-hashing utilities

**Planned:**
* Pbf support
* Remove every last trace of Java code

# Installation
Make sure your `build.sbt` contains the Sonatype snapshot resolver.
```scala
resolvers +=
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
```
Then just add a library dependency.
```scala
libraryDependencies ++= Seq(
  "io.plasmap" %% "geow" % "0.3-SNAPSHOT",
)
```

# Usage

## Parsing
Stream Osm files to process Osm objects. Currently files in .osm and .osm.bz2 format are supported. Pbf support is planned for the future.
```scala

  import io.plasmap.parser.OsmParser

  // create a parser from a file
  val parser = OsmParser(fileName)

  // pull openstreetmap data
  for (elem <- parser) println(elem)

```

## Serialization
Serialize and deserialize Osm objects for network transfer.
```scala
  import io.plasmap.serializer.OsmSerializer._

  ... parse or create an osm object element

  val serialized = toBinary(osmObject)
  val deserialized = fromBinary(serialized)
```

# Performance

See the [Benchmarks](https://github.com/geow-org/api/wiki/Benchmarks) wiki page.

# Contributing

If you like to contribute, please create an issue and send a pull request. For more information on pull requests see the [Github pull request tutorial](https://help.github.com/articles/using-pull-requests).
