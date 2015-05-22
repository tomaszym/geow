import java.io.File

import io.plasmap.parser.OsmDenormalizedParser



val parser = OsmDenormalizedParser("/Users/janschulte/plasmap/vm/shared/workspace/geow/src/test/resources/ways.geojson")
for(elem <- parser) println(elem)