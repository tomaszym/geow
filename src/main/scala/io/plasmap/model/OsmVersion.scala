package io.plasmap.model

case class OsmVersion(
                       timestamp: Long = System.currentTimeMillis(),
                       versionNumber: Int = 1,
                       changeset: Int = 1,
                       visible: Boolean = true)