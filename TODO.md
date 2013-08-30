DSS-Rip plan


dssfile object
  catalog indexing - faster searching.
  get and put methods to read/write rDataContainer objects.

rTimeSeriesContainer
  extends the xts object
  represents all data necessary to write into DSS file (metadata).

rDataFrameContainer
  convert (numeric) dataframe into a PairedDataContainer object, stores other DSSmetadata on the side.

rCollectionContainer
  manage collection data as ensemble of rTimeSeriesContainers or rDataFrameContainers.

rTextContainer
  allows storing a string into a DSS database as a readme or to provide other metdata.


Problems to solve / issues to fix:
  Matching times from hecTime objects with XTS timestamps.
