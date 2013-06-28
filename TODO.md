DSS-Rip plan


dssfile object
  catalog indexing - faster searching
  get and put methods to read/write RxyzDataContainer objects

rTimeSeriesContainer
  extends the xts object
  represents all data necessary to write into DSS file (metadata)

rDataContainer
  convert (numeric) dataframe into a PairedDataContainer object

rCollectionContainer
  manage collection data as ensemble of runs



Problems to solve:
  Matching times from hecTime objects with XTS timestamps.

