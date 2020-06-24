TODO List:
==========
- Implement a 'safe' HecDss$get() method that handles a non-existing path more gracefully.
- Implement write methods for TSCs and PDCs.

Qs for HEC:
===========
Answers from conversation with Karl Tarbet @ HEC (24 Jun 2020)

Goal for dssrip - provide connection through Java API in a way that provides the functions described in DSSVue's Chapter 8 scripting.

Method to do this:
- load location of DSS .jars and javaheclib.dll from environment variables
- initialize rJava's JVM with these
- provides functions to easily get pathnames in file and read into timeseries object in R without doing much r<->Java voodoo

0) Best program to point to for a somewhat stable API?
  API changes like xOrdinates vs xData on PairedDataContainers?
  Other fields that have proper names, what can I count on?
  Future version of DSSVue
  Dev version of DSSvue JARs
  
1) What is the minimum set of jar files to point it to?
Using DSSVue 3.0 jars gives me error about missing DataManager class

lookup.jar ?
hec.jar
rma.jar
images.jar ?

2) Best way to go about opening a DSS file to get to a TimeSeriesContainer / DataContainer
Other classes likely not going away?
-> xOrdinates / xData

3) DSS v6 vs v7 differences that I should watch for?

4) Future API changes to watch for?
  .e.g. field names?
  Is there a better API to use?

Extra Qs:
5) difference between done() and close()?




Old DSS-Rip plan:
=================
this would be an idealized replacement for the Java methods.

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
  