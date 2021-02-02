# neon-aquatics

Repository for getting and organizing surface water chemistry and water quality data and where it was collected. 

## files

* `01-download-data.R` - downloads shapefiles for aop boxes, domains, reaches, watersheds, unzips and organizes sw chem data by site
* `02-swchem-meta.R` - save shapefile of coordinates for sampling locations
* `03-aop-dates.R` - get image datetimes using neon api
* `04-swchem-data.R` - pull all values 
* `05-plots-swchem.R` - makes doc-timeseries.png, doc-x-site.png, tss-x-site.png
* `06-sensor-meta.R` - get locations of sensor positions
* `07-sensor-data.R` - get sensor time series data. rearrange into separate files for each sensor and wq parameter. 

## notes 

notes from [geoNEON](https://github.com/NEONScience/NEON-geolocation/tree/master/geoNEON) repo

* Terrestrial and Aquatic Instrumentation (TIS & AIS): Downloaded data are accompanied by a file called sensor_positions that contains coordinates of a reference location and the offsets to the location of each sensor relevant to the data. These locations are also accessible via the NEON API and the geoNEON package.

* Aquatic Observational Sampling (AOS): Observational data collected in aquatic systems include latitude, longitude, elevation, and associated uncertainties. Most AOS data products also report an additional uncertainty that should be added to the reported uncertainty; the user guides for each product describe the spatial data in more detail. The NEON API and geoNEON package can be used to access location data that are not included in the download, such as easting and northing.

## surface water sampling locations 

* fielddata basic file contains a named location eg "HOPB.AOS.S2" and a collectDate eg 2020-12-08T15:08Z
* fieldSuperParent contains named location, decimal lat and lon, elevation, datum, collect date again, event Id, sampler Type (eg Grab)

## AOP flight collection times

* image date time should be between parentheses but not consistent
* Digital camera: FLHTSTRT_EHCCCCCC(IMAGEDATETIME)-NNNN_ort.tif
* IMAGEDATETIME: Date and time of image capture, YYYYMMDDHHmmSS

## Water quality sensors

* only downstream sensor (S2) has fDOM
* both have SC, DO, pH, chl, turbidity


