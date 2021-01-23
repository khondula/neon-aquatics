# readme

## files

* `01-map-points.R` downloads shapefiles for aop boxes, domains, reaches, watersheds
* `02-chem-sw.R` unzips and organizes sw chem data by site

## notes 

notes from [geoNEON](https://github.com/NEONScience/NEON-geolocation/tree/master/geoNEON) repo

* Terrestrial and Aquatic Instrumentation (TIS & AIS): Downloaded data are accompanied by a file called sensor_positions that contains coordinates of a reference location and the offsets to the location of each sensor relevant to the data. These locations are also accessible via the NEON API and the geoNEON package.

* Aquatic Observational Sampling (AOS): Observational data collected in aquatic systems include latitude, longitude, elevation, and associated uncertainties. Most AOS data products also report an additional uncertainty that should be added to the reported uncertainty; the user guides for each product describe the spatial data in more detail. The NEON API and geoNEON package can be used to access location data that are not included in the download, such as easting and northing.

## surface water sampling locations 

* fielddata basic file contains a named location eg "HOPB.AOS.S2" and a collectDate eg 2020-12-08T15:08Z
* fieldSuperParent contains named location, decimal lat and lon, elevation, datum, collect date again, event Id, sampler Type (eg Grab)
