require(raster)
require(rgdal)
require(scales)
require(mapproj)
require(mregions)
require(maptools)

#get EEZ
EEZ <- readOGR(dsn = "eezCHONE/rawdata.Canada-CHONe2014/bathy",layer = "eez")

EEZ <- EEZ[EEZ$GeoName=="Canadian Exclusive Economic Zone",]

#get land
Canada <- getData('GADM', country=c("CAN"), level=1)
USA <- getData('GADM', country=c("USA"), level=0)
Greenland <- getData('GADM', country=c("GRL"), level=0)

# make unique IDs
# Canada <- spChFIDs(Canada,paste0("Canada",1:length(Canada)))
USA <- spChFIDs(USA,paste0("USA",1:length(USA)))
Greenland <- spChFIDs(Greenland,paste0("Greenland",1:length(Greenland)))
USA <- rbind(USA,Greenland)
rm(Greenland)




#set projection
proj <- "+proj=lcc +lon_0=-92.8667"

#plot EEZ
EEZ <- spTransform(EEZ,CRS(proj))
plot(EEZ,border="transparent",col="#97DBF0",bg="#DCFEFD",xlim=c(-2500000,2500000),ylim=c(5000000,11100000))

# plot Canada
Canada <- spTransform(Canada,CRS(proj))
plot(Canada,border="#c0c0a5",col="#ffffda",lwd=0.5,add=TRUE)

USA <- spTransform(USA,CRS(proj))
plot(USA,border="#c0c0a5",col="lightgrey",lwd=0.5,add=TRUE)

# get carbon layers
MethaneHydrates <- readOGR(dsn = "eezCHONE/rawdata.Canada-CHONe2014/bathy",layer = "MethaneHydrates_eez")
MethaneHydrates <- spTransform(MethaneHydrates,CRS(proj))
plot(MethaneHydrates,border="transparent",col="#FFA903",add=TRUE)
rm(MethaneHydrates)

Permafrost <- readOGR(dsn = "eezCHONE/rawdata.Canada-CHONe2014/bathy",layer = "Permafrost_eez")
Permafrost <- spTransform(Permafrost,CRS(proj))
plot(Permafrost,border="transparent",col="#A76F00",add=TRUE)
rm(Permafrost)




easts = c(seq(130,170,20),179.7,seq(-170,-10,20))
norths = seq(0,100,10)

gl = spTransform(gridlines(spTransform(USA,CRS("+proj=longlat")), easts = easts,norths=norths), proj)
lines(gl,col="blue",lwd=0.5)
legend("bottom",
       fill=c("#A76F00","#FFA903","#97DBF0"),
       c("Permafrost","Methane Hydrates","EEZ"))


