# set the working directories for our files
setwd('D:/Remote sensing/Remote sensing')
library(raster)
library(rgdal)

# set the extent
e<-extent(838332.7134,862990.3004,-51152.68162,-22688.83612)

#read the shapefile
shp<-readOGR('Export_Output.shp')
crs(shp)
extent(shp)

data<-read.csv(file = 'Sample_points.csv')

#Sentinel images
bs1<- raster('../T36MZE_20190313T074711_B01.jp2')
bs2<- raster('../T36MZE_20190313T074711_B02.jp2')
bs3<- raster('../T36MZE_20190313T074711_B03.jp2')
bs4<- raster('../T36MZE_20190313T074711_B04.jp2')
bs5<- raster('../T36MZE_20190313T074711_B05.jp2')
bs6<- raster('../T36MZE_20190313T074711_B06.jp2')
bs7<- raster('../T36MZE_20190313T074711_B07.jp2')
bs8<- raster('../T36MZE_20190313T074711_B08.jp2')
bs8A<-raster('../T36MZE_20190313T074711_B8A.jp2')
bs9<- raster('../T36MZE_20190313T074711_B09.jp2')
bs10<- raster('../T36MZE_20190313T074711_B10.jp2')
bs11<- raster('../T36MZE_20190313T074711_B11.jp2')
bs12<- raster('../T36MZE_20190313T074711_B12.jp2')

#transform the shapefile
e<-spTransform(shp, CRS(proj4string(bs1)))
e<-extent(e)
e
#create a truecolor stack
truecol<-stack(bs4,bs3,bs2)
truecrop=crop(truecol,e)
#save as png
png(filename="Sentineltrue.png")
plotRGB(truecrop,main="TRUE COLOR COMPOSITE(sentinel)",stretch='lin')
dev.off()

#create a false color composite
bs9=crop(bs9,e)
bs3=crop(bs3,e)
bs4=crop(bs4,e)
bs10=crop(bs10,extent(bs3))
bs11=crop(bs11,extent(bs3))
bs10<-resample(bs10,bs3)
bs11<-resample(bs11,bs3)
falsecol<-stack(bs11,bs4,bs3)
#save to tif 
tiff(filename="Sentinelfalsecolour.tif")
plotRGB(falsecol,main="FALSE COLOR COMPOSITE (Sentinel)",stretch='lin',axes=FALSE)
dev.off()
#crop the band to the same extent
bs3<-crop(bs3,e)
bs1<-crop(bs1,extent(bs3))
bs2<-crop(bs2,extent(bs3))
bs4<-crop(bs4,extent(bs3))
bs5<-crop(bs5,extent(bs3))
bs6<-crop(bs6,extent(bs3))
bs7<-crop(bs7,extent(bs3))
bs8<-crop(bs8,extent(bs3))

#correct for different resolutions and projections
bs1<-resample(bs1,bs3)
bs5<-resample(bs5,bs3)
bs6<-resample(bs6,bs3)
bs7<-resample(bs7,bs3)
bs8<-resample(bs8,bs3)

stck<-stack(bs1,bs2,bs3,bs4,bs5,bs6,bs7,bs8)

compareRaster(bs3,bs8)
stckcrop=crop(stck,e)
stckcrop

#calculate NDWI
ndwi<-(stckcrop[[3]]-stckcrop[[8]])/(stckcrop[[3]]+stckcrop[[8]])
ndwi<-(bs3-bs8)/(bs3+bs8)

png(filename="NDWIpoints1.png")
plot(ndwi,col=rev(terrain.colors(16)),main="NDWI",axes=FALSE)
points(data$X,data$Y,pch=19,col=data$Color,cex=0.8)
text(data$X,data$Y, labels=data$ID,cex=0.8,col=data$Color,offset=0.2, font=2,pos=4)
dev.off()

#convert to a matrix
nr<-getValues(ndwi)
str(nr)
i<-!is.na(nr)
i
#generate random points
set.seed(99)

#kmeansclustering
kmncluster<-kmeans(nr[i],centers=4, iter.max=500,nstart=2,algorithm='Lloyd')
str(kmncluster)

#go back to raster
#you can also do it like this
nr[i]<-kmncluster$cluster
knr<-setValues(ndwi,nr)

#get the colors
mycolor <- c('blue','red','green','brown')

#plot the classification
plot(knr, main='LAND USE LAND COVER(Sentinel)',col=mycolor, axes=TRUE)
#create a legend
legend(x='bottom',y='left', legend=c('Water','urban','Vegetation','Bareland'),
       col=mycolor,text.col = 'black',pch=15,cex=0.7,inset=0.01)


