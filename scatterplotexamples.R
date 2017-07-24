#to generate 2 random plots
attach(mtcars)
par(mfrow=c(1,2))
plot(wt, mpg, main="Scatterplot Example", 
            xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)
plot(wt, hp, main= "Scatterplot Example 2", xlab="Car Weight", ylab="HP", pch=19)

mtcars <- mtcars

par (mfrow = c(1, 2))
arrows2D(x0 = runif(10), y0 = runif(10),
         x1 = runif(10), y1 = runif(10), colvar = 1:10,
         code = 3, main = "arrows2D")


arrows3D(x0 = wt, y0 = cyl, z0 = hp,
         x1 = wt, y1 = qsec, z1 = hp,
         colvar = 1:32, code = 1:3, main = "arrows3D", colkey = FALSE)


# second way: add dots on bottom and left panel
  # before the scatters are drawn, 
  # add small dots on basal plane and on the depth plane
  panelfirst <- function(pmat) {
         zmin <- min(-quakes$depth)
         XY <- trans3D(quakes$long, quakes$lat, 
                                                z = rep(zmin, nrow(quakes)), pmat = pmat)
         scatter2D(XY$x, XY$y, colvar = quakes$mag, pch = ".", 
                                     cex = 2, add = TRUE, colkey = FALSE)
    
          xmin <- min(quakes$long)
        XY <- trans3D(x = rep(xmin, nrow(quakes)), y = quakes$lat, 
                                                z = -quakes$depth, pmat = pmat)
       scatter2D(XY$x, XY$y, colvar = quakes$mag, pch = ".", 
                                       cex = 2, add = TRUE, colkey = FALSE)
      }

 with(quakes, scatter3D(x = long, y = lat, z = -depth, colvar = mag, 
                                       pch = 16, cex = 1.5, xlab = "longitude", ylab = "latitude", 
                                       zlab = "depth, km", clab = c("Richter","Magnitude"),
                                       main = "Earthquakes off Fiji", ticktype = "detailed", 
                                       panel.first = panelfirst, theta = 10, d = 2, 
                                       colkey = list(length = 0.5, width = 0.5, cex.clab = 0.75))
                  )
 
 ######
 with(quakes, scatter3D(x = long, y = lat, z = -depth, colvar = mag, 
                               pch = 16, cex = 1.5, xlab = "longitude", ylab = "latitude", 
                               zlab = "depth, km", clab = c("Richter","Magnitude"),
                               main = "Earthquakes off Fiji", ticktype = "detailed", 
                               type = "h", theta = 10, d = 2, 
                               colkey = list(length = 0.5, width = 0.5, cex.clab = 0.75))
             )
 quakes <- quakes
 #####

