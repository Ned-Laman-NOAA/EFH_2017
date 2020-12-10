#################	Package testing and loading code  ######################
# Setting lists of required packages & installing it
list_rpackage <- c("rgdal", "raster", "FNN", "xlsx", "lubridate", "xtable","gridGraphics", "maps",
                   "grid", "gridExtra", "mgcv", "dismo", "rJava", "PresenceAbsence", "sp", "here", "maptools",
                   "viridis")
# rJava, rgeos, maps
which_not_installed <- which(list_rpackage %in% rownames(installed.packages()) == FALSE)

if(length(which_not_installed) > 1){
     install.packages(list_rpackage[which_not_installed], dep = TRUE)
     # not necessary unless simulating populations or running VAST
     # if ("INLA" %in% list_rpackage[which_not_installed]){
     # 	install.packages("INLA", repos=c(getOption("repos"),
     # 		INLA = "https://inla.r-inla-download.org/R/stable"), dep = TRUE, quiet = TRUE)
     # 	}
}

# load the packages from the list
lapply(list_rpackage, require, character.only = TRUE)
#################################################################################


########################MAPPING STUFF#####################################
aea.proj <- "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
#############MAP ODDS AND ENDS#################################
AI.xaxis <- c(174,176,178,180,-178,-176,-174,-172,-170,-168,-166)
AI.yaxis <- c(47,49,51,53)
AI.yaxis.ticks <- cbind(c(174,172.5,170.75,168.75), AI.yaxis)
AI.xaxis.ticks <- cbind(AI.xaxis, c(46.9,47.55,48.25,48.85,49,49.5,50.25,50.5,50.75,51.25,51.5))
AI.yaxis.ticks <- rgdal::project(AI.yaxis.ticks, aea.proj)
AI.xaxis.ticks <- rgdal::project(AI.xaxis.ticks, aea.proj)
AI.yaxis.ticks <- AI.yaxis.ticks[, 2]
AI.xaxis.ticks <- AI.xaxis.ticks[, 1]
AI.ext.x <- c(-164.95,-164.95,171,171,-164.95)
AI.ext.y <- c(54.5,50.5,50.5,54.5,54.5)
AI.ext.pol <- sp::Polygon(cbind(AI.ext.x, AI.ext.y))
AI.ext.pol <- sp::Polygons(list(AI.ext.pol), "AI")
AI.ext.pol <- sp::SpatialPolygons(list(AI.ext.pol), proj4string = CRS("+proj=longlat +datum=WGS84"))
AI.ext.pol.proj <- sp::spTransform(AI.ext.pol, CRS(aea.proj))
wrld_p <- maps::map("world", interior = FALSE, plot = FALSE)
llCRS <- sp::CRS("+proj=longlat +ellps=WGS84")
wrld_sp <- maptools::map2SpatialLines(wrld_p, proj4string = llCRS)
prj_new <- sp::CRS("+proj=moll")
wrld_proj <- sp::spTransform(wrld_sp, prj_new)
wrld_grd <- sp::gridlines(wrld_sp, easts = c(-178, seq(-176,178, 2), 180), norths = seq(-75, 75, 2), ndiscr = 100)
wrld_grd_proj2 <- sp::spTransform(wrld_grd, CRS(aea.proj))

#######STEP 7 - PLOT THE RESULTS ##########################################################
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                                 "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

# note that this is analogous to getwd() as long as the .Rproj is located in your working directory
home.dir <- here::here()

training.data <- read.csv(paste0(home.dir, "/variables/Variables_raw_data/AI_Trawl_trainingdata.csv"),
     header = TRUE, stringsAsFactors = FALSE)

test.data <- read.csv(paste0(home.dir, "/variables/Variables_raw_data/AI_Trawl_testdata.csv"),
     header = TRUE, stringsAsFactors = FALSE)


# substituting raster temp for btemp where btemp is null
for(i in 1:length(training.data$btemp)){
     if(is.na(training.data[i, "btemp"]) == TRUE){
          training.data[i, "btemp"] <- training.data[i, "rtemp"]
          }
     }

for(i in 1:length(test.data$btemp)){
     if(is.na(test.data[i, "btemp"]) == TRUE){
          test.data[i, "btemp"] <- test.data[i, "rtemp"]
          }
     }


###MAKE THE 1 KM BASE RASTERS#####
ak.coast <- rgdal::readOGR(dsn = "//akc0ss-n086/SEA_Programs/RACE_EFH_variables/shapefiles", layer = "namerica_dcw", verbose = F)

# (network location is \\akc0ss-n086/SEA_Programs/RACE_EFH_variables)
bathy <- raster::raster("//akc0ss-n086/SEA_Programs/RACE_EFH_variables/Variables/Variables_AI_1km/Bathy")
slope <- raster::raster("//akc0ss-n086/SEA_Programs/RACE_EFH_variables/Variables/Variables_AI_1km/Slope")
color <- raster::raster("//akc0ss-n086/SEA_Programs/RACE_EFH_variables/Variables/Variables_AI_1km/Color")
tmax <- raster::raster("//akc0ss-n086/SEA_Programs/RACE_EFH_variables/Variables/Variables_AI_1km/Tmax")
bcurrent <- raster::raster("//akc0ss-n086/SEA_Programs/RACE_EFH_variables/Variables/Variables_AI_1km/Bcurrent")
btemp <- raster::raster("//akc0ss-n086/SEA_Programs/RACE_EFH_variables/Variables/Variables_AI_1km/Btemp")
lat <- raster::init(bathy, v ='y')
lat <- raster::mask(lat, bathy, filename = "//akc0ss-n086/SEA_Programs/RACE_EFH_variables/Variables/Variables_AI_1km/Lat",
     overwrite = TRUE)
lon <- raster::init(bathy, v ='x')
lon <- raster::mask(lon, bathy, filename = "//akc0ss-n086/SEA_Programs/RACE_EFH_variables/Variables/Variables_AI_1km/Lon",
     overwrite = TRUE)
coral <- raster::raster("//akc0ss-n086/SEA_Programs/RACE_EFH_variables/Variables/Variables_AI_1km/Coralfactor")
sponge <- raster::raster("//akc0ss-n086/SEA_Programs/RACE_EFH_variables/Variables/Variables_AI_1km/Spongefactor")
whips <- raster::raster("//akc0ss-n086/SEA_Programs/RACE_EFH_variables/Variables/Variables_AI_1km/Whipsfactor")

raster.stack <- raster::stack(lon,lat,bathy,slope,btemp,color,bcurrent,tmax, sponge, coral, whips)
names(raster.stack) <- c("lon","lat","bdepth","slope","btemp","color","speed", "tmax", "sponge", "coral", "pen")


ModelPlan <- read.csv(paste0(home.dir, "/ModelPlan.csv"), header = TRUE, stringsAsFactors = FALSE)
ModelPlan <- subset(ModelPlan,ModelPlan$Region == "AI")
species <- c(ModelPlan$Trawl_name_juvenile, ModelPlan$Trawl_name_adult)
models <- c(ModelPlan$Trawl_model_juvenile, ModelPlan$Trawl_model_adult)
start_year <- rep(ModelPlan$Start_year, 2)
species_name <- c(paste0("Juvenile_", ModelPlan$Species), paste0("Adult_", ModelPlan$Species))
species_name <- species_name[species != ""]
models <- models[species != ""]
start_year <- start_year[species != ""]
species <- species[species != ""]

#j<-1
#k<-1

species_pct <- species

for(j in c(3,29,36)){
     # for(j in 1:length(species)){
     if(start_year[j] > 1990){
          training.dat <- subset(training.data, training.data$year >= start_year[j])
          test.dat <- subset(test.data, test.data$year >= start_year[j])
          }

     if(start_year[j] == 0){
          training.dat <- training.data
          test.dat <- test.data
          }

     t1 <- (length(subset(training.dat[, species[j]], training.dat[, species[j]] > 0)) + length(subset(test.dat[, species[j]],
          test.dat[, species[j]] > 0)))/(length(training.dat[, 1]) + length(test.dat[, 1]))
     species_pct[j] <- t1

     print(paste(species_name[j], round(t1, 2)))

     results.path <- paste0(home.dir, "/Trawl_models/AI/", species_name[j])
     # if writing to an existing directory this will allow original directory to stand and suppress the warning that it exists
     # will need to be paired with overwrite = T down the line where necessary
     dir.create(results.path)


     ####################################################################################################################################
     ####################################################################################################################################
     ##################################################CPUE MODEL######################################################################
     ####################################################################################################################################

     if(models[j] == "gam"){

          yvar <- species[j]

          ##########################################################################
          ## To cope with the possibility that a presence-absence factor could
          ## have zero contrast (i.e., 100% present or 100% absent), I built a test
          ## to sequentially identify which factors had contrast and thus could be
          ## included in the formula.

          # continuous x variables
          base.form <- c("s(lon,lat,k=10)", "s(slope,k=4)", "s(tmax,k=4)", "s(color,k=4)", "s(speed,k=4)", "s(btemp,k=4)",
                         "s(bdepth,k=4)")
          # empty vector to accept strings below
          factor.form <- vector()
          # testing factors - if length of unique values == 1 then no contrast. So if >= 2 then include factor term
          # written to serially include and accumulate terms as they qualify
          if(length(unique(training.dat$sponge)) >= 2){
               factor.form <- "as.factor(sponge)"
               }
          if(length(unique(training.dat$coral)) >= 2){
               factor.form <- c(factor.form, "as.factor(coral)")
               }
          if(length(unique(training.dat$pen)) >= 2){
               factor.form <- c(factor.form, "as.factor(pen)")
               }

          xvars <- c(base.form, factor.form)

          gam.form <- as.formula(paste(yvar, "~", paste(xvars, collapse = "+")))

          for(i in 1:length(xvars)){

               cpue.gam <- mgcv::gam(gam.form, family = gaussian, data = training.dat)
               gcv_gam <- cpue.gam$gcv.ubre
               pvals <- summary(cpue.gam)$s.pv
               pvals <- c(pvals, summary(cpue.gam)$p.pv[-1])
               least_sig <- which.max(pvals)

               xvars1 <- xvars[-least_sig]
               gam.form1 <- as.formula(paste(yvar, "~", paste(xvars1, collapse = "+")))
               cpue.gam1 <- mgcv::gam(gam.form1, family = gaussian, data = training.dat)
               gcv_gam1 <- cpue.gam1$gcv.ubre


               if(gcv_gam > gcv_gam1){
               	gam.form <- gam.form1
               	gcv_gam <- gcv_gam1
               	xvars1 <- xvars[-least_sig]
               	print(summary(cpue.gam1))
               	}
               if(gcv_gam<gcv_gam1)break
               }

          ######## Training Data

          pred2 <- predict(cpue.gam, training.dat, fun = predict, type = "response")
          cpue.obs.pred <- data.frame(cbind(x = training.dat[species[j]]^1, y = pred2))
          cpue.obs.pred <- na.omit(cpue.obs.pred)
          colnames(cpue.obs.pred)[1] <- "x"

          ######### Test Data
          pred4 <- predict(cpue.gam, test.dat, fun = predict, type = "response")
          t.yvar <- test.dat[species[j]]

          t.cpue.obs.pred <- data.frame(cbind(x = t.yvar, y = pred4))
          colnames(t.cpue.obs.pred)[1] <- "x"

          ######## Prediction Map
          predict.CPUE.raster1 <- predict(raster.stack, cpue.gam, fun = predict, na.rm = TRUE, overwrite = TRUE,
               progress = "text", type = "response", newdata.guaranteed = TRUE)
          predict.CPUE.raster <- raster::mask(predict.CPUE.raster1, ak.coast, inverse = TRUE, overwrite = TRUE,
               progress = "text", filename = paste0(results.path, "/CPUEpredict"))

          sample1 <- sampleRandom(predict.CPUE.raster, 600000, na.rm = TRUE)
          sample1[sample1 <= 0] <- NA
          breaks <- quantile(sample1, probs = c(0,0.05,0.25,0.5,0.75,1), na.rm = TRUE, names = FALSE)

          EFH.raster <- cut(predict.CPUE.raster, breaks = breaks, overwrite = TRUE,
               filename = paste0(results.path, "/EFHmap"), progress = "text")
          save.image(paste0(results.path, "/", species_name[j], ".RData"))

          ####FIGURE 1 - GAM DIAGNOSTICS PLOTS
          dir.create(paste0(results.path, "/Figures"))

          png(filename = paste0(results.path, "/Figures/CPUEGAMdiagnostics.png"),
              width = 8.5, height = 11, res = 300, units = "in")
          par(mfcol = c(3,2), family = "sans", mar = c(4,4,3,1))

          qqnorm((cpue.obs.pred$x-cpue.obs.pred$y), main = "Training data")
          qqline((cpue.obs.pred$x-cpue.obs.pred$y))
          hist((cpue.obs.pred$x-cpue.obs.pred$y), xlab = "Residuals", main = "")
          regr <- lm(y~x, data = cpue.obs.pred)
          rsqr <- summary(lm(y~x, data = cpue.obs.pred))$r.squared
          adj.rsqr <- summary(lm(y~x, data = cpue.obs.pred))$adj.r.squared
          pred.max <- max(cpue.obs.pred$y, na.rm = T)
          obs.max <- max(cpue.obs.pred$x, na.rm = T)

          if(pred.max > obs.max){
          	plot.max = pred.max + (0.04*pred.max)
          }else{
          	plot.max = obs.max + (0.04*obs.max)
          	}

          par(xaxs = "i", yaxs = "i")
          plot(cpue.obs.pred$x, cpue.obs.pred$y, ylim = c(0,plot.max), xlim = c(0,plot.max), ylab = "Predicted",
               xlab = "Observed", main = "", pch = 20)
          abline(coef = c(0,1), lty = 2)
          lines(cpue.obs.pred$x, regr$fitted.values)
          text(1, plot.max-1, paste("R-squared = ", round(rsqr,2)), pos = 4)

          #Plots for test data
          qqnorm((t.cpue.obs.pred$x-t.cpue.obs.pred$y), main = "Test data")
          qqline((t.cpue.obs.pred$x-t.cpue.obs.pred$y))
          hist((t.cpue.obs.pred$x-t.cpue.obs.pred$y), xlab = "Residuals", main = "")
          regr <- lm(y~x, data = t.cpue.obs.pred)
          rsqr <- summary(lm(y~x, data = t.cpue.obs.pred))$r.squared
          adj.rsqr <- summary(lm(y~x, data = t.cpue.obs.pred))$adj.r.squared
          pred.max <- max(t.cpue.obs.pred$y)
          obs.max <- max(t.cpue.obs.pred$x)

          if(pred.max > obs.max){
          	plot.max = pred.max + (0.04*pred.max)
          }else{
          	plot.max = obs.max + (0.04*obs.max)
               }

          par(xaxs = "i", yaxs = "i")
          plot(t.cpue.obs.pred$x, t.cpue.obs.pred$y, ylim = c(0,plot.max), xlim = c(0,plot.max), ylab = "Predicted",
               xlab = "Observed", main = "", pch = 20)
          abline(coef = c(0,1), lty = 2)
          lines(t.cpue.obs.pred$x, regr$fitted.values)
          text(1, plot.max-1, paste("R-squared = ", round(rsqr,2)), pos = 4)
          dev.off()

          ####FIGURE 2 - GAM EFFECTS PLOT################################
          png(filename = paste0(results.path, "/Figures/CPUEGAMplots.png"), width = 8.5, height = 11, res = 300, units = "in")
          n1 <- summary(cpue.gam)$m
          n2 <- length(summary(cpue.gam)$p.t)
          n2 <- ifelse(n2>0,n2-1,0)+n1
          v1 <- all.vars(formula(cpue.gam))
          v1 <- v1[-1]

          if(v1[1] == "lon"){
               v1[1] <- "lonlat"
          	v1 <- v1[-2]
          	}

          par(mfrow = c(ceiling(n2/3), 3), mar = c(4,4,2,.01))
          for(i in 1:n2){
          	if(v1[i] == "lonlat"){plot(cpue.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
          	    xlab = "Longitude (northings)", ylab = "Latitude (eastings)") }
          	if(v1[i] == "slope"){plot(cpue.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
          	    ylab = "Variable effect", xlab = "Slope (%)")  }
          	if(v1[i] == "tmax"){plot(cpue.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
          	    ylab = "Variable effect", xlab = "Tidal current speed (cm/s)")}
          	if(v1[i] == "color"){plot(cpue.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
          	    ylab = "Variable effect", xlab = "Ocean color")  }
          	if(v1[i] == "speed"){plot(cpue.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
          	    ylab = "Variable effect", xlab = "Current speed (m/s)")}
          	if(v1[i] == "btemp"){plot(cpue.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
          	    ylab = "Variable effect", xlab = "Temperature (C)")}
          	if(v1[i] == "bdepth"){plot(cpue.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
          	    ylab = "Variable effect", xlab = "Depth (m)")}
          	if(v1[i] == "sponge"){plot(cpue.gam, select = i, all.terms = TRUE, ylabs = "Variable effect",
          	    xlabs = "Sponge presence")}
          	if(v1[i] == "coral"){plot(cpue.gam, select = i, all.terms = TRUE, ylabs = "Variable effect",
          	    xlabs = "Coral presence")}
          	if(v1[i] == "pen"){plot(cpue.gam, select = i, all.terms = TRUE, ylabs = "Variable effect",
          	    xlabs = "Pennatulacean presence")}}
          dev.off()


          ######FIGURE 3 - PREDICTED ABUNDANCE MAP ##################################################

          png(filename = paste0(results.path, "/Figures/CPUEGAMmap.png"), width = 7.1, height = 5, res = 300, units = "in")
          par(mfrow = c(1,1), mar = c(5,4,1,1), family = "sans")
          plot(predict.CPUE.raster, main = "", xaxt = "n", yaxt = "n", box = F, col = jet.colors(255), ext = AI.ext.pol.proj,
               legend.shrink = 0.5, axis.args = list(cex.axis = 0.65), legend.args = list(text = "Abundance", cex = 0.65,
               cex.lab = 0.65, side = 1, line = 2), horiz = TRUE, ylab = "Latitude", xlab = "Longitude",
               zlim = c(0, maxValue(predict.CPUE.raster)))
          plot(ak.coast, col = "black", add = TRUE)
          plot(wrld_grd_proj2, lty = 3, col = "lightgrey", add = T)
          axis(1, at = AI.xaxis.ticks, labels = AI.xaxis)
          axis(2, at = AI.yaxis.ticks, labels = AI.yaxis)
          dev.off()

          ######FIGURE 4 - EFH MAP ##################################################
          png(filename = paste0(results.path, "/Figures/EFHmap.png"), width = 7.1, height = 5, res = 300, units = "in")
          par(mfrow = c(1,1), mar = c(5,4,1,1), family = "sans")
          plot(EFH.raster, main = "", xaxt = "n", yaxt = "n", box = F, col = colorRampPalette(c("grey","coral3"))(4),
               ext = AI.ext.pol.proj, legend = FALSE, ylab = "Latitude", xlab = "Longitude", horiz = TRUE,
               zlim = c(2, maxValue(EFH.raster)))
          plot(ak.coast, col = "black", add = TRUE)
          plot(wrld_grd_proj2, lty = 3, col = "lightgrey", add = T)
          axis(1, at = AI.xaxis.ticks, labels = AI.xaxis)
          axis(2, at = AI.yaxis.ticks, labels = AI.yaxis)
          legVals <- c("95%","75%","50%","25%")
          legend("bottomleft", legend = legVals, pch = 15, col = colorRampPalette(c("grey","coral3"))(4), bty = "n",
               pt.cex = 2 , title = "Percentiles", cex = 1)
          depth.contour <- contour(raster.stack[["bdepth"]], levels = c(100,300,500), col = "black", lwd = 0.05,
               drawlabels = FALSE, add = TRUE)
          dev.off()


          ##########################WRITE RASTERS TO ArcGIS FORMAT GeoTIFF's############################################
          raster::writeRaster(predict.CPUE.raster, filename = paste0(home.dir, "/GeoTiffs/GeoTiffs_AI/", species_name [j],
               "_CPUE_map.tif"), overwrite = TRUE, format = "GTiff", options = c("PROFILE=GeoTIFF", "TFW=YES"), NAflag = -999)
          raster::writeRaster(EFH.raster, filename = paste0(home.dir, "/GeoTiffs/GeoTiffs_AI/", species_name[j], "_EFHmap.tif"),
               overwrite = TRUE, format = "GTiff", options = c("PROFILE=GeoTIFF", "TFW=YES"), NAflag = -999)

          ########TABLE 1 - SUMMARY TABLE FOR CPUE GAM#######################

          n1 <- summary(cpue.gam)$m
          n2 <- length(summary(cpue.gam)$p.t)
          n2 <- ifelse(n2>0,n2-1,0)+n1
          v1 <- all.vars(formula(cpue.gam))
          v1 <- v1[-1]

          if(v1[1] == "lon"){
               v1[1] <- "lonlat"
          	v1 <- v1[-2]
          	}

          table1 <- array("", dim = c(length(v1)+2, 6))
          table1[1,] <- c(" ", " ", " ", " ", "Training data", "Test data")
          table1[2,] <- c("Model term", "EDF", "p-value", "Deviance explained", "rsq", "rsq")
          table1[3,4] <- round(summary(cpue.gam)$dev.expl, 3)
          table1[3,5] <- rsqr <- round(summary(lm(y~x, data = cpue.obs.pred))$r.squared, 2)
          table1[3,6] <- rsqr <- round(summary(lm(y~x, data = t.cpue.obs.pred))$r.squared, 2)

          table1a <- v1
          table1a[table1a == "lonlat"] <- "Longitude*latitude"
          table1a[table1a == "slope"] <- "slope"
          table1a[table1a == "pen"] <- "Pennatulacean present"
          table1a[table1a == "coral"] <- "Coral present"
          table1a[table1a == "sponge"] <- "Sponge present"
          table1a[table1a == "color"] <- "Ocean color"
          table1a[table1a == "speed"] <- "Current speed"
          table1a[table1a == "bdepth"] <- "Depth"
          table1a[table1a == "tmax"] <- "Tidal current"
          table1a[table1a == "btemp"] <- "Temperature"

          table1b <- c(summary(cpue.gam)$edf, summary(cpue.gam)$pTerms.df)
          table1c <- c(summary(cpue.gam)$s.pv, summary(cpue.gam)$pTerms.pv)

          table1[3:(length(v1)+2), 1] <- table1a[order(table1c)]
          table1[3:(length(v1)+2), 2] <- signif(table1b[order(table1c)], digits = 2)
          table1[3:(length(v1)+2), 3] <- signif(table1c[order(table1c)], digits = 3)
          cpue.table <- xtable::xtable(table1)

          xtable::print.xtable(cpue.table, type = "html", file = paste0(results.path, "/Figures/cpuegamtable.html"),
               include.rownames = getOption("xtable.include.rownames", FALSE), html.table.attributes = 2,
               include.colnames = getOption("xtable.include.colnames", FALSE),  hline.after = getOption("xtable.hline.after",
               c(-1, 1, nrow(table1))))
          print(paste(species[j], "Catch model complete"))
          }

     ####################################################################################################################################
     ####################################################################################################################################
     ##################################################HURDLE MODEL######################################################################
     ####################################################################################################################################


     if(models[j]=="hgam"){

     ######PRESENCE ABSENCE PART#########
     yvar <- species[j]
     pa.dat <- training.dat
     # pa.dat <- na.omit(pa.dat)
     eval(parse(text = paste0("pa.dat$", yvar, "<- ifelse(", "pa.dat$", yvar, "> 0, 1, 0)")))
     xvars <- c("s(lon,lat, k=10)", "s(slope, k=4)", "s(tmax, k=4)", "s(color, k=4)", "s(speed, k=4)", "s(btemp, k=4)",
                "s(bdepth,k=4)", "as.factor(sponge)", "as.factor(coral)", "as.factor(pen)")
     gam.form <- as.formula(paste(yvar, "~", paste(xvars, collapse = "+")))

    for(i in 1:length(xvars)){
     pa.gam <- mgcv::gam(gam.form, family = binomial, data = pa.dat)
     gcv_gam <- pa.gam$gcv.ubre
     	pvals <- summary(pa.gam)$s.pv
     	pvals <- c(pvals,summary(pa.gam)$p.pv[-1])
     	least_sig <- which.max(pvals)

     xvars1 <- xvars[-least_sig]
     gam.form1 <- as.formula(paste(yvar, "~", paste(xvars1,collapse = "+")))
     pa.gam1 <- mgcv::gam(gam.form1, family = binomial, data = pa.dat)
     gcv_gam1 <- pa.gam1$gcv.ubre

     if(gcv_gam > gcv_gam1){
     	gam.form <- gam.form1
     	gcv_gam <- gcv_gam1
     	xvars <- xvars[-least_sig]
     	print(summary(pa.gam1))
     	}
     if(gcv_gam<gcv_gam1)
          break
          }

     ######## Training Data

     pred2 <- predict(pa.gam, pa.dat, fun = predict, na.rm = TRUE, overwrite = TRUE, progress = "text",
                      type = "response", newdata.guaranteed = TRUE)
     eval(parse(text = paste0("pa.obs.pred <- data.frame(cbind(x = pa.dat$", yvar, ", y = pred2))")))
     colnames(pa.obs.pred)[1] <- "x"
     auc.dat <- data.frame(cbind(seq(1,length(pred2)), pa.obs.pred))
     print(auc(auc.dat, na.rm = TRUE))
     print(PresenceAbsence::optimal.thresholds(auc.dat, opt.methods = c(seq(1:9))))
     thresh <- PresenceAbsence::optimal.thresholds(auc.dat, opt.methods = 2)
     thresh <- thresh[,2]
     print(PresenceAbsence::cmx(auc.dat, threshold = thresh))

     ######### Test Data
     t.pa.dat <- test.dat
     eval(parse(text = paste0("t.pa.dat$", yvar, "<- ifelse(", "t.pa.dat$", yvar, "> 0, 1, 0)")))
     pred4 <- predict(pa.gam, t.pa.dat, fun = predict, na.rm = TRUE, overwrite = TRUE, progress = "text",
                      type = "response", newdata.guaranteed = TRUE)
     # .pa.obs.pred <- data.frame(cbind(x = t.yvar, y = pred4))
     eval(parse(text = paste0("t.pa.obs.pred <- data.frame(cbind(x = t.pa.dat$", yvar, ", y = pred4))")))
     colnames(t.pa.obs.pred)[1] <- "x"
     t.auc.dat <- data.frame(cbind(seq(1,length(pred4)), t.pa.obs.pred))
     print(PresenceAbsence::auc(t.auc.dat, na.rm = TRUE))
     print(PresenceAbsence::cmx(t.auc.dat, threshold = thresh))

     ######## Prediction Map
     predict.pa.raster1 <- predict(raster.stack, pa.gam, fun = predict, na.rm = TRUE, overwrite = TRUE, progress = "text",
          type = "response", newdata.guaranteed = TRUE)
     predict.pa.raster <- raster::mask(predict.pa.raster1, ak.coast, inverse = TRUE, overwrite = TRUE, progress = "text",
          filename = paste(results.path, "/PApredict", sep = ""))
     breaks = c(thresh,1)
     pa.raster.cut <- raster::cut(predict.pa.raster, breaks = breaks)

     ######CPUE PART##########
     training.dat.catch <- subset(training.dat, training.dat[species[j]] > 0)
     test.dat.catch <- subset(test.dat, test.dat[species[j]] > 0)

     ##########################################################################
     ## To cope with the possibility that a presence-absence factor could
     ## have zero contrast (i.e., 100% present or 100% absent), I built a test
     ## to sequentially identify which factors had contrast and thus could be
     ## included in the formula.
     ## Does not appaer to be a problem for the pagam with a binomial distribution

     # continuous x variables
     base.form <- c("s(lon,lat,k=10)", "s(slope,k=4)", "s(tmax,k=4)", "s(color,k=4)", "s(speed,k=4)", "s(btemp,k=4)",
                    "s(bdepth,k=4)")
     # empty vector to accept strings below
     factor.form <- vector()
     # testing factors - if length of unique values == 1 then no contrast. So if >= 2 then include factor term
     # written to serially include and accumulate terms as they qualify
     if(length(unique(training.dat.catch$sponge)) >= 2){
          factor.form <- "as.factor(sponge)"
          }
     if(length(unique(training.dat.catch$coral)) >= 2){
          factor.form <- c(factor.form, "as.factor(coral)")
          }
     if(length(unique(training.dat.catch$pen)) >= 2){
          factor.form <- c(factor.form, "as.factor(pen)")
          }

     xvars <- c(base.form, factor.form)

     gam.form <- as.formula(paste(yvar, "~", paste(xvars, collapse = "+")))

     for(i in 1:length(xvars)){
          cpue.gam <- mgcv::gam(gam.form, family = gaussian, data = training.dat.catch)
          gcv_gam <- cpue.gam$gcv.ubre
          pvals <- summary(cpue.gam)$s.pv
          pvals <- c(pvals,summary(cpue.gam)$p.pv[-1])
          least_sig <- which.max(pvals)

          xvars1 <- xvars[-least_sig]
          gam.form1 <- as.formula(paste(yvar, "~", paste(xvars1, collapse = "+")))
          cpue.gam1 <- mgcv::gam(gam.form1, family = gaussian, data = training.dat.catch)
          gcv_gam1 <- cpue.gam1$gcv.ubre


          if(gcv_gam > gcv_gam1){
               gam.form <- gam.form1
               gcv_gam <- gcv_gam1
               xvars <- xvars[-least_sig]
               print(summary(cpue.gam1))
               }
          if(gcv_gam < gcv_gam1)
               break
          }

     ######## Training Data

     pred2 <- predict(cpue.gam, training.dat.catch, fun = predict, na.rm = TRUE, overwrite = TRUE, progress = "text",
          type = "response", newdata.guaranteed = TRUE)
     eval(parse(text = paste0("cpue.obs.pred <- data.frame(cbind(x = training.dat.catch$", yvar, ", y = pred2))")))
     colnames(cpue.obs.pred)[1] <- "x"

     ######### Test Data
     pred4 <- predict(cpue.gam, test.dat.catch, fun = predict, na.rm = TRUE, overwrite = TRUE, progress = "text",
          type = "response", newdata.guaranteed = TRUE)
     t.yvar <- test.dat.catch[species[j]]
     t.cpue.obs.pred <- data.frame(cbind(x = t.yvar, y = pred4))
     colnames(t.cpue.obs.pred)[1] <- "x"

     ######## Prediction Map
     predict.CPUE.raster1 <- predict(raster.stack, cpue.gam, fun = predict, na.rm = TRUE, overwrite = TRUE, progress = "text",
          type = "response", newdata.guaranteed = TRUE)
     predict.CPUE.raster2 <- raster::mask(predict.CPUE.raster1, ak.coast, inverse = TRUE, overwrite = TRUE, progress = "text")
     predict.CPUE.raster <- raster::mask(predict.CPUE.raster2, pa.raster.cut, overwrite = TRUE, progress = "text",
          filename = paste0(results.path, "/CPUEpredict"))

     sample1 <- sampleRandom(predict.CPUE.raster, 600000, na.rm = TRUE)
     sample1[sample1 <= 0] <- NA
     breaks <- quantile(sample1, probs =c(0,0.05,0.25,0.5,0.75,1), na.rm = TRUE, names = FALSE)

     EFH.raster <- cut(predict.CPUE.raster, breaks = breaks, overwrite = TRUE,
          filename = paste0(results.path, "/EFHmap"), progress = "text")
     save.image(paste0(results.path, "/", species_name[j], ".RData"))

     ####FIGURE 1 - PA GAM EFFECTS PLOT################################
     dir.create(paste0(results.path, "/Figures"))
     png(filename = paste0(results.path, "/Figures/PAGAMplots.png"), width = 8.5, height = 11, res = 300, units = "in")
     n1 <- summary(pa.gam)$m
     n2 <- length(summary(pa.gam)$p.t)
     n2 <- ifelse(n2>0, n2-1, 0) + n1
     v1 <- all.vars(formula(pa.gam))
     v1 <- v1[-1]

     if(v1[1] == "lon"){
          v1[1] <- "lonlat"
     	v1 <- v1[-2]
     	}

     par(mfrow = c(ceiling(n2/3), 3), mar = c(4, 4, 2, 0.01))
     for(i in 1:n2){
     	if(v1[i] == "lonlat"){plot(pa.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
     	    xlab = "Longitude (northings)", ylab = "Latitude (eastings)") }
     	if(v1[i] == "slope"){plot(pa.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
     	    ylab = "Variable effect", xlab = "Slope (%)")  }
     	if(v1[i] == "tmax"){plot(pa.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
     	    ylab = "Variable effect", xlab = "Tidal current speed (cm/s)")}
     	if(v1[i] == "color"){plot(pa.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
     	    ylab = "Variable effect", xlab = "Ocean color")  }
     	if(v1[i] == "speed"){plot(pa.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
     	    ylab = "Variable effect", xlab = "Current speed (m/s)")}
     	if(v1[i] == "btemp"){plot(pa.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
     	    ylab = "Variable effect", xlab = "Temperature (C)")}
     	if(v1[i] == "bdepth"){plot(pa.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
     	    ylab = "Variable effect", xlab = "Depth (m)")}
     	if(v1[i] == "sponge"){plot(pa.gam, select = i, all.terms = TRUE, ylabs = "Variable effect", xlabs = "Sponge presence")}
     	if(v1[i] == "coral"){plot(pa.gam, select = i, all.terms = TRUE, ylabs = "Variable effect", xlabs = "Coral presence")}
     	if(v1[i] == "pen"){plot(pa.gam, select = i, all.terms = TRUE, ylabs = "Variable effect", xlabs = "Pennatulacean presence")}}
     dev.off()

     ####FIGURE 2 - PA GAM DIAGNOSTICS PLOTS
     #Plots for training data
     png(filename = paste0(results.path, "/Figures/PAGAMdiagnostics.png"), width = 8.5, height = 11, res = 300, units = "in")
     par(mfcol = c(3,2), family = "sans", mar = c(4, 4, 1, 0.01))
     PresenceAbsence::auc.roc.plot(auc.dat, opt.methods = 2, main = "", add.legend = F, xlab = "Specificity", ylab = "Sensitivity",
          add.opt.legend = F)
     text(0, 0.95, "Training data", pos = 4, cex = 1.25)
     PresenceAbsence::calibration.plot(auc.dat, N.bins = 10, xlab = "Predicted occurence", ylab = "Proportion of observed occurence", main = "")
     PresenceAbsence::presence.absence.hist(auc.dat, truncate.tallest = TRUE, main = "", ylab = "Number of observations",
          xlab = "Predicted probability")

     #Plots for test data
     PresenceAbsence::auc.roc.plot(t.auc.dat, main = "", add.legend = F, xlab = "Specificity", ylab = "Sensitivity", add.opt.legend = F)
     text(0, 0.95, "Test data", pos = 4, cex = 1.25)
     PresenceAbsence::calibration.plot(t.auc.dat, N.bins = 10, xlab = "Predicted occurence", ylab = "Proportion of observed occurence", main = "")
     PresenceAbsence::presence.absence.hist(t.auc.dat, truncate.tallest = TRUE, main = "", ylab = "Number of observations",
          xlab = "Predicted probability")
     dev.off()

     ######FIGURE 3 - PREDICTED PRESENCE MAP ##################################################

     png(filename = paste0(results.path, "/Figures/PAGAMmap.png"), width = 7.1, height = 5, res = 300, units = "in")
     par(mfrow = c(1,1), mar = c(5,4,1,1), family = "sans")
     plot(predict.pa.raster, main = "", xaxt = "n", yaxt = "n", box = F,col = jet.colors(255), ext = AI.ext.pol.proj,
          legend.shrink = 0.5, axis.args = list(cex.axis = 0.65), legend.args = list(text = "Probability of presence",
          cex = 0.65, cex.lab = 0.65, side = 1, line = 2), horiz = TRUE, ylab = "Latitude", xlab = "Longitude", zlim = c(0,1))
     plot(ak.coast, col = "black", add = TRUE)
     plot(wrld_grd_proj2, lty = 3, col = "lightgrey", add = T)
     axis(1, at = AI.xaxis.ticks, labels = AI.xaxis)
     axis(2, at = AI.yaxis.ticks, labels = AI.yaxis)
     dev.off()

     ####FIGURE 4 - CPUE GAM EFFECTS PLOT################################
     png(filename = paste0(results.path, "/Figures/CPUEGAMplots.png"), width = 8.5, height = 11, res = 300, units = "in")
     n1 <- summary(cpue.gam)$m
     n2 <- length(summary(cpue.gam)$p.t)
     n2 <- ifelse(n2 > 0, n2-1, 0) + n1
     v1 <- all.vars(formula(cpue.gam))
     v1 <- v1[-1]

     if(v1[1] == "lon"){
          v1[1] <- "lonlat"
     	v1 <- v1[-2]
     	}

     par(mfrow = c(ceiling(n2/3), 3), mar = c(4, 4, 2, 0.01))
     for(i in 1:n2){
     	if(v1[i] == "lonlat"){plot(cpue.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
     	    xlab = "Longitude (northings)", ylab = "Latitude (eastings)") }
     	if(v1[i] == "slope"){plot(cpue.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
     	    ylab = "Variable effect", xlab = "Slope (%)")  }
     	if(v1[i] == "tmax"){plot(cpue.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
     	    ylab = "Variable effect", xlab = "Tidal current speed (cm/s)")}
     	if(v1[i] == "color"){plot(cpue.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
     	    ylab = "Variable effect", xlab = "Ocean color")  }
     	if(v1[i] == "speed"){plot(cpue.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
     	    ylab = "Variable effect", xlab = "Current speed (m/s)")}
     	if(v1[i] == "btemp"){plot(cpue.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
     	    ylab = "Variable effect", xlab = "Temperature (C)")}
     	if(v1[i] == "bdepth"){plot(cpue.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
     	    ylab = "Variable effect", xlab = "Depth (m)")}
     	if(v1[i] == "sponge"){plot(cpue.gam,select = i, all.terms = TRUE, ylabs = "Variable effect", xlabs = "Sponge presence")}
     	if(v1[i] == "coral"){plot(cpue.gam, select = i, all.terms = TRUE, ylabs = "Variable effect", xlabs = "Coral presence")}
     	if(v1[i] == "pen"){plot(cpue.gam, select = i, all.terms = TRUE, ylabs = "Variable effect", xlabs = "Pennatulacean presence")}}
     dev.off()

     ####FIGURE 5 - CPUE GAM DIAGNOSTICS PLOTS
     #Plots for training data
     png(filename = paste0(results.path, "/Figures/CPUEGAMdiagnostics.png"), width = 8.5, height = 11, res = 300, units = "in")
     par(mfcol = c(3,2), family = "sans", mar = c(4,4,3,1))
     qqnorm((cpue.obs.pred$x - cpue.obs.pred$y), main = "Training data")
     qqline((cpue.obs.pred$x - cpue.obs.pred$y))
     hist((cpue.obs.pred$x - cpue.obs.pred$y), xlab = "Residuals", main = "")
     regr <- lm(y~x, data = cpue.obs.pred)
     rsqr <- summary(lm(y~x, data = cpue.obs.pred))$r.squared
     adj.rsqr <- summary(lm(y~x, data = cpue.obs.pred))$adj.r.squared
     pred.max <- max(cpue.obs.pred$y)
     obs.max <- max(cpue.obs.pred$x)

     if(pred.max > obs.max){
     	plot.max = pred.max + (0.04*pred.max)
     }else{
     	plot.max = obs.max + (0.04*obs.max)
     	}

     par(xaxs = "i", yaxs = "i")
     plot(cpue.obs.pred$x, cpue.obs.pred$y, ylim = c(0, plot.max), xlim = c(0, plot.max), ylab = "Predicted", xlab = "Observed",
     	main = "", pch = 20)
     abline(coef = c(0, 1), lty = 2)
     lines(cpue.obs.pred$x, regr$fitted.values)
     text(1, plot.max-1, paste("R-squared = ", round(rsqr,2)), pos = 4)

     #Plots for test data
     qqnorm((t.cpue.obs.pred$x - t.cpue.obs.pred$y), main = "Test data")
     qqline((t.cpue.obs.pred$x - t.cpue.obs.pred$y))
     hist((t.cpue.obs.pred$x - t.cpue.obs.pred$y), xlab = "Residuals", main = "")
     regr <- lm(y~x, data = t.cpue.obs.pred)
     rsqr <- summary(lm(y~x, data = t.cpue.obs.pred))$r.squared
     adj.rsqr <- summary(lm(y~x, data = t.cpue.obs.pred))$adj.r.squared
     pred.max <- max(t.cpue.obs.pred$y)
     obs.max <- max(t.cpue.obs.pred$x)

     if(pred.max > obs.max){
     	plot.max = pred.max + (0.04*pred.max)
     }else{
     	plot.max = obs.max + (0.04*obs.max)
     	}

     par(xaxs = "i", yaxs = "i")
     plot(t.cpue.obs.pred$x, t.cpue.obs.pred$y, ylim = c(0,plot.max), xlim = c(0,plot.max), ylab = "Predicted", xlab = "Observed",
     	main = "", pch = 20)
     abline(coef = c(0,1), lty = 2)
     lines(t.cpue.obs.pred$x, regr$fitted.values)
     text(1, plot.max-1, paste("R-squared = ", round(rsqr,2)), pos = 4)
     dev.off()

     ######FIGURE 6 - PREDICTED ABUNDANCE MAP ##################################################
     png(filename = paste0(results.path, "/Figures/CPUEGAMmap.png"), width = 7.1, height = 5, res = 300, units = "in")
     par(mfrow = c(1,1), mar = c(5,4,1,1), family = "sans")
     plot(predict.CPUE.raster, main = "", xaxt = "n", yaxt = "n", box = F,col = jet.colors(255), ext = AI.ext.pol.proj,
          legend.shrink = 0.5, axis.args = list(cex.axis = 0.65), legend.args = list(text = "Abundance", cex = 0.65,
          cex.lab = 0.65, side = 1, line = 2), horiz = TRUE, ylab = "Latitude", xlab = "Longitude",
          zlim = c(0, maxValue(predict.CPUE.raster)))
     plot(ak.coast, col = "black", add = TRUE)
     plot(wrld_grd_proj2, lty = 3, col = "lightgrey", add = T)
     axis(1, at = AI.xaxis.ticks, labels = AI.xaxis)
     axis(2, at = AI.yaxis.ticks, labels = AI.yaxis)
     dev.off()

     ######FIGURE 7 - EFH MAP ##################################################
     png(filename = paste0(results.path, "/Figures/EFHmap.png"), width = 7.1, height = 5, res = 300, units = "in")
     par(mfrow = c(1,1), mar = c(5,4,1,1), family = "sans")
     plot(EFH.raster, main = "", xaxt = "n", yaxt = "n", box = F, col = colorRampPalette(c("grey","coral3"))(4),
          ext = AI.ext.pol.proj, legend = FALSE, ylab = "Latitude", xlab = "Longitude", horiz = TRUE,
          zlim = c(2, maxValue(EFH.raster)))
     plot(ak.coast, col = "black", add = TRUE)
     plot(wrld_grd_proj2, lty = 3, col = "lightgrey", add = T)
     axis(1, at = AI.xaxis.ticks, labels = AI.xaxis)
     axis(2, at = AI.yaxis.ticks, labels = AI.yaxis)
     legVals <- c("95%","75%","50%","25%")
     legend("bottomleft", legend = legVals, pch = 15, col = colorRampPalette(c("grey","coral3"))(4), bty = "n", pt.cex = 2,
          title = "Percentiles", cex = 1)
     depth.contour <- contour(raster.stack[["bdepth"]], levels = c(100,300,500), col = "black", lwd = 0.05,
          drawlabels = FALSE, add = TRUE)
     dev.off()


     ##########################WRITE RASTERS TO ArcGIS FORMAT GeoTIFF's############################################
     dir.create(paste0(results.path, "/GeoTiffs"))
     raster::writeRaster(predict.CPUE.raster, filename = paste0(home.dir, "/GeoTiffs/GeoTiffs_AI/", species_name[j], "_CPUE_map.tif"),
          overwrite = TRUE, format = "GTiff", options = c("PROFILE=GeoTIFF", "TFW=YES"), NAflag = -999)
     raster::writeRaster(predict.pa.raster, filename = paste0(home.dir, "/GeoTiffs/GeoTiffs_AI/", species_name[j], "_CPUE_map.tif"),
          overwrite = TRUE, format = "GTiff", options = c("PROFILE=GeoTIFF", "TFW=YES"), NAflag = -999)
     raster::writeRaster(EFH.raster, filename = paste0(home.dir, "/GeoTiffs/GeoTiffs_AI/", species_name[j], "_CPUE_map.tif"),
          overwrite = TRUE, format = "GTiff", options = c("PROFILE=GeoTIFF", "TFW=YES"), NAflag = -999)

     ########TABLE 1 - SUMMARY TABLE FOR PRESENCE-ABSENCE GAM#######################
     n1 <- summary(pa.gam)$m
     n2 <- length(summary(pa.gam)$p.t)
     n2 <- ifelse(n2>0,n2-1,0)+n1
     v1 <- all.vars(formula(pa.gam))
     v1 <- v1[-1]

     if(v1[1] == "lon"){
          v1[1] <- "lonlat"
     	v1 <- v1[-2]
     	}

     table1 <- array("", dim = c(length(v1)+2,9))
     table1[1,] <- c(" ", " ", " ", " ", "Training data", " ", " ", "Test data", " ")
     table1[2,] <- c("Model term", "EDF", "p-value", "Deviance explained", "AUC", "Optimum threshold",
          "Percent correctly classified", "AUC", "Percent correctly classified")
     table1[3,4] <- round(summary(pa.gam)$dev.expl, 3)
     table1[3,5] <- round(PresenceAbsence::auc(auc.dat)$AUC, 2)
     table1[3,6] <- thresh
     table1[3,7] <- round(PresenceAbsence::pcc(PresenceAbsence::cmx(auc.dat, threshold = thresh))$PCC, 2)
     table1[3,8] <- round(PresenceAbsence::auc(t.auc.dat)$AUC, 2)
     table1[3,9] <- round(PresenceAbsence::pcc(PresenceAbsence::cmx(t.auc.dat, threshold = thresh))$PCC, 2)

     table1a <- v1
     table1a[table1a == "lonlat"] <- "Longitude*latitude"
     table1a[table1a == "slope"] <- "Slope"
     table1a[table1a == "pen"] <- "Pennatulacean present"
     table1a[table1a == "coral"] <- "Coral present"
     table1a[table1a == "sponge"] <- "Sponge present"
     table1a[table1a == "color"] <- "Ocean color"
     table1a[table1a == "speed"] <- "Current speed"
     table1a[table1a == "bdepth"] <- "Depth"
     table1a[table1a == "tmax"] <- "Tidal current"
     table1a[table1a == "btemp"] <- "Temperature"
     table1b <- c(summary(pa.gam)$edf, summary(pa.gam)$pTerms.df)
     table1c <- c(summary(pa.gam)$s.pv, summary(pa.gam)$pTerms.pv)
     table1[3:(length(v1) + 2), 1] <- table1a[order(table1c)]
     table1[3:(length(v1) + 2), 2] <- signif(table1b[order(table1c)], digits = 2)
     table1[3:(length(v1) + 2), 3] <- signif(table1c[order(table1c)], digits = 3)
     pa.table <- xtable::xtable(table1)
     xtable::print.xtable(pa.table,type = "html", file = paste0(results.path, "/Figures/pagamtable.html"),
          include.rownames = getOption("xtable.include.rownames", FALSE), html.table.attributes = 2,
          include.colnames = getOption("xtable.include.colnames", FALSE), hline.after = getOption("xtable.hline.after",
          c(-1, 1, nrow(table1))))

     ########TABLE 2 - SUMMARY TABLE FOR CPUE GAM#######################
     n1 <- summary(cpue.gam)$m
     n2 <- length(summary(cpue.gam)$p.t)
     n2 <- ifelse(n2>0,n2-1,0)+n1
     v1 <- all.vars(formula(cpue.gam))
     v1 <- v1[-1]

     if(v1[1] == "lon"){
          v1[1] <- "lonlat"
     	v1 <- v1[-2]
     	}

     table1 <- array("", dim = c(length(v1)+2, 6))
     table1[1,] <- c(" ", " ", " ", " ", "Training data", "Test data")
     table1[2,] <- c("Model term", "EDF", "p-value", "Deviance explained", "rsq", "rsq")
     table1[3,4] <- round(summary(cpue.gam)$dev.expl, 3)
     table1[3,5] <- rsqr <- round(summary(lm(y~x, data = cpue.obs.pred))$r.squared, 2)
     table1[3,6] <- rsqr <- round(summary(lm(y~x, data = t.cpue.obs.pred))$r.squared, 2)

     table1a <- v1
     table1a[table1a == "lonlat"] <- "Longitude*latitude"
     table1a[table1a == "slope"] <- "Slope"
     table1a[table1a == "pen"] <- "Pennatulacean present"
     table1a[table1a == "coral"] <- "Coral present"
     table1a[table1a == "sponge"] <- "Sponge present"
     table1a[table1a == "color"] <- "Ocean color"
     table1a[table1a == "speed"] <- "Current speed"
     table1a[table1a == "bdepth"] <- "Depth"
     table1a[table1a == "tmax"] <- "Tidal current"
     table1a[table1a == "btemp"] <- "Temperature"
     table1b <- c(summary(cpue.gam)$edf, summary(cpue.gam)$pTerms.df)
     table1c <- c(summary(cpue.gam)$s.pv, summary(cpue.gam)$pTerms.pv)
     table1[3:(length(v1) + 2), 1] <- table1a[order(table1c)]
     table1[3:(length(v1) + 2), 2] <- signif(table1b[order(table1c)], digits = 2)
     table1[3:(length(v1) + 2), 3] <- signif(table1c[order(table1c)], digits = 3)
     cpue.table <- xtable::xtable(table1)
     xtable::print.xtable(cpue.table, type = "html", file = paste0(results.path, "/Figures/cpuegamtable.html"),
          include.rownames = getOption("xtable.include.rownames", FALSE), html.table.attributes = 2,
          include.colnames = getOption("xtable.include.colnames", FALSE), hline.after = getOption("xtable.hline.after",
          c(-1, 1, nrow(table1))))
     print(paste(species[j], "hurdle model complete"))
     }


     ####################################################################################################################################
     ####################################################################################################################################
     ##################################################MAXENT MODEL######################################################################
     ####################################################################################################################################
     if(models[j] == "maxent"){

     training.pos <- subset(training.dat, training.dat[species[j]] > 0)
     training.pos <- cbind(training.pos$lon, training.pos$lat)

     test.pos <- subset(test.dat, test.dat[species[j]] > 0)
     test.pos <- cbind(test.pos$lon, test.pos$lat)

     maxent.stack <- raster::stack(bathy, slope, btemp, color, bcurrent, tmax)
     names(maxent.stack) <- c("Depth", "Slope", "Temperature", "Color", "Current_speed", "Tidal_current")

     maxent.model <- dismo::maxent(maxent.stack, training.pos, args = c("-P","-J"), path = results.path)

     habitat.prediction1 <- predict(maxent.model, maxent.stack, overwrite = TRUE, progress = "text")
     habitat.prediction <- raster::mask(habitat.prediction1, ak.coast, inverse = TRUE, overwrite = TRUE, progress = "text",
          filename = paste0(results.path, "/SuitableHabitat"))

     ######STEP 5 - TEST THE PREDICTIONS AGAINST THE TRAINING DATA############################
     #Extract the predictions at the training points
     train.predicted <- raster::extract(habitat.prediction, training.pos)

     #Create a vector of 1's (presence points for training data)
     train.observed <- rep(1, length(train.predicted))

     #Choose a random vector of absence points
     train.background <- dismo::randomPoints(habitat.prediction, length(training.pos[,1]), training.pos, tryf = 50)

     #Extract the predictions at these points
     train.background.predicted <- raster::extract(habitat.prediction, train.background)

     #Create a vector of 0's (absence points for training data)
     train.background.observed <- rep(0, length(train.background.predicted))

     #Bind the observations and predictions together and create a dataframe
     train.predicted <- c(train.predicted, train.background.predicted)
     train.observed <- c(train.observed, train.background.observed)
     train.auc_data <- data.frame(cbind(seq(1, length(train.predicted), 1), train.observed, train.predicted))
     train.auc_data <- subset(train.auc_data, train.auc_data$train.predicted >= 0)

     #Calculate the AUC
     PresenceAbsence::auc(train.auc_data, na.rm = TRUE)

     #Estimate the thresholds and calculate diagnostics
     PresenceAbsence::optimal.thresholds(train.auc_data, opt.methods = c(seq(1:9)))
     train.threshold <- PresenceAbsence::optimal.thresholds(train.auc_data, opt.methods = 2)
     train.threshold <- train.threshold[,2]
     PresenceAbsence::cmx(train.auc_data, threshold = train.threshold)

     #######STEP 6 - TEST THE PREDICTIONS AGAINST THE TEST DATA FOR FALL############################
     #Extract the predictions at the testing points
     test.predicted <- raster::extract(habitat.prediction, test.pos)

     #Create a vector of 1's (presence points for testing data)
     test.observed <- rep(1, length(test.predicted))

     #Choose a random vector of absence points
     test.background <- dismo::randomPoints(habitat.prediction, length(test.pos[, 1]), test.pos, tryf = 50)

     #Extract the predictions at these points
     test.background.predicted <- raster::extract(habitat.prediction, test.background)

     #Create a vector of 0's (absence points for testing data)
     test.background.observed <- rep(0,length(test.background.predicted))

     #Bind the observations and predictions together and create a dataframe
     test.predicted <- c(test.predicted, test.background.predicted)
     test.observed <- c(test.observed, test.background.observed)
     test.auc_data <- data.frame(cbind(seq(1, length(test.predicted), 1), test.observed, test.predicted))
     test.auc_data<-subset(test.auc_data,test.auc_data$test.predicted>=0)

     #Calculate the AUC
     PresenceAbsence::auc(test.auc_data, na.rm = TRUE)

     PresenceAbsence::cmx(test.auc_data, threshold = train.threshold)

     ####FIGURE 2 - MAXENT EFFECTS PLOT################################
     dir.create(paste0(results.path, "/Figures"))
     png(filename = paste0(results.path, "/Figures/VariableEffectplots.png"), width = 8.5, height = 11, res = 300, units = "in")

     par(mfrow = c(2,3), mar = c(4, 4, 2, 0.01))
     dismo::response(maxent.model, var = "Depth", col = 1, rug = FALSE)
     dismo::response(maxent.model, var = "Tidal_current", col = 1, rug = FALSE)
     dismo::response(maxent.model, var = "Slope", col = 1, rug = FALSE)
     dismo::response(maxent.model, var = "Color", col = 1, rug = FALSE)
     dismo::response(maxent.model, var = "Temperature", col = 1, rug = FALSE)
     dismo::response(maxent.model, var = "Current_speed", col = 1, rug = FALSE)
     dev.off()

     ####FIGURE 3 - MAXENT DIAGNOSTICS PLOTS
     #Plots for training data
     png(filename = paste0(results.path, "/Figures/maxentdiagnostics.png"), width = 8.5, height = 11, res = 300, units = "in")
     par(mfcol = c(3,2), family = "sans", mar = c(4, 4, 1, 0.01))
     PresenceAbsence::auc.roc.plot(train.auc_data, opt.methods = 2, main = "", add.legend = F, xlab = "Specificity",
          ylab = "Sensitivity", add.opt.legend = F)
     text(0, 0.95, "Training data", pos = 4, cex = 1.25)
     PresenceAbsence::calibration.plot(train.auc_data, N.bins = 10, xlab = "Predicted occurence",
          ylab = "Proportion of observed occurence", main = "")
     PresenceAbsence::presence.absence.hist(train.auc_data, truncate.tallest = TRUE, main = "", ylab = "Number of observations",
          xlab = "Predicted probability")

     #Plots for test data
     PresenceAbsence::auc.roc.plot(test.auc_data, main = "", add.legend = F, xlab = "Specificity", ylab = "Sensitivity",
          add.opt.legend = F)
     text(0, 0.95, "Test data", pos = 4, cex = 1.25)
     PresenceAbsence::calibration.plot(test.auc_data, N.bins = 10, xlab = "Predicted occurence",
          ylab = "Proportion of observed occurence", main = "")
     PresenceAbsence::presence.absence.hist(test.auc_data, truncate.tallest = TRUE, main = "", ylab = "Number of observations",
          xlab = "Predicted probability")
     dev.off()

     ######FIGURE 4 - PREDICTED PRESENCE MAP ##################################################

     png(filename = paste0(results.path, "/Figures/SuitableHabitatmap.png"), width = 7.1, height = 5, res = 300, units = "in")
     par(mfrow = c(1,1), mar = c(5,4,1,1), family = "sans")
     plot(habitat.prediction, main = "", xaxt = "n", yaxt = "n", box = F, col = jet.colors(255), ext = AI.ext.pol.proj,
          legend.shrink = 0.5, axis.args = list(cex.axis = 0.65), legend.args = list(text = "Probability of suitable habitat",
          cex = 0.65, cex.lab = 0.65, side = 1, line = 2), horiz = TRUE, ylab = "Latitude", xlab = "Longitude", zlim = c(0,1))
     plot(ak.coast, col = "black", add = TRUE)
     plot(wrld_grd_proj2, lty = 3, col = "lightgrey", add = T)
     axis(1, at = AI.xaxis.ticks, labels = AI.xaxis)
     axis(2, at = AI.yaxis.ticks, labels = AI.yaxis)
     dev.off()


     ######FIGURE 5 - EFH MAP ##################################################

     sample1 <- raster::sampleRandom(habitat.prediction, 300000, na.rm = TRUE)
     sample1[sample1 <= 0] <- NA
     breaks <- quantile(sample1, probs =c(0,0.05,0.25,0.5,0.75,1), na.rm = TRUE, names = FALSE)

     EFH.raster <- raster::cut(habitat.prediction, breaks = breaks, overwrite = TRUE, filename = paste0(results.path, "/EFHmap"))
     save.image(paste0(results.path, "/", species_name[j], ".RData"))
     png(filename = paste0(results.path, "/Figures/EFHmap.png"), width = 7.1, height = 5, res = 300, units = "in")
     par(mfrow = c(1,1), mar = c(5,4,1,1), family = "sans")
     plot(EFH.raster, main = "",xaxt="n",yaxt="n", box=F,col = colorRampPalette(c("grey","coral3"))(4),ext=AI.ext.pol.proj,legend=FALSE,ylab="Latitude",xlab="Longitude",horiz=TRUE, zlim = c(2,maxValue(EFH.raster)))
     plot(ak.coast, col = "black", add = TRUE)
     plot(wrld_grd_proj2, lty = 3, col = "lightgrey", add = T)
     axis(1, at = AI.xaxis.ticks, labels = AI.xaxis)
     axis(2, at = AI.yaxis.ticks, labels = AI.yaxis)
     legVals <- c("95%","75%","50%","25%")
     legend("bottomleft", legend = legVals, pch = 15, col = colorRampPalette(c("grey","coral3"))(4), bty = "n", pt.cex = 2,
          title = "Percentiles", cex = 1)
     depth.contour <- raster::contour(maxent.stack[["Depth"]], levels = c(100,300,500), col = "black", lwd = 0.05,
          drawlabels = FALSE, add = TRUE)
     dev.off()

     #######FIGURE 1 - POINTS ON DEPTH MAP#################################################
     png(filename = paste0(results.path, "/Figures/Mappts.png"), width = 7.1, height = 5, res = 300, units = "in")
     par(mfrow = c(1,1), mar = c(5,4,1,1), family = "sans")
     plot(maxent.stack[["Depth"]], main = "", xaxt = "n", yaxt = "n", box = F, ext = AI.ext.pol.proj, legend.shrink = 0.5,
          axis.args = list(cex.axis = 0.65), legend.args = list(text = "Depth", cex = 0.65, cex.lab = 0.65, side = 1, line = 2),
          horiz = TRUE, ylab = "Latitude", xlab = "Longitude")
     plot(ak.coast, col = "black", add = TRUE)
     points(training.pos, col = "blue", pch = 20)
     points(test.pos, col = "purple", pch = 20)
     plot(wrld_grd_proj2, lty = 3, col = "lightgrey", add = T)
     axis(1, at = AI.xaxis.ticks, labels = AI.xaxis)
     axis(2, at = AI.yaxis.ticks, labels = AI.yaxis)
     legVals <- c("Training data", "Testing data")
     legend("bottomleft", legend = legVals, pch = 20, col = c("blue","purple"), bty = "n", pt.cex = 2 , title = "", cex = 1)
     dev.off()


     ##########################WRITE RASTERS TO ArcGIS FORMAT GeoTIFF's############################################
     dir.create(paste0(results.path, "/GeoTiffs"))
     raster::writeRaster(habitat.prediction, filename = paste0(home.dir, "/GeoTiffs/GeoTiffs_AI/",species_name[j],
          "_SuitableHabitat_map.tif"), overwrite = TRUE, format = "GTiff", options = c("PROFILE=GeoTIFF", "TFW=YES"),
          NAflag = -999)
     raster::writeRaster(EFH.raster, filename = paste0(home.dir, "/GeoTiffs/GeoTiffs_AI/",species_name[j],"_EFH_map.tif"),
          overwrite = TRUE, format = "GTiff", options = c("PROFILE=GeoTIFF", "TFW=YES"), NAflag = -999)

     ########TABLE 1 - SUMMARY TABLE FOR PRESENCE-ABSENCE GAM#######################
     table1 <- array("", dim = c(12,9))
     table1[,1] <- c(" ", " ", " ", "Training data", " ", " ", " ", " ", "Test data", " ", " ", " ")
     table1[,2] <- c("Model term", "Relative importance", "Permutation importance", "AUC", "Correlation", "Optimum threshold",
          "Percent correctly classified", "Kappa", "AUC", "Correlation", "Percent correctly classified", "Kappa")
     table1[4,3] <- round(PresenceAbsence::auc(train.auc_data)$AUC, 2)
     table1[5,3] <- round(cor.test(train.auc_data[,2], train.auc_data[, 3])$estimate, 3)
     table1[6,3] <- train.threshold
     table1[7,3] <- round(PresenceAbsence::pcc(PresenceAbsence::cmx(train.auc_data, threshold = train.threshold))$PCC, 2)
     table1[8,3] <- round(PresenceAbsence::Kappa(PresenceAbsence::cmx(train.auc_data, threshold = train.threshold))$Kappa, 2)
     table1[9,3] <- round(PresenceAbsence::pcc(PresenceAbsence::cmx(test.auc_data, threshold = train.threshold))$PCC, 2)
     table1[10,3] <- round(cor.test(test.auc_data[, 2], test.auc_data[, 3])$estimate, 3)
     table1[11,3] <- round(PresenceAbsence::pcc(PresenceAbsence::cmx(test.auc_data, threshold = train.threshold))$PCC, 2)
     table1[12,3] <- round(PresenceAbsence::Kappa(PresenceAbsence::cmx(test.auc_data, threshold = train.threshold))$Kappa, 2)

     maxentResults <- read.csv(paste0(results.path, "/maxentResults.csv"), header = TRUE)
     maxentResults <- as.vector(maxentResults[8:21])
     col1 <- unlist(strsplit(colnames(maxentResults), split = ".", fixed = TRUE))[1]
     col2 <- unlist(strsplit(colnames(maxentResults), split = ".", fixed = TRUE))[3]
     col3 <- unlist(strsplit(colnames(maxentResults), split = ".", fixed = TRUE))[5]
     col4 <- unlist(strsplit(colnames(maxentResults), split = ".", fixed = TRUE))[7]
     col5 <- unlist(strsplit(colnames(maxentResults), split = ".", fixed = TRUE))[9]
     col6 <- unlist(strsplit(colnames(maxentResults), split = ".", fixed = TRUE))[11]
     col7 <- unlist(strsplit(colnames(maxentResults), split = ".", fixed = TRUE))[13]
     names1 <- c(col1,col2,col3,col4,col5,col6,col7)

     cont.order <- maxentResults[1, (1:7)]
     perm.order <- maxentResults[1, (8:14)]
     perm.order <- perm.order[order(cont.order[1, ])]
     names1 <- names1[order(cont.order[1, ])]
     cont.order <- cont.order[order(cont.order[1, ])]

     table1[1, (3:9)] <- rev(names1)
     table1[2, (3:9)] <- round(rev(unlist(cont.order)), 1)
     table1[3, (3:9)] <- round(rev(unlist(perm.order)), 1)

     pa.table <- xtable::xtable(table1)
     xtable::print.xtable(pa.table, type = "html", file = paste0(results.path, "/Figures/MaxentTable.html"),
          include.rownames = getOption("xtable.include.rownames", FALSE), html.table.attributes = 2,
          include.colnames = getOption("xtable.include.colnames", FALSE), hline.after = getOption("xtable.hline.after",
          c(-1, 1, nrow(table1))))
     print(paste(species[j], "maxent model complete"))
     }

     ###################################################################################
     #################################### Publication Figure Plates ####################
     ###################################################################################
     print(paste("Making publication graphics for", species[j], models[j], sep = " "))

     if(models[j] == "gam"){
          myplot <- list()
          myplot[]

          # presence-absence map (includes training and test data)
          print("presence-absence map (GAM)")
          dev.new(width = 7.1, height = 5, res = 300, units = "in")
          par(mfrow = c(1,1), mar = c(5,4,1,1), family = "sans")
          plot(raster.stack[["bdepth"]], main = "", xaxt = "n", yaxt = "n", box = F, ext = AI.ext.pol.proj, zlim = c(0,500),
               legend.shrink = 0.5, axis.args = list(cex.axis = 0.65), legend.args = list(text = "Depth", cex = 0.65,
               cex.lab = 0.65, side = 1, line = 2), horiz = TRUE, ylab = "Latitude", xlab = "Longitude")
          plot(ak.coast, col = "black", add = TRUE)
          training.pos <- subset(training.dat, training.dat[species[j]] > 0)
          test.pos <- subset(test.dat, test.dat[species[j]] > 0)
          present <- rbind(cbind(training.pos$lon, training.pos$lat), cbind(test.pos$lon, test.pos$lat))
          training.pos <- subset(training.dat, training.dat[species[j]] == 0)
          test.pos <- subset(test.dat, test.dat[species[j]] == 0)
          absent <- rbind(cbind(training.pos$lon, training.pos$lat), cbind(test.pos$lon, test.pos$lat))
          points(absent, col = "black", pch = 4, cex = 0.5)
          points(present, col = "blue", pch = 20, cex = 1.5)
          plot(wrld_grd_proj2, lty = 3, col = "lightgrey", add = T)
          axis(1, at = AI.xaxis.ticks, labels = AI.xaxis)
          axis(2, at = AI.yaxis.ticks, labels = AI.yaxis)
          legVals <- c("Present","Absent")
          legend("bottomleft", legend = legVals, pch = c(20,4), col = c("blue","black"), bty = "n", pt.cex = c(2,1),
                 title = paste0("n = ", (length(training.pos[, 1]) + length(test.pos[, 1]))), cex = 1)
          text(-1100000, 980000, species_name[j], font = 2, cex = 1)

          gridGraphics::grid.echo()
          a <- grid::grid.grab()
          myplot[["a"]] <- a
          dev.off()

          # plotting model effects
          print("GAM model effects")
          dev.new(width = 7.1, height = 5, res = 300, units = "in")
          n1 <- summary(cpue.gam)$m
          n2 <- length(summary(cpue.gam)$p.t)
          n2 <- ifelse(n2>0,n2-1,0)+n1
          v1 <- all.vars(formula(cpue.gam))
          v1 <- v1[-1]

          if(v1[1] == "lon"){
               v1[1] <- "lonlat"
          	v1 <- v1[-2]
          	}

          par(mfrow = c(ceiling(n2/3), 3), mar = c(4,4,2,0.01))
          for(i in 1:n2){
          	if(v1[i] == "lonlat"){plot(cpue.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
          	    xlab = "Longitude (northings)", ylab = "Latitude (eastings)")}
          	if(v1[i] == "slope"){plot(cpue.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
          	    ylab = "Variable effect", xlab = "Slope (%)")}
          	if(v1[i] == "tmax"){plot(cpue.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
          	    ylab = "Variable effect", xlab = "Tidal current speed (cm/s)")}
          	if(v1[i] == "color"){plot(cpue.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
          	    ylab = "Variable effect", xlab = "Ocean color")}
          	if(v1[i] == "speed"){plot(cpue.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
          	    ylab = "Variable effect", xlab = "Current speed (m/s)")}
          	if(v1[i] == "btemp"){plot(cpue.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
          	    ylab = "Variable effect", xlab = "Temperature (C)")}
          	if(v1[i] == "bdepth"){plot(cpue.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
          	    ylab = "Variable effect", xlab = "Depth (m)")}
          	if(v1[i] == "sponge"){plot(cpue.gam, select = i, all.terms = TRUE, ylabs = "Variable effect",
          	    xlabs = "Sponge presence")}
          	if(v1[i] == "coral"){plot(cpue.gam, select = i, all.terms = TRUE, ylabs = "Variable effect",
          	    xlabs = "Coral presence")}
          	if(v1[i] == "pen"){plot(cpue.gam, select = i, all.terms = TRUE, ylabs = "Variable effect",
          	    xlabs = "Pennatulacean presence")}
               }
          gridGraphics::grid.echo()
          b <- grid::grid.grab()
          myplot[["b"]] <- b
          dev.off()

          # model prediction map
          print("GAM model prediction")
          dev.new(width = 7.1, height = 5, res = 300, units = "in")
          par(mfrow = c(1,1), mar = c(5,4,1,1), family = "sans")
          plot(predict.CPUE.raster, main = "", xaxt = "n", yaxt = "n", box = F, col = plasma(255), ext = AI.ext.pol.proj,
               legend.shrink = 0.5, axis.args = list(cex.axis = 0.65), legend.args = list(text = "Abundance", cex = 0.65,
               cex.lab = 0.65, side = 1, line = 2), horiz = TRUE, ylab = "Latitude", xlab = "Longitude",
               zlim = c(0, maxValue(predict.CPUE.raster)))
          plot(ak.coast, col = "black", add = TRUE)
          plot(wrld_grd_proj2, lty = 3, col = "lightgrey", add = T)
          axis(1, at = AI.xaxis.ticks, labels = AI.xaxis)
          axis(2, at = AI.yaxis.ticks, labels = AI.yaxis)

          gridGraphics::grid.echo()
          c <- grid::grid.grab()
          myplot[["c"]] <- c
          dev.off()

          png(filename = paste0(home.dir, "/Publication_Figures/AI/", species_name[j],"_ModelFig.png"), width = 8.1,
              height = 16, res = 300, units = "in")
          gridExtra::grid.arrange(grobs = myplot, ncol = 1, heights = unit(c(5,5,5), c("in")))
          dev.off()
          }

     if(models[j] == "hgam"){
          myplot <- list()
          myplot[]

          # hgam presence-absence map
          print("Presence-Absence map for hGAM")
          dev.new(width = 7.1, height = 5, res = 300, units = "in")
          par(mfrow = c(1,1), mar = c(5,4,1,1), family = "sans")
          plot(raster.stack[["bdepth"]], main = "", xaxt = "n", yaxt = "n", box = F, ext = AI.ext.pol.proj, zlim = c(0,500),
               legend.shrink = 0.5, axis.args = list(cex.axis = 0.65), legend.args = list(text = "Depth", cex = 0.65,
               cex.lab = 0.65, side = 1, line = 2), horiz = TRUE, ylab = "Latitude", xlab = "Longitude")
          plot(ak.coast, col = "black", add = TRUE)
          training.pos <- subset(training.dat, training.dat[species[j]] > 0)
          test.pos <- subset(test.dat, test.dat[species[j]] > 0)
          present <- rbind(cbind(training.pos$lon, training.pos$lat), cbind(test.pos$lon, test.pos$lat))
          training.pos <- subset(training.dat, training.dat[species[j]] == 0)
          test.pos <- subset(test.dat, test.dat[species[j]] == 0)
          absent <- rbind(cbind(training.pos$lon, training.pos$lat), cbind(test.pos$lon, test.pos$lat))
          points(absent, col = "black", pch = 4, cex = 0.5)
          points(present, col = "blue", pch = 20, cex = 1.5)
          plot(wrld_grd_proj2, lty = 3, col = "lightgrey", add = T)
          axis(1, at = AI.xaxis.ticks, labels = AI.xaxis)
          axis(2, at = AI.yaxis.ticks, labels = AI.yaxis)
          legVals <- c("Present","Absent")
          legend("bottomleft", legend = legVals, pch = c(20,4), col = c("blue","black"), bty = "n", pt.cex = c(2,1),
               title = paste0("n = ", (length(training.pos[, 1]) + length(test.pos[, 1]))), cex = 1)
          text(-1100000, 980000, species_name[j], font = 2, cex = 1)

          gridGraphics::grid.echo()
          a <- grid::grid.grab()
          myplot[["a"]] <- a
          dev.off()

          # presence-absence GAM summary
          print("GAM effects for presence-absence step")
          dev.new(width = 7.1, height = 5, res = 300, units = "in")
          n1 <- summary(pa.gam)$m
          n2 <- length(summary(pa.gam)$p.t)
          n2 <- ifelse(n2>0,n2-1,0)+n1
          v1 <- all.vars(formula(pa.gam))
          v1 <- v1[-1]

          if(v1[1] == "lon"){
               v1[1] <- "lonlat"
          	v1 <- v1[-2]
          	}

          par(mfrow = c(ceiling(n2/3),3), mar = c(4,4,2,0.01))
          for(i in 1:n2){
          	if(v1[i] == "lonlat"){plot(pa.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
          	    xlab = "Longitude (northings)", ylab = "Latitude (eastings)") }
          	if(v1[i] == "slope"){plot(pa.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
          	    ylab = "Variable effect", xlab = "Slope (%)")  }
          	if(v1[i] == "tmax"){plot(pa.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
          	    ylab = "Variable effect", xlab = "Tidal current speed (cm/s)")}
          	if(v1[i] == "color"){plot(pa.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
          	    ylab = "Variable effect", xlab = "Ocean color")  }
          	if(v1[i] == "speed"){plot(pa.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
          	    ylab = "Variable effect", xlab = "Current speed (m/s)")}
          	if(v1[i] == "btemp"){plot(pa.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
          	    ylab = "Variable effect", xlab = "Temperature (C)")}
          	if(v1[i] == "bdepth"){plot(pa.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
          	    ylab = "Variable effect", xlab = "Depth (m)")}
          	if(v1[i] == "sponge"){plot(pa.gam, select = i, all.terms = TRUE,
          	    ylabs = "Variable effect", xlabs = "Sponge presence")}
          	if(v1[i] == "coral"){plot(pa.gam, select = i, all.terms = TRUE,
          	    ylabs = "Variable effect", xlabs = "Coral presence")}
          	if(v1[i] == "pen"){plot(pa.gam, select = i, all.terms = TRUE,
          	    ylabs = "Variable effect", xlabs = "Pennatulacean presence")}}
          gridGraphics::grid.echo()
          b <- grid::grid.grab()
          myplot[["b"]] <- b
          dev.off()

          # prediction map for presence-absence GAM
          print("Prediction map for presence-absence step in hGAM")
          dev.new(width = 7.1, height = 5, res = 300, units = "in")
          par(mfrow = c(1,1), mar = c(5,4,1,1), family = "sans")
          plot(predict.pa.raster, main = "", xaxt = "n", yaxt = "n", box = F, col = plasma(255), ext = AI.ext.pol.proj,
               legend.shrink = 0.5, axis.args = list(cex.axis = 0.65), legend.args = list(text = "Probability of presence",
               cex = 0.65, cex.lab = 0.65, side = 1, line = 2), horiz = TRUE, ylab = "Latitude", xlab = "Longitude", zlim = c(0,1))
          plot(ak.coast, col = "black", add = TRUE)
          plot(wrld_grd_proj2, lty = 3, col = "lightgrey", add = T)
          axis(1, at = AI.xaxis.ticks, labels = AI.xaxis)
          axis(2, at = AI.yaxis.ticks, labels = AI.yaxis)
          grdiGraphics::grid.echo()
          c <- grid::grid.grab()
          myplot[["c"]] <- c
          dev.off()

          # model effects for CPUE step in hGAM
          print("CPUE model summary for abundance-where-present step in hGAM")
          dev.new(width = 7.1, height = 5, res = 300, units = "in")
          n1 <- summary(cpue.gam)$m
          n2 <- length(summary(cpue.gam)$p.t)
          n2 <- ifelse(n2>0,n2-1,0)+n1
          v1 <- all.vars(formula(cpue.gam))
          v1 <- v1[-1]

          if(v1[1] == "lon"){
               v1[1] <- "lonlat"
          	v1 <- v1[-2]
          	}

          par(mfrow = c(ceiling(n2/3), 3), mar = c(4,4,2,0.01))
          for(i in 1:n2){
          	if(v1[i] == "lonlat"){plot(cpue.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
          	    xlab = "Longitude (northings)", ylab = "Latitude (eastings)") }
          	if(v1[i] == "slope"){plot(cpue.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
          	    ylab = "Variable effect", xlab = "Slope (%)")  }
          	if(v1[i] == "tmax"){plot(cpue.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
          	    ylab = "Variable effect", xlab = "Tidal current speed (cm/s)")}
          	if(v1[i] == "color"){plot(cpue.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
          	    ylab = "Variable effect",xlab="Ocean color")  }
          	if(v1[i] == "speed"){plot(cpue.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
          	    ylab = "Variable effect", xlab = "Current speed (m/s)")}
          	if(v1[i] == "btemp"){plot(cpue.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
          	    ylab = "Variable effect", xlab = "Temperature (C)")}
          	if(v1[i] == "bdepth"){plot(cpue.gam, scale = 0, shade = TRUE, select = i, all.terms = TRUE,
          	    ylab = "Variable effect", xlab = "Depth (m)")}
          	if(v1[i] == "sponge"){plot(cpue.gam, select = i, all.terms = TRUE, ylabs = "Variable effect",
          	    xlabs = "Sponge presence")}
          	if(v1[i] == "coral"){plot(cpue.gam, select = i, all.terms = TRUE, ylabs = "Variable effect",
          	    xlabs = "Coral presence")}
          	if(v1[i] == "pen"){plot(cpue.gam, select = i, all.terms = TRUE, ylabs = "Variable effect",
          	    xlabs = "Pennatulacean presence")}
               }
          gridGraphics::grid.echo()
          d <- grid::grid.grab()
          myplot[["d"]] <- d
          dev.off()

          # CPUE prediction map from abundance-where-present GAM
          print("Prediction of CPUE from abundance-where-present")
          dev.new(width = 7.1, height = 5, res = 300, units = "in")
          par(mfrow = c(1,1), mar = c(5,4,1,1), family = "sans")
          plot(predict.CPUE.raster, main = "", xaxt = "n", yaxt = "n", box = F, col = plasma(255), ext = AI.ext.pol.proj,
               legend.shrink = 0.5, axis.args = list(cex.axis = 0.65), legend.args = list(text = "Abundance", cex = 0.65,
               cex.lab = 0.65, side = 1, line = 2), horiz = TRUE, ylab = "Latitude", xlab = "Longitude")
          plot(ak.coast, col = "black", add = TRUE)
          plot(wrld_grd_proj2, lty = 3, col = "lightgrey", add = T)
          axis(1, at = AI.xaxis.ticks, labels = AI.xaxis)
          axis(2, at = AI.yaxis.ticks, labels = AI.yaxis)
          gridGraphics::grid.echo()
          e <- grid::grid.grab()
          myplot[["e"]] <- e
          dev.off()

          png(filename = paste0(home.dir, "/Publication_Figures/AI/", species_name[j],"_ModelFig.png"),
               width = 16.2, height = 16, res = 300, units = "in")
          gridExtra::grid.arrange(grobs = myplot, ncol = 2, layout_matrix = rbind(c(NA,1,1,NA), c(2,2,4,4), c(3,3,5,5)),
               heights = unit(c(5,5,5), c("in")))
          dev.off()
          }


     if(models[j] == "maxent"){
          myplot <- list()
          myplot[]

          # presence-absence map for maxent
          print("Presence-absence for maxent model")
          dev.new(width = 7.1, height = 5, res = 300, units = "in")
          par(mfrow = c(1,1), mar = c(5,4,1,1), family = "sans")
          plot(maxent.stack[["Depth"]], main = "", xaxt = "n", yaxt = "n", box = F, ext = AI.ext.pol.proj,
               zlim = c(0,500), legend.shrink = 0.5, axis.args = list(cex.axis = 0.65), legend.args = list(text = "Depth",
               cex = 0.65, cex.lab = 0.65, side = 1, line = 2), horiz = TRUE, ylab = "Latitude", xlab = "Longitude")
          plot(ak.coast, col = "black", add = TRUE)
          points(training.pos, col = "blue", pch = 20)
          points(test.pos, col = "purple", pch = 20)
          plot(wrld_grd_proj2, lty = 3, col = "lightgrey", add = T)
          axis(1, at = AI.xaxis.ticks, labels = AI.xaxis)
          axis(2, at = AI.yaxis.ticks, labels = AI.yaxis)
          legVals <- c("Training data","Testing data")
          legend("bottomleft", legend = legVals, pch = 20, col = c("blue","purple"), bty = "n", pt.cex = 2 ,
                 title = paste0("n = ",(length(training.pos[, 1]) + length(test.pos[, 1]))), cex = 1)
          text(-1100000, 980000, species_name[j], font = 2, cex = 1)
          gridGraphics::grid.echo()
          a <- grid::grid.grab()
          myplot[["a"]] <- a
          dev.off()

          # maxent model effects
          print("maxent model effects")
          dev.new(width = 7.1, height = 5, res = 300, units = "in")
          par(mfrow = c(2,3), mar = c(4,2,2,0.01), oma = c(2,4,2,4))
          dismo::response(maxent.model, var = "Depth", col = 1, rug = FALSE)
          dismo::response(maxent.model, var = "Tidal_current", col = 1, rug = FALSE)
          dismo::response(maxent.model, var = "Slope", col = 1, rug = FALSE)
          dismo::response(maxent.model, var = "Color", col = 1, rug = FALSE)
          dismo::response(maxent.model, var = "Temperature", col = 1, rug = FALSE)
          dismo::response(maxent.model, var = "Current_speed", col = 1, rug = FALSE)
          mtext("Predicted value", side = 2, outer = TRUE, line = 1)
          grdiGraphics::grid.echo()
          b <- grid::grid.grab()
          myplot[["b"]] <- b
          dev.off()

          # maxent prediction map for suitable habitat
          print("map of suitable habitat from maxent")
          dev.new(width = 7.1, height = 5, res = 300, units = "in")
          par(mfrow = c(1,1), mar = c(5,4,1,1), family = "sans")
          plot(habitat.prediction, main = "", xaxt = "n", yaxt = "n", box = F, col = plasma(255), ext = AI.ext.pol.proj,
               legend.shrink = 0.5, axis.args = list(cex.axis = 0.65), legend.args = list(text = "Probability of suitable habitat",
               cex = 0.65, cex.lab = 0.65, side = 1, line = 2), horiz = TRUE, ylab = "Latitude", xlab = "Longitude", zlim = c(0,1))
          plot(ak.coast, col = "black", add = TRUE)
          plot(wrld_grd_proj2, lty = 3, col = "lightgrey", add = T)
          axis(1, at = AI.xaxis.ticks, labels = AI.xaxis)
          axis(2, at = AI.yaxis.ticks, labels = AI.yaxis)
          gridGraphics::grid.echo()
          c <- grid::grid.grab()
          myplot[["c"]] <- c
          dev.off()

          png(filename = paste0(home.dir, "/Publication_Figures/AI/", species_name[j], "_ModelFig.png"),
              width = 8.1, height = 16, res = 300, units = "in")
          #dev.new(width=18,height=7,units="in",dpi=300)
          gridExtra::grid.arrange(grobs = myplot, ncol = 1, heights = unit(c(5,5,5), c("in")))
          dev.off()
          }

     removeTmpFiles(h=.5)
     }




########################MAPPING STUFF#####################################
#############MAP ODDS AND ENDS#################################
# AI.xaxis <- c(174,176,178,180,-178,-176,-174,-172,-170,-168,-166)
# AI.yaxis <- c(47,49,51,53)
# AI.yaxis.ticks <- cbind(c(174,172.5,170.75,168.75), AI.yaxis)
# AI.xaxis.ticks <- cbind(AI.xaxis, c(46.9,47.55,48.25,48.85,49,49.5,50.25,50.5,50.75,51.25,51.5))
# AI.yaxis.ticks <- rgdal::project(AI.yaxis.ticks,
#      "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
# AI.xaxis.ticks <- rgdal::project(AI.xaxis.ticks,
#      "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
# AI.yaxis.ticks <- AI.yaxis.ticks[, 2]
# AI.xaxis.ticks <- AI.xaxis.ticks[, 1]
# AI.ext.x <- c(-164.95,-164.95,171,171,-164.95)
# AI.ext.y <- c(54.5,50.5,50.5,54.5,54.5)
# AI.ext.pol <- sp::Polygon(cbind(AI.ext.x, AI.ext.y))
# AI.ext.pol <- sp::Polygons(list(AI.ext.pol), "AI")
# AI.ext.pol <- sp::SpatialPolygons(list(AI.ext.pol), proj4string = CRS("+proj=longlat +datum=WGS84"))
# AI.ext.pol.proj <- sp::spTransform(AI.ext.pol,
#     CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
# wrld_p <- maps::map("world", interior = FALSE, plot = FALSE)
# llCRS <- sp::CRS("+proj=longlat +ellps=WGS84")
# wrld_sp <- maptools::map2SpatialLines(wrld_p, proj4string = llCRS)
# prj_new <- sp::CRS("+proj=moll")
# wrld_proj <- sp::spTransform(wrld_sp, prj_new)
# wrld_grd <- sp::gridlines(wrld_sp, easts = c(-178, seq(-176,178, 2), 180), norths = seq(-75, 75, 2), ndiscr = 100)
# wrld_grd_proj2 <- sp::spTransform(wrld_grd,
#      CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

#######STEP 7 - PLOT THE RESULTS ##########################################################
# jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
#                    "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))


