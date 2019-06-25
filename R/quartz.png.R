
# %M%S
quartz.png <- function(file, width=600, dir) {
	if (missing(file)) {
	    file <- paste0(format(Sys.time(), "%Y%m%d_%H"), ".png")
	} else {
		if (!grepl("\\.png$", file)) {
			file <- paste0(file, ".png")
		}
	}
    if (missing(dir)) {
    	dir <- getwd()
    }
    filepath <- file.path(normalizePath(dir), file)
    fin <- par()$fin
    dpi <- width/fin[1]
    dim <- floor(fin*dpi)
	quartz.save(filepath, dpi=dpi)
	message(paste0(
	  "File:\n ",
	  file, "\n ",
	  dim[1], "x", dim[2], " pixels"))
}


# set.seed(1)
# i_h100 <- round(runif(100, 2, 30), 2)
# i_cd <- rexp(100, 1/i_h100)
# mydata <- data.frame(i_cd, i_h100)

# mydata$i_h100_2m <- cut(mydata$i_h100, seq(2, 30, by=2))

# i_cd_2m <- aggregate(i_cd ~ i_h100_2m, mydata, mean)

# set_mar(x=2.5)
# plot.default(i_cd_2m, xaxt="n", main="Groupwise means", xlab="", cex.main=1)
# axis(1, i_cd_2m[,1], as.character(i_cd_2m[,1]), cex.axis=0.6, las=2)

# quartz.png()
# quartz.png("test", 550)
