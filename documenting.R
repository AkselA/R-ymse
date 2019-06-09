
# load required libraries
library(roxygen2)
library(devtools)

# set project directory and name
setwd("~/Documents/R/prosjekter")
projname <- "ymse"

# turns objects found in "projname"/data.R (project root)
# into data files available through data()
# by saving them as .rda files in "projname"/data
add_data <- function(projname) {
	if (!dir.exists(projname)) {
		stop(paste("Could not find", projname, "in current directory"))
	}
	datapath <- file.path(projname, "data.R")
	if (!file.exists(datapath)) {
		stop(paste("Could not find", datapath))
	}
    dir.create(file.path(projname, "data"), showWarnings=FALSE)
    tmp.env <- new.env()
	source(datapath, local=tmp.env)
	tmp.list <- as.list(tmp.env, sorted=TRUE)
    files <- file.path(projname, "data", paste0(names(tmp.list), ".rda"))
    obj <- mapply(save, list=names(tmp.list), file=files, 
      MoreArgs=list(compress="xz", envir=tmp.env))
    if (length(files) == 1) {
        cat("File added:")
    } else {
    	cat("Files added:")
    }
    dtf <- data.frame(x=paste0(files), 
                      y=paste(sprintf("%.1f", file.size(files)/1000), "kB"))
    names(dtf) <- c(" ", " ")
    dtf
}

use_build_ignore(
  c("^data\\.R", "documenting\\.R", "commit\\.command", "\\.pdf$", 
    "\\.png$", "^.*\\.Rproj$", "^__.*"),
  pkg=projname, escape=FALSE)

readLines(file.path(projname, ".Rbuildignore"))

# detach(package:pollplot)

document(projname)
load_all(projname)
add_data(projname)

?weekday

check(projname, manual=FALSE)

# Inspect package object sizes
ppath <- paste0(find.package(projname, lib.loc=.libPaths()), "/R/", projname)
lazyLoad(ppath)

ll <- sapply(ls(), function(x) object.size(get(x)))
ll <- sort(ll, decreasing=TRUE)
ll[] <- utils:::format.object_size(ll, units="Kb")
as.data.frame(ll)

show_pdf <- function(package, lib.loc=NULL, opt="--force") {
    owd <- getwd()
    setwd(package)
    path <- find.package(package, lib.loc) 
    system(paste(shQuote(file.path(R.home("bin"), "R")), 
                  "CMD", "Rd2pdf", paste(opt, collapse=" "),
                  shQuote(path))) 
    setwd(owd)
} 
show_pdf(projname)


# run convenience script to add, commit and maybe push change
system(paste0("open ", projname, "/commit.command"))

# dev_example(projname)

install_github(paste0("AkselA/R-", projname))
library(projname, character.only=TRUE)

