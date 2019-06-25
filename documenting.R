
# load required libraries
library(roxygen2)
library(devtools)

# set project directory and name
setwd("~/Documents/R/prosjekter")
projname <- "ymse"

# Inspect package object sizes
objsizes <- function(projname, load.installed=FALSE) {
	if (load.installed) {
		ppath <- find.package(projname, lib.loc=.libPaths())
	    ppathr <- paste0(, "/R/", projname)
        lazyLoad(ppathr)
    }
	pkgls <- ls(paste0("package:", projname))
	ll <- t(sapply(pkgls, function(x) {
		o <- get(x)
		c(bytes=object.size(o), class=class(o))
		}))
	dtf <- type.convert(data.frame(ll))
	dtf$"SI size" <- sapply(dtf[,1], 
	  utils:::format.object_size, units="auto", standard="SI")
	tot <- sum(dtf[,1])
    tot <- utils:::format.object_size(tot, units="auto", standard="SI")
    dtf <- dtf[order(-dtf[,1]), c(1, 3, 2)]
    print(dtf)
    cat(paste("\nTotal:", tot))
    invisible(list(dtf, total=tot))
}

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

# Create and show documentation PDF
show_pdf <- function(package, lib.loc=NULL, opt="--force") {
    owd <- getwd()
    setwd(package)
    path <- find.package(package, lib.loc) 
    system(paste(shQuote(file.path(R.home("bin"), "R")), 
                  "CMD", "Rd2pdf", paste(opt, collapse=" "),
                  shQuote(path))) 
    setwd(owd)
} 

# get a list of function arguments ready for Roxygen2 use
params <- function(fun) {
	cat(paste("@param", names(formals(fun))), sep="\n")
}

use_build_ignore(
  c("^data\\.R", "documenting\\.R", "commit\\.command", "\\.pdf$", 
    "\\.png$", "^.*\\.Rproj$", "^__.*", "^\\.DS_Store$"),
  pkg=projname, escape=FALSE)

readLines(file.path(projname, ".Rbuildignore"))

# detach(paste0("package:", projname), character.only=TRUE)

document(projname)
load_all(projname)
add_data(projname)

?file_ext

check(projname, manual=FALSE)

objsizes(projname)

show_pdf(projname)


# run convenience script to add, commit and maybe push change
system(paste0("open ", projname, "/commit.command"))

# dev_example(projname)

install_github(paste0("AkselA/R-", projname))
library(projname, character.only=TRUE)

