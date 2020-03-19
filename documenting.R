
# load required libraries

# devtools::install_version("devtools", version="1.13.3")
# devtools::install_version("roxygen2", version="6.1.1")

library(roxygen2)
# library(devtools)
sessionInfo()


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
    invisible(dtf)
}

# turns objects found in "projname"/data.R (project root)
# into data files available through data()
# by saving them as .rda files in "projname"/data
pkg_data <- function(package, clean=TRUE) {
    if (!dir.exists(package)) {
        stop(paste("Could not find", package, "in current directory"))
    }
    datapath <- file.path(package, "data.R")
    if (!file.exists(datapath)) {
        stop(paste("Could not find", datapath))
    }
    if (clean) {
    	unlink(file.path(package, "data"))
    }
    dir.create(file.path(package, "data"), showWarnings=FALSE)
    tmp.env <- new.env()
    source(datapath, local=tmp.env)
    tmp.list <- as.list(tmp.env, sorted=TRUE)
    files <- file.path(package, "data", paste0(names(tmp.list), ".rda"))
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


descr <- function(package) {
    if (!dir.exists(package)) {
        stop(paste("Could not find", package, "in current directory"))
    }
	d <- scan(file.path(package, "DESCRIPTION"), "", sep="\n")
	d <- split(d, cumsum(grepl("^[^#: -][^: ]+:", d)))
	d <- gsub(" +|\t", " ", sapply(d, paste, collapse="\n"))
	d <- gsub(" *\n *", "\n", sapply(d, paste, collapse="\n"))
	names(d) <- sub(": .*", "", d)
	d <- sub("^[^:]+: *", "", d)
	sub("^([^:]+): *.*", "\\1", d)
}

# Create and show documentation PDF
pkg_pdf <- function(package, popt="--force") {
    if (!dir.exists(package)) {
        stop(paste("Could not find", package, "in current directory"))
    }
    owd <- getwd()
	popts <- paste(popt, collapse=" ")

	R <- file.path(R.home("bin"), "R")
    pkg <- file.path(owd, package) 
	parg <- paste("CMD", "Rd2pdf", popts, shQuote(pkg))

    setwd(package)
    system2(R, parg)
    setwd(owd)
}

pkg_check <- function(package, bopt="--no-manual", 
  copt=c("--as-cran", "--no-manual", paste0("--output=", tempdir())),
  rm.src=TRUE) {
    if (!dir.exists(package)) {
        stop(paste("Could not find", package, "in current directory"))
    }
	bopts <- paste(bopt, collapse=" ")
	copts <- paste(copt, collapse=" ")
	
	pkg <- file.path(getwd(), package)
	dcr <- descr(package)
	tgz <- paste0(dcr["Package"], "_", dcr["Version"], ".tar.gz")
	R <- file.path(R.home("bin"), "R")
	
	barg <- paste("CMD", "build", bopts, shQuote(pkg))
	system2(R, barg)

	carg <- paste("CMD", "check", copts, tgz)
	system2(R, carg)
	
	if (rm.src) {
		cwd <- system2(R, "--no-save --slave -e 'cat(getwd())'", stdout=TRUE)
	    n <- file.remove(file.path(tail(cwd, 1), tgz))
	}
}


pkg_install <- function(package, ..., rm.src=TRUE, bopt="") {
    if (!dir.exists(package)) {
        stop(paste("Could not find", package, "in current directory"))
    }

    bopts <- paste(bopt, collapse=" ")

	pkg <- file.path(getwd(), package)
	dcr <- descr(package)
	tgz <- paste0(dcr["Package"], "_", dcr["Version"], ".tar.gz")
	R <- file.path(R.home("bin"), "R")
	
	barg <- paste("CMD", "build", bopts, shQuote(pkg))
	system2(R, barg)

    install.packages(tgz, repos=NULL, ...)

    # iarg <- paste("CMD", "INSTALL", iopts, tgz)
    # system2(R, iarg)

	if (rm.src) {
		cwd <- system2(R, "--no-save --slave -e 'cat(getwd())'", stdout=TRUE)
	    n <- file.remove(file.path(tail(cwd, 1), tgz))
	}
}


# get a list of function arguments ready for Roxygen2 use
params <- function(fun) {
    cat(paste("@param", names(formals(fun))), sep="\n")
}

# Write .Rbuildignore file
buildignore <- function(projname, add=FALSE, pat=c("^data\\.R", 
  "documenting\\.R", "commit\\.command", "\\.pdf$", "\\.png$",
  "\\.Rproj$", "^__.*", "^\\.DS_Store$")) {
    bignore.path <- file.path(projname, ".Rbuildignore") 
    if (!file.exists(bignore.path)) {
        file.create(bignore.path)
    }
    if (add) {
        pat <- union(scan(bignore.path, "", sep="\n"), pat)
    }
    message(paste(pat, collapse="   "))
    cat(pat, file=bignore.path, sep="\n")
}

buildignore(projname)
# detach(paste0("package:", projname), character.only=TRUE)

roxygenise(projname)
add_data(projname)


objsizes(projname)

show_pdf(projname)


# run convenience script to add, commit and maybe push change
system(paste0("open ", projname, "/commit.command"))


install_github(paste0("AkselA/R-", projname), build_manual=TRUE, force=TRUE)
library(projname, character.only=TRUE)
?aug_median
