#' Save as PNG
#' 
#' Save the contents of the current Quartz window as PNG file
#' 
#' @param file file name. If it contains any \code{"\%"} it is passed on as a 
#'        format string to \code{format(Sys.time(), file)}. A \code{.png} file 
#'        extension is added automatically.
#' @param width pixel width of the PNG file
#' @param dir directory to save to. Defaults to current working directory
#' @param force force overwriting of existing file with same name. By default
#'        duplicate path names are resolved by appending \code{_N}, using 
#'        successive integers, to the end of the file name.
#' 
#' @return
#' A PNG file is written to disk and a message is written to the console, giving
#' the new file's path and pixel dimensions. The file path is also returned
#' invisibly.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' set.seed(1)
#' i_h100 <- round(runif(100, 2, 30), 2)
#' i_cd <- rexp(100, 1/i_h100)
#' mydata <- data.frame(i_cd, i_h100)
#' 
#' mydata$i_h100_2m <- cut(mydata$i_h100, seq(2, 30, by=2))
#' 
#' i_cd_2m <- aggregate(i_cd ~ i_h100_2m, mydata, mean)
#' 
#' set_mar(x=2.5)
#' plot.default(i_cd_2m, xaxt="n", main="Groupwise means", xlab="", cex.main=1)
#' axis(1, i_cd_2m[,1], as.character(i_cd_2m[,1]), cex.axis=0.6, las=2)
#' 
#' quartz.png()
#' p <- quartz.png("test", 550)
#' file.info(p)
#' }

quartz.png <- function(file="%Y%m%d_%H", width=550, dir, force=FALSE) {
    if (grepl("%", file, fixed=TRUE)) {
        file <- paste0(format(Sys.time(), file), ".png")
    } else {
        if (!grepl("\\.png$", file)) {
            file <- paste0(file, ".png")
        }
    }
    if (missing(dir)) {
        dir <- getwd()
    }
    dir <- normalizePath(path.expand(dir))
    filepath <- file.path(dir, file)
    if (!force) {
        if (file.exists(filepath)) {
            cand <- list.files(dir, pattern="\\.png$")
            file <- resolve_dup(file, cand)
            filepath <- file.path(dir, file)
        }
    }
    fin <- par()$fin
    dpi <- width/fin[1]
    dim <- floor(fin*dpi)
    quartz.save(filepath, dpi=dpi)
    message(paste0(
      "File:\n ",
      filepath, "\n ",
      dim[1], "x", dim[2], " pixels"))
    invisible(filepath)
}

#' Resolve duplicate
#' 
#' Resolve duplicate names by appending successive integers
#' 
#' @param x character string; name to be resolved
#' @param candidates character vector; possible duplicate names
#' @param ignore.extension logical; append to the end of \code{x}, even if
#'        it has something that can be interpreted as an extension
#' 
#' @export
#' 
#' @examples
#' x <- c("my.var", "aaa.png", "aaa.jpg", "aaa_1.png", "doc-folder")
#' resolve_dup("aaa.jpg", x)
#' resolve_dup("aaa.png", x)
#' resolve_dup("aaa_1.png", x)
#' resolve_dup("doc-folder", x)
#' resolve_dup("New Document", x)
#' resolve_dup("my.var", x, ignore.ext=TRUE)
#' x <- c(x, resolve_dup("aaa.png", x))
#' resolve_dup("aaa.png", x)

resolve_dup <- function(x, candidates, ignore.extension=FALSE) {
    cand <- candidates
    if (sum(x == cand) == 1) {
        if (ignore.extension) {
            ne <- c(x, "")
        } else {
            ne <- file_name_ext(x)
            if (is.na(ne[2])) {
                ne[2] <- ""
            } else {
                ne[2] <- paste0(".", ne[2])
            }
        }
        ma <- grep(paste0(ne[1], "_[0-9]+", ne[2], "$"), cand, value=TRUE)
        if (length(ma) > 0) {
            ma <- substr(ma, 1, nchar(ma)-nchar(ne[2]))
            i <- sub(paste0(".*_([0-9]+)$"), "\\1", ma)
            i <- max(as.numeric(i)) + 1
            ma <- paste0(sub("[0-9]+$", "", ma[1]), i)
            file_new <- paste0(ma, ne[2])
            } else {
                file_new <- paste(ne, collapse="_1")
            }
    } else {
        file_new <- x
    }
    file_new
}
