#' @import dplyr
#' @import purrr
#' @import tibble
NULL

read_src <- function(file_path, pkg, verbose = FALSE, filter_empty = TRUE) {
    if (verbose) {
        print(file_path)
    }
    ## base_dir <- paste0(cran_tempdir, "/", pkg, "/")
    ## if (filename %in% c("NAMESPACE", "DESCRIPTION")) {
    ##     path <- paste0(base_dir, filename)
    ## } else {
    ##     path <- paste0(base_dir, "R/", filename)
    ## }
    content <- readLines(file_path)
    if (filter_empty) {
        content <- content[content != ""]
    }
    filename <- tail(strsplit(file_path, "/")[[1]], 1)
    tibble(code = content) %>% mutate(line_num = seq_along(code), filename = filename, pkg = pkg)
}

# Helper function to extract tarball and return the paths of all files and directories in the tarball.
untar_pkg <- function(pkg_path) {
    content_list <- untar(pkg_path, list = TRUE)
    dest_tempdir <- tempdir()
    z <- untar(pkg_path, exdir = dest_tempdir)
    if (z == 0) {
        return(paste0(dest_tempdir, "/", content_list))
    }
    stop('Untar failed.')
}


#' Fetch the source of a pkg into a tibble
#'
#' This function fetches the source code of a package and dump them into a tibble.
#' @param pkg string, name of a package.
#' @param verbose boolean, display debug info.
#' @return a tibble with the source code of all R files, DESCRIPTION and NAMESPACE
#' @export
read_cranpkg <- function(pkg, verbose = FALSE) {
    cran_tempdir <- tempdir()
    res <- download.packages(pkg, destdir = cran_tempdir)
    extracted_files <- untar_pkg(res[2])
    target_files <- grep(extracted_files, pattern = '/R/.+\\.[rR]$|/DESCRIPTION$|/NAMESPACE$', value = TRUE)
    pkg_sources <- map_dfr(target_files, read_src, pkg = pkg, verbose = verbose)
    return(pkg_sources)
}


