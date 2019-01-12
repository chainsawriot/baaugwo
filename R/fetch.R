#' @import dplyr
#' @import purrr
#' @import tibble
#' @import RSQLite
NULL

read_src <- function(filename, pkg, cran_tempdir, verbose = FALSE) {
    if (verbose) {
        print(filename)
    }
    base_dir <- paste0(cran_tempdir, "/", pkg, "/")
    if (filename %in% c("NAMESPACE", "DESCRIPTION")) {
        path <- paste0(base_dir, filename)
    } else {
        path <- paste0(base_dir, "R/", filename)
    }
    content <- readLines(path)
    tibble(code = content) %>% filter(code != "") %>% mutate(line_num = seq_along(code), filename = filename, pkg = pkg)
}

#' Fetch the source of a pkg into a tibble
#'
#' This function fetches the source code of a package and dump them into a tibble.
#' @param pkg string, name of a package.
#' @param verbose boolean, display debug info.
#' @return a tibble with the source code of all R files, DESCRIPTION and NAMESPACE
#' @export
fetch_pkg_src <- function(pkg, verbose = FALSE) {
    cran_tempdir <- tempdir()
    res <- download.packages(pkg, destdir = cran_tempdir)
    z <- untar(res[2], exdir = cran_tempdir)
    all_files <- c(list.files(paste0(cran_tempdir, "/", pkg, "/R"), pattern = '\\.[rR]$'), "DESCRIPTION", "NAMESPACE")
    pkg_sources <- map_dfr(all_files, read_src, pkg = pkg, cran_tempdir = cran_tempdir, verbose = verbose)
    return(pkg_sources)
}

#' Dump source code of a package into a SQLite database.
#'
#' More information later.
#' 
#' @export
dump_pkg_src_db <- function(pkg, dbname) {
    db <- dbConnect(SQLite(), dbname = dbname)
    pkg_sources <- fetch_pkg_src(pkg)
    dbWriteTable(conn = db, name = 'code', value = pkg_sources,  append = TRUE)
    dbDisconnect(db)
}
