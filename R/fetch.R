#' @import dplyr
#' @import purrr
#' @import tibble
NULL

read_src <- function(file_path, pkg_name, verbose = FALSE, filter_empty = TRUE) {
    if (verbose) {
        print(file_path)
    }
    content <- readLines(file_path)
    if (filter_empty) {
        content <- content[content != ""]
    }
    filename <- tail(strsplit(file_path, "/")[[1]], 1)
    tibble(code = content) %>% mutate(line_num = seq_along(code), filename = filename, pkg_name = pkg_name)
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


#' Fetch the source of a pkg and dump them into a tibble
#'
#' This function fetches the source code of a package from CRAN and dump them into a tibble.
#' 
#' @param pkg string, name of a package.
#' @param verbose boolean, display debug info.
#' @return a tibble with the source code of all R files, DESCRIPTION and NAMESPACE
#' @export
read_cranpkg <- function(pkg, verbose = FALSE) {
    cran_tempdir <- tempdir()
    res <- download.packages(pkg, destdir = cran_tempdir)
    return(read_tarball(res[2], verbose = verbose))
}

#' Dump the source from a package tarball into a tibble
#'
#' This function dump the content of a tarball containing an R package into a tibble.
#' 
#' @param tarball_path string, path to a tarball in .tar.gz.
#' @param verbose boolean, display debug info.
#' @return a tibble with the source code of all R files, DESCRIPTION and NAMESPACE
#' @export
read_tarball <- function(tarball_path, verbose = FALSE) {
    extracted_files_paths <- untar_pkg(tarball_path)
    target_files_paths <- grep(extracted_files_paths, pattern = '/R/.+\\.[rR]$|/DESCRIPTION$|/NAMESPACE$', value = TRUE)
    pkg_name <- extract_description(extracted_files_paths)
    pkg_sources <- map_dfr(target_files_paths, read_src, pkg_name = pkg_name, verbose = verbose)
    return(pkg_sources)
}

#' Extract package name from tarball
#'
#' This function returns the package name from a tarball containing an R package.
#' 
#' @param tarball_path string, path to a tarball in .tar.gz.
#' @param verbose boolean, display debug info.
#' @return package name
#' @export
read_tarball_meta <- function(tarball_path) {
    extracted_files_paths <- untar_pkg(tarball_path)
    return(extract_description(extracted_files_paths))
}

extract_description <- function(extracted_files_paths, field = c('Package', 'Bundle')) {
    target_files_paths <- grep(extracted_files_paths, pattern = '/R/.+\\.[rR]$|/DESCRIPTION$|/NAMESPACE$', value = TRUE)
    des_path <- grep(target_files_paths, pattern = "/DESCRIPTION$", value = TRUE)
    ## Some weird packages have two DESCRIPTION files.
    if (length(des_path) > 1) {
        des_path <- des_path[which.min(nchar(des_path))]
    }
    parsed_description <- read.dcf(des_path, field = field)
    if (!is.na(as.character(parsed_description[,1]))) {
        return(as.character(parsed_description[,1]))
    } else {
        return(as.character(parsed_description[,2]))
    }
}
