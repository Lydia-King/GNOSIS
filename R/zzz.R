# https://community.rstudio.com/t/
# trouble-including-image-jpeg-png-svg-etc-in-shiny-app-embedded-in-r-package/
# 56156
.onLoad <- function(libname, pkgname) {
    addResourcePath(
        prefix = "www",
        directoryPath = system.file("www", package = pkgname)
    )
}

.onUnload <- function(libname, pkgname) {
    removeResourcePath("www")
}
