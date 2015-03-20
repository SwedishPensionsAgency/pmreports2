.onLoad <- function (libname, pkgname) {
  #Create a environment in the package environment for storing the translation tables 
  assign(".pmreports.env", new.env(), envir= asNamespace(pkgname))
}

.onAttach <- function(libname, pkgname){
  theme_preloading()

  set_year(2013)
  set_fontsize(7.2)
  set_lang(lang = "sv")
  
  if (require(MIDAS)) {
    .midas_db <- "\\\\psysstat8w\\G\\MIDAS"
    change.database(.midas_db)
  } else {
    message("Please install the MIDAS-package with dependencies and run '?change.database' for full function. Maybe it is located under 'F:\\20-Linje lokalt\\26-Analysavd\\00-Avdelningsgemensamt\\79-Pensionsmodellen\\50-Utveckling\\R\\R_paket_midas\\built_packages'. Please contact the maintainer of the package.")
  }

  if (!require(format.plot)) {
    message("Please install the package format.plot devtools::install_github('SwedishPensionsAgency/format.plot') for full functionallity.")
  }
  
  if (!require(translater)) {
    message("Please install the package translater devtools::install_github('SwedishPensionsAgency/translater') for full functionallity.")
  }
  
  assign("pm_colors", pm_colors_cmyk(), envir = .GlobalEnv)
  
  Sys.setenv(R_GSCMD = ghostscript_path())
  
}

