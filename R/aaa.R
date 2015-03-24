.onLoad <- function (libname, pkgname) {
  #Create a environment in the package environment for storing the translation tables 
  assign(".pmreports.env", new.env(), envir= asNamespace(pkgname))
}

.onAttach <- function(libname, pkgname) {
  theme_preloading()
  
  set_fontsize(7.2)
  set_lang(lang = "sv")
  
  # pmbundle is a non-required dependency. However, several functions in pmreports depend on it at the moment.
  if (!require(pmbundle)) {
    message("Package pmbundle is not installed. pmreports will work without it but some functionality might be limited or broken. The pmbundle package is only available to employees of the Swedish Pensions Agency. For more information, please contact the package maintainer.")
  }  
  if (!require(format.plot)) {
    message("Please install the package format.plot devtools::install_github('SwedishPensionsAgency/format.plot') for full functionallity.")
  }
  
  if (!require(translater)) {
    message("Please install the package translater devtools::install_github('SwedishPensionsAgency/translater') for full functionallity.")
  }
  
  assign("pm_colors", pm_colors_cmyk(), envir = .GlobalEnv)
}

