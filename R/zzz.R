
.onAttach <- function(libname, pkgname) {
  
    packageStartupMessage(
      "If importing existing GEDCOM files, you should ensure that they ",
      "are error free.\nThis package assumes imported GEDCOM files are valid and ",
      "very few validation checks are carried out.\nSeveral GEDCOM validators are available, ",
      "including an online validator at http://ged-inline.elasticbeanstalk.com/"
    )
    
}