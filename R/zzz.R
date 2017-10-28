.onLoad <- function(libname = base::find.package("usefulr"), pkgname = "usefulr"){

  # CRAN Note avoidance
  if (getRversion() >= "2.15.1")
    utils::globalVariables(
      c("term", "estimate", "std.error", "p.value", "printout")
      )
  base::invisible()
}
