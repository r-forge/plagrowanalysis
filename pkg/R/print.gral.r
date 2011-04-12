print.gral <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat(x$lname,"\n")
  cat("Units: ", x$units, "\n\n")
  print(x$ans)
  invisible(x)
}
  