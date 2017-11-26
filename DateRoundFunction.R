myRound <- function (x, convert = TRUE)  {
  as.Date(x)
  x <- as.POSIXlt(x)
  mins <- x$min
  mult <- mins %/% 15
  remain <- mins %% 15
  if(remain > 7L || (remain == 7L && x$sec > 29))
    mult <- mult + 1
  if(mult > 3) {
    x$min <- 0
    x <- x + 3600
  } else {
    x$min <- 15 * mult
  }
  x <- trunc.POSIXt(x, units = "mins")
  if(convert) {
    x <- format(x, format = "%Y-%m-%d %H:%M")
  }
  x
}