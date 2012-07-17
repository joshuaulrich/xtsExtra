.onAttach <- function(...){
  # Suggests quantmod to user of OHLC plot.xts
  assign(".QUANTMOD_MESSAGE", TRUE, .GlobalEnv)
}
