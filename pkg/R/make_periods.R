make_periods <- function(times) {
  nperiods <- length(times)-1
  periods <- paste(times[seq_len(nperiods)], times[seq_len(nperiods)+1], sep = "-")
  periods
}