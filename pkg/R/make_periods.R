make_periods <- function(times, first_date = FALSE) {
  nperiods <- length(times)-1
  if(first_date){
    periods <- paste(times[1], times[seq_len(nperiods)+1], sep = "-")
  } else {
  periods <- paste(times[seq_len(nperiods)], times[seq_len(nperiods)+1], sep = "-")
  }
  periods
}