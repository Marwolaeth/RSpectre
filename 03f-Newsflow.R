library(readxl)
library(data.table)
library(tidyr)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(tm)
library(tidytext)
library(dtplyr)
library(RNewsflow)
library(igraph)

weight_terms <- function(
  term.rank,
  method = 'log_hyperbolic',
  numerator = 2,
  base = 10,
  correction = 2,
  pos.weights = c(
    PROPN = 1.5,
    NOUN  = 1.2
  ),
  term.pos = NULL # an object of class "character"
) {
  stopifnot(method %in% c('log', 'hyperbolic', 'log_hyperbolic'))
  if (length(pos.weights) & length(term.pos)) {
    if (any(is.na(term.pos))) {
      weight <- 1
      warning(
        'Parts-of-speech are not specified: using default value',
        .call = FALSE
      )
    }
    weight <- ifelse(
      term.pos %in% names(pos.weights),
      pos.weights[term.pos],
      1
    )
  } else {
    weight <- 1
  }
  if (method == 'log') {
    return(weight * log(rev(term.rank) + correction, base = base))
  } else if (method == 'hyperbolic') {
    return(weight * (numerator / term.rank))
  } else {
    return(weight * (numerator / (log(term.rank, base = base) + correction)))
  }
}
