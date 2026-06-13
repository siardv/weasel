# fixture: small panel with hand-built, known participation patterns
make_fixture <- function() {
  # 8 waves; patterns chosen to exercise gaps, endpoints, and entry/exit
  patterns <- list(
    a1 = 1:8,                     # complete
    a2 = 1:8,                     # complete (shares pattern with a1)
    b1 = c(1, 2, 4, 5, 6, 7, 8),  # one interior gap of length 1
    c1 = c(1, 2, 3, 6, 7, 8),     # one interior gap of length 2
    d1 = c(2, 3, 4, 5, 6, 7),     # missing both endpoints, no interior gap
    e1 = c(1, 3, 5, 7),           # three interior gaps of length 1, no upper
    f1 = c(1, 2, 3)               # early exit only
  )
  do.call(rbind, lapply(names(patterns), function(nm) {
    data.frame(id = nm, time = patterns[[nm]],
               var1 = seq_along(patterns[[nm]]),
               stringsAsFactors = FALSE)
  }))
}
