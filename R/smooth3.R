
#' @export
smooth3 <- function(data, filt = "b", sz = 3) {
    padsize <- (sz - 1) / 2
    ones(sz) / prod(sz)

    if (filt == "b")  # box
        smooth <- ones(sz) / prod(sz)
    else
        stop("Unknown filter")
}



