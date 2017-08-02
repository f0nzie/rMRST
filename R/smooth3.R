
#' Smooth a 3-D array
#' @param data three dimensaional array
#' @param filt type of filter
#' @param sz size
#' @export
Smooth3 <- function(data, filt = "box", sz = 3) {
    if (ndims(data) != 3)
        stop("Object is not a 3D array")

    if (length(sz) == 1)
        sz = c(sz, sz, sz)     # c(3,3,3)
    else if (length(sz) != 3)
        stop("Invalid size input.")

    padSize <- (sz - 1) / 2    # c(1, 1, 1)

    if (any(padSize < 0))
        stop("Invalid size values")

    if (substring(filt,1,1) == "b")  {  # box
        smooth <- ones(sz) / prod(sz)
    } else {
        stop("Unknown filter")
    }
    #sz
    # padSize
    #smooth
}


#' Duplicate rows and columns in N-dimensional array
#' @param a n-dimensional array
#' @param padSize padding vector
#' @export
padreplicate <- function(a, padSize) {
    if (length(padSize) != length(dim(a)))
        stop("Incorrect dimensions of padSize")
    # Pad an array by replicating values.
    numDims <- length(padSize)
    idx <- vector("list", numDims)
    for (k in 1:numDims) {
        M <-  dim(a)[k]       # 32
        # onesVector <-  ones(1, padSize[k])
        onesVector <-  matrix(1, 1, padSize[k])
        idx[[k]] <- c(onesVector, 1:M, M * onesVector)
    }
    do.call( `[`, c(list(a), idx))
}


# function b=padreplicate(a, padSize)
# %Pad an array by replicating values.
# numDims = length(padSize);
# idx = cell(numDims, 1);
# for k = 1:numDims
# M = size(a,k);
# onesVector = ones(1,padSize(k));
# idx{k} = [onesVector 1:M M*onesVector];
# end
#
# b = a(idx{:});



