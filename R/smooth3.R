
#' @export
Smooth3 <- function(data, filt = "box", sz = 3) {
    if (ndims(data) != 3)
        stop("Object is not a 3D array")

    if (length(sz) == 1)
        sz = c(sz, sz, sz)     # c(3,3,3)
    else if (length(sz) != 3)
        stop("Ivalid size input.")

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


padreplicate <- function(a, padSize) {
    # Pad an array by replicating values.
    numDims <- length(padSize)
    # idx = matrix(nrow = numDims, ncol = 1)
    idx <- vector("list", numDims)
    for (k in 1:numDims) {
        M <-  dim(a)[k]       # 32
        onesVector <-  ones(1, padSize[k])
        idx[[k]] <- c(onesVector, 1:M, M * onesVector)
    }
    return(a[unlist(idx[1]), unlist(idx[2])])
    #return(a[apply(idx, 1, function(x) unlist[x])])
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



