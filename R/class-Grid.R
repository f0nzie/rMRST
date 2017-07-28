#' Grid class
#'
#' @export
#' @importFrom R6 R6Class
Grid <- R6Class("Grid",
        public = list(
            Nx = 0, Ny = 0, Nz = 0,
            hx = 0, hy = 0, hz = 0,
            K  = NA,
            N  = NA,
            initialize = function(Nx, Ny, Nz) {
                self$Nx = Nx
                self$Ny = Ny
                self$Nz = Nz
                self$hx = 1 / self$Nx
                self$hy = 1 / self$Ny
                self$hz = 1 / self$Nz
                self$K  = array(1, c(3, self$Nx, self$Ny))
                self$N  = self$Nx * self$Ny * self$Nz
                self$K = ones(3, self$Nx, self$Ny)
                # cat(sprintf("Grid of %dx%dx%d", self$Nx, self$Ny, self$Nz))
            }
        )
)

# grid <- Grid$new(8, 8, 1)
# Grid of 8x8x1
