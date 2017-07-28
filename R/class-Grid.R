.Var3d <- R6Class("Var3d",
                  public = list(
                      x = 0, y = 0, z = 0,
                      initialize = function(x, y, z) {
                          self$x = x
                          self$y = y
                          self$z = z
                      }
                  )
)

#' 3D variable with x, y, z dimensions
#'
#' @param x the x dimension
#' @param y the y dimension
#' @param z the z dimension
#' @export
#' @importFrom R6 R6Class
Var3d <- function(x = 0, y = 0, z = 0) {
    .Var3d$new(x = x, y = y, z = z)
}



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
            V  = Var3d(),
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
                self$V$x = 0
                self$V$y = 0
                self$V$z = 0
                # cat(sprintf("Grid of %dx%dx%d", self$Nx, self$Ny, self$Nz))
            }
        )
)


# grid <- Grid$new(8, 8, 1)
# Grid of 8x8x1



