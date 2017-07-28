#' TPFA function
#' @param Grid a Grid object
#' @param K permeability
#' @param q flow rates
#' @export
TPFA01 <- function(Grid, K, q) {
    # Compute transmissibilities by harmonic averaging.
    Nx = Grid$Nx; Ny = Grid$Ny; Nz = Grid$Nz;
    N = Nx * Ny * Nz;
    hx = Grid$hx; hy = Grid$hy; hz = Grid$hz;
    list(Nx = Nx, Ny = Ny, Nz = Nz, hx = hx, hy = hy, hz = hz)
}

TPFA02 <- function(Grid, K, q) {
    # Compute transmissibilities by harmonic averaging.
    Nx = Grid$Nx; Ny = Grid$Ny; Nz = Grid$Nz;
    N = Nx * Ny * Nz;
    hx = Grid$hx; hy = Grid$hy; hz = Grid$hz;
    L <- K^(-1)      # 3x8x8 array
    list(L = L, K = Grid$K)
}


TPFA03 <- function(Grid, K, q) {
    # Compute transmissibilities by harmonic averaging.
    Nx = Grid$Nx; Ny = Grid$Ny; Nz = Grid$Nz;
    N = Nx * Ny * Nz;
    hx = Grid$hx; hy = Grid$hy; hz = Grid$hz;
    L <- K^(-1)      # 3x8x8 array

    tx = 2*hy*hz/hx; TX <- zeros(Nx+1,Ny,Nz);
    ty = 2*hx*hz/hy; TY <- zeros(Nx,Ny+1,Nz);
    tz = 2*hx*hy/hz; TZ <- zeros(Nx, Ny, (Nz+1));

    TX[2:Nx,,] = tx / (L[1, 1:(Nx-1), ] + L[1, 2:Nx, ])
    TY[,2:Ny,] = ty / (L[2, , 1:Ny-1] + L[2, ,2:Ny])
    # print(L[3,,c(Nz-1)] )
    # print( L[3,,c(1-Nz)])
    # print(tz / (L[3,,1:(Nz-1)] + L[3, ,2:Nz])) # 8x2x1
    # print(tz / (L[3,,1:(Nz-1)] + L[3, ,1-Nz]))  #0x0x0
    # TZ[,,2:Nz] = tz / (L[3,,1:(Nz-1)] + L[3, ,2:Nz])
    TZ[,,1-Nz] = tz / (L[3,, Nz-1] + L[3, , 1-Nz])
    # print(TZ)

    list(TX = TX, TY = TY, TZ = TZ)
}

TPFA04 <- function(Grid, K, q) {
    # Compute transmissibilities by harmonic averaging.
    Nx = Grid$Nx; Ny = Grid$Ny; Nz = Grid$Nz;
    N = Nx * Ny * Nz;
    hx = Grid$hx; hy = Grid$hy; hz = Grid$hz;
    L <- K^(-1)      # 3x8x8 array

    tx = 2*hy*hz/hx; TX <- zeros(Nx+1,Ny,Nz);
    ty = 2*hx*hz/hy; TY <- zeros(Nx,Ny+1,Nz);
    tz = 2*hx*hy/hz; TZ <- zeros(Nx, Ny, (Nz+1));

    TX[2:Nx,,] = tx / (L[1, 1:(Nx-1), ] + L[1, 2:Nx, ])
    TY[,2:Ny,] = ty / (L[2, , 1:Ny-1] + L[2, ,2:Ny])
    TZ[,,1-Nz] = tz / (L[3,, Nz-1] + L[3, , 1-Nz])

    # Assemble TPFA discretization matrix.
    x1 = pracma::Reshape(TX[1:Nx,,],N,1); x2 = pracma::Reshape(TX[2:(Nx+1),,],N,1);
    y1 = pracma::Reshape(TY[,1:Ny,],N,1); y2 = pracma::Reshape(TY[,2:(Ny+1),],N,1);
    z1 = pracma::Reshape(TZ[,,1:Nz],N,1); z2 = pracma::Reshape(TZ[,,2:(Nz+1)],N,1);

    list(x1 = x1, y1 = y1, z1 = z1, x2 = x2, y2 = y2, z2 = z2)
}

TPFA05 <- function(Grid, K, q) {
    # Compute transmissibilities by harmonic averaging.
    Nx = Grid$Nx; Ny = Grid$Ny; Nz = Grid$Nz;
    N = Nx * Ny * Nz;
    hx = Grid$hx; hy = Grid$hy; hz = Grid$hz;
    L <- K^(-1)      # 3x8x8 array

    tx = 2*hy*hz/hx; TX <- zeros(Nx+1,Ny,Nz);
    ty = 2*hx*hz/hy; TY <- zeros(Nx,Ny+1,Nz);
    tz = 2*hx*hy/hz; TZ <- zeros(Nx, Ny, (Nz+1));

    TX[2:Nx,,] = tx / (L[1, 1:(Nx-1), ] + L[1, 2:Nx, ])
    TY[,2:Ny,] = ty / (L[2, , 1:Ny-1] + L[2, ,2:Ny])
    TZ[,,1-Nz] = tz / (L[3,, Nz-1] + L[3, , 1-Nz])

    # Assemble TPFA discretization matrix.
    x1 = pracma::Reshape(TX[1:Nx,,],N,1); x2 = pracma::Reshape(TX[2:(Nx+1),,],N,1);
    y1 = pracma::Reshape(TY[,1:Ny,],N,1); y2 = pracma::Reshape(TY[,2:(Ny+1),],N,1);
    z1 = pracma::Reshape(TZ[,,1:Nz],N,1); z2 = pracma::Reshape(TZ[,,2:(Nz+1)],N,1);

    DiagVecs <-  cbind(cbind(-z2,-y2,-x2,x1+x2+y1+y2+z1+z2,-x1,-y1,-z1))
    DiagIndx  <- c(-Nx*Ny,-Nx,-1,0,1,Nx,Nx*Ny)

    list(DiagVecs = DiagVecs, DiagIndx = DiagIndx)
}


TPFA06 <- function(Grid, K, q) {
    # Compute transmissibilities by harmonic averaging.
    Nx = Grid$Nx; Ny = Grid$Ny; Nz = Grid$Nz;
    N = Nx * Ny * Nz;
    hx = Grid$hx; hy = Grid$hy; hz = Grid$hz;
    L <- K^(-1)      # 3x8x8 array

    tx = 2 *hy*hz/hx; TX <- zeros(Nx+1,Ny,Nz);
    ty = 2 *hx*hz/hy; TY <- zeros(Nx,Ny+1,Nz);
    tz = 2 *hx*hy/hz; TZ <- zeros(Nx, Ny, (Nz+1));

    # TX(2:Nx,:,:)   = tx./ (L(1, 1:Nx-1, :, :) + L(1, 2:Nx, :, :));
    # TY(:,2:Ny,:)   = ty./ (L (2,:,1:Ny-1,:)  + L(2,:,2:Ny,:));
    # TZ (:,:, 2:Nz) = tz./ (L(3,:,:,1: Nz-1)  + L(3,:,:, 2:Nz));
    TX[2:Nx,,] = tx / (L[1, 1:(Nx-1), ] + L[1, 2:Nx, ])
    TY[,2:Ny,] = ty / (L[2, , 1:Ny-1] + L[2, ,2:Ny])
    TZ[,,1-Nz] = tz / (L[3,, Nz-1] + L[3, , 1-Nz])

    # Assemble TPFA discretization matrix.
    x1 = pracma::Reshape(TX[1:Nx,,],N,1); x2 = pracma::Reshape(TX[2:(Nx+1),,],N,1);
    y1 = pracma::Reshape(TY[,1:Ny,],N,1); y2 = pracma::Reshape(TY[,2:(Ny+1),],N,1);
    z1 = pracma::Reshape(TZ[,,1:Nz],N,1); z2 = pracma::Reshape(TZ[,,2:(Nz+1)],N,1);

    DiagVecs <-  cbind(cbind(-z2,-y2,-x2,x1+x2+y1+y2+z1+z2,-x1,-y1,-z1))
    DiagIndx  <- c(-Nx*Ny,-Nx,-1,0,1,Nx,Nx*Ny)

    # permeability K
    # Grid$K

    spA <-  m.spdiags(DiagVecs, DiagIndx, N, N)
    A <- spA$B
    # A[1,1] <-  A[1,1] + sum(Grid.K[,1,1,1])
    #
    A
}

TPFA07 <- function(Grid, K, q) {
    # Compute transmissibilities by harmonic averaging.
    Nx = Grid$Nx; Ny = Grid$Ny; Nz = Grid$Nz;
    N = Nx * Ny * Nz;
    hx = Grid$hx; hy = Grid$hy; hz = Grid$hz;
    L <- K^(-1)      # 3x8x8 array

    tx = 2 *hy*hz/hx; TX <- zeros(Nx+1,Ny,Nz);
    ty = 2 *hx*hz/hy; TY <- zeros(Nx,Ny+1,Nz);
    tz = 2 *hx*hy/hz; TZ <- zeros(Nx, Ny, (Nz+1));

    TX[2:Nx,,] = tx / (L[1, 1:(Nx-1), ] + L[1, 2:Nx, ])
    TY[,2:Ny,] = ty / (L[2, , 1:Ny-1] + L[2, ,2:Ny])
    TZ[,,1-Nz] = tz / (L[3,, Nz-1] + L[3, , 1-Nz])

    # Assemble TPFA discretization matrix.
    x1 = pracma::Reshape(TX[1:Nx,,],N,1); x2 = pracma::Reshape(TX[2:(Nx+1),,],N,1);
    y1 = pracma::Reshape(TY[,1:Ny,],N,1); y2 = pracma::Reshape(TY[,2:(Ny+1),],N,1);
    z1 = pracma::Reshape(TZ[,,1:Nz],N,1); z2 = pracma::Reshape(TZ[,,2:(Nz+1)],N,1);

    DiagVecs <-  cbind(cbind(-z2,-y2,-x2,x1+x2+y1+y2+z1+z2,-x1,-y1,-z1))
    DiagIndx  <- c(-Nx*Ny,-Nx,-1,0,1,Nx,Nx*Ny)

    # the sparse matrix function m.spdiags returns two components:
    # B and d
    spA <-  m.spdiags(DiagVecs, DiagIndx, N, N)
    A <- spA$B   # get component "B" like in Matlab
    # In original Matlab code sum(Grid.K(:,1,1,1)) means that it will add
    # all the rows in the first column of the first panel
    # sum(Grid.K(:,1,1,1)) = sum(Grid$K[, 1, 1])
    # sum(Grid.K(1,:,1,1)) = sum(Grid$K[1, , 1])
    # sum(Grid.K(1,1,:,1)) = sum(Grid$K[1, 1, ])
    A[1, 1] <-  A[1, 1] + sum(Grid$K[, 1, 1])
    A
}

TPFA08 <- function(Grid, K, q) {
    # Compute transmissibilities by harmonic averaging.
    Nx = Grid$Nx; Ny = Grid$Ny; Nz = Grid$Nz;
    N = Nx * Ny * Nz;
    hx = Grid$hx; hy = Grid$hy; hz = Grid$hz;
    L <- K^(-1)      # 3x8x8 array

    tx = 2 *hy*hz/hx; TX <- zeros(Nx+1,Ny,Nz);
    ty = 2 *hx*hz/hy; TY <- zeros(Nx,Ny+1,Nz);
    tz = 2 *hx*hy/hz; TZ <- zeros(Nx, Ny, (Nz+1));

    TX[2:Nx,,] = tx / (L[1, 1:(Nx-1), ] + L[1, 2:Nx, ])
    TY[,2:Ny,] = ty / (L[2, , 1:Ny-1] + L[2, ,2:Ny])
    TZ[,,1-Nz] = tz / (L[3,, Nz-1] + L[3, , 1-Nz])

    # Assemble TPFA discretization matrix.
    x1 = pracma::Reshape(TX[1:Nx,,],N,1); x2 = pracma::Reshape(TX[2:(Nx+1),,],N,1);
    y1 = pracma::Reshape(TY[,1:Ny,],N,1); y2 = pracma::Reshape(TY[,2:(Ny+1),],N,1);
    z1 = pracma::Reshape(TZ[,,1:Nz],N,1); z2 = pracma::Reshape(TZ[,,2:(Nz+1)],N,1);

    DiagVecs <-  cbind(cbind(-z2,-y2,-x2,x1+x2+y1+y2+z1+z2,-x1,-y1,-z1))
    DiagIndx  <- c(-Nx*Ny,-Nx,-1,0,1,Nx,Nx*Ny)

    # the sparse matrix function m.spdiags returns two components:
    # B and d
    spA <-  m.spdiags(DiagVecs, DiagIndx, N, N)
    A <- spA$B   # get component "B" like in Matlab
    # In original Matlab code sum(Grid.K(:,1,1,1)) means that it will add
    # all the rows in the first column of the first panel
    # sum(Grid.K(:,1,1,1)) = sum(Grid$K[, 1, 1])
    # sum(Grid.K(1,:,1,1)) = sum(Grid$K[1, , 1])
    # sum(Grid.K(1,1,:,1)) = sum(Grid$K[1, 1, ])
    A[1, 1] <-  A[1, 1] + sum(Grid$K[, 1, 1])

    # Solve linear system and extract interface ﬂuxes.
    u = solve(A, q)
    u
}

TPFA09 <- function(Grid, K, q) {
    # Compute transmissibilities by harmonic averaging.
    Nx = Grid$Nx; Ny = Grid$Ny; Nz = Grid$Nz;
    N = Nx * Ny * Nz;
    hx = Grid$hx; hy = Grid$hy; hz = Grid$hz;
    L <- K^(-1)      # 3x8x8 array

    tx = 2 *hy*hz/hx; TX <- zeros(Nx+1,Ny,Nz);
    ty = 2 *hx*hz/hy; TY <- zeros(Nx,Ny+1,Nz);
    tz = 2 *hx*hy/hz; TZ <- zeros(Nx, Ny, (Nz+1));

    TX[2:Nx,,] = tx / (L[1, 1:(Nx-1), ] + L[1, 2:Nx, ])
    TY[,2:Ny,] = ty / (L[2, , 1:Ny-1] + L[2, ,2:Ny])
    TZ[,,1-Nz] = tz / (L[3,, Nz-1] + L[3, , 1-Nz])

    # Assemble TPFA discretization matrix.
    x1 = pracma::Reshape(TX[1:Nx,,],N,1); x2 = pracma::Reshape(TX[2:(Nx+1),,],N,1);
    y1 = pracma::Reshape(TY[,1:Ny,],N,1); y2 = pracma::Reshape(TY[,2:(Ny+1),],N,1);
    z1 = pracma::Reshape(TZ[,,1:Nz],N,1); z2 = pracma::Reshape(TZ[,,2:(Nz+1)],N,1);

    DiagVecs <-  cbind(cbind(-z2,-y2,-x2,x1+x2+y1+y2+z1+z2,-x1,-y1,-z1))
    DiagIndx  <- c(-Nx*Ny,-Nx,-1,0,1,Nx,Nx*Ny)

    # the sparse matrix function m.spdiags returns two components:
    # B and d
    spA <-  m.spdiags(DiagVecs, DiagIndx, N, N)
    A <- spA$B   # get component "B" like in Matlab
    # In original Matlab code sum(Grid.K(:,1,1,1)) means that it will add
    # all the rows in the first column of the first panel
    # sum(Grid.K(:,1,1,1)) = sum(Grid$K[, 1, 1])
    # sum(Grid.K(1,:,1,1)) = sum(Grid$K[1, , 1])
    # sum(Grid.K(1,1,:,1)) = sum(Grid$K[1, 1, ])
    A[1, 1] <-  A[1, 1] + sum(Grid$K[, 1, 1])

    # Solve linear system and extract interface ﬂuxes.
    u = solve(A, q)
    # P: pressure
    # V: fluxes
    # make P an 8x8 matrix as in the original function
    # P <-  matrix(rreshape(u, Nx, Ny, Nz), nrow = Nx, ncol = Ny)
    P <-  rreshape(u, Nx, Ny, Nz)
    Grid$V$x <- zeros(Nx+1, Ny, Nz);
    Grid$V$y <- zeros(Nx, Ny+1, Nz);
    Grid$V$z <- zeros(Nx, Ny, Nz+1);

    list(P = P, V = Grid$V)
}

TPFA10 <- function(Grid, K, q) {
    # Compute transmissibilities by harmonic averaging.
    Nx = Grid$Nx; Ny = Grid$Ny; Nz = Grid$Nz;
    N = Nx * Ny * Nz;
    hx = Grid$hx; hy = Grid$hy; hz = Grid$hz;
    L <- K^(-1)      # 3x8x8 array

    tx = 2 *hy*hz/hx; TX <- zeros(Nx+1,Ny,Nz);
    ty = 2 *hx*hz/hy; TY <- zeros(Nx,Ny+1,Nz);
    tz = 2 *hx*hy/hz; TZ <- zeros(Nx, Ny, (Nz+1));

    TX[2:Nx,,] = tx / (L[1, 1:(Nx-1), ] + L[1, 2:Nx, ])
    TY[,2:Ny,] = ty / (L[2, , 1:Ny-1] + L[2, ,2:Ny])
    TZ[,,1-Nz] = tz / (L[3,, Nz-1] + L[3, , 1-Nz])

    # Assemble TPFA discretization matrix.
    x1 = pracma::Reshape(TX[1:Nx,,],N,1); x2 = pracma::Reshape(TX[2:(Nx+1),,],N,1);
    y1 = pracma::Reshape(TY[,1:Ny,],N,1); y2 = pracma::Reshape(TY[,2:(Ny+1),],N,1);
    z1 = pracma::Reshape(TZ[,,1:Nz],N,1); z2 = pracma::Reshape(TZ[,,2:(Nz+1)],N,1);

    DiagVecs <-  cbind(cbind(-z2,-y2,-x2,x1+x2+y1+y2+z1+z2,-x1,-y1,-z1))
    DiagIndx  <- c(-Nx*Ny,-Nx,-1,0,1,Nx,Nx*Ny)

    # the sparse matrix function m.spdiags returns two components:
    # B and d
    spA <-  m.spdiags(DiagVecs, DiagIndx, N, N)
    A <- spA$B   # get component "B" like in Matlab
    # In original Matlab code sum(Grid.K(:,1,1,1)) means that it will add
    # all the rows in the first column of the first panel
    # sum(Grid.K(:,1,1,1)) = sum(Grid$K[, 1, 1])
    # sum(Grid.K(1,:,1,1)) = sum(Grid$K[1, , 1])
    # sum(Grid.K(1,1,:,1)) = sum(Grid$K[1, 1, ])
    A[1, 1] <-  A[1, 1] + sum(Grid$K[, 1, 1])

    # Solve linear system and extract interface ﬂuxes.
    u = solve(A, q)
    # P: pressure
    # V: fluxes
    # make P an 8x8 matrix as in the original function
    # P <-  matrix(rreshape(u, Nx, Ny, Nz), nrow = Nx, ncol = Ny) # 8x8
    P <- rreshape(u, Nx, Ny, Nz) # 8x8x1
    Grid$V$x <- zeros(Nx+1, Ny, Nz);
    Grid$V$y <- zeros(Nx, Ny+1, Nz);
    Grid$V$z <- zeros(Nx, Ny, Nz+1);

    # V.x(2:Nx ,:,:) = (P(1:Nx-1,:,:)-P(2:Nx,:,:)).*TX(2:Nx,:,:);
    Grid$V$x[2:Nx,,] = (P[1:Nx-1,, ] - P[2:Nx, ,]) * TX[2:Nx,,]
    # V.y (:,2:Ny,:) = (P(:,1:Ny-1,:)-P(:,2:Ny,:)).*TY(:,2:Ny,:);
    Grid$V$y[,2:Ny,] = (P[,1:Ny-1,] - P[,2:Ny,]) * TY[,2:Ny,]
    # V.z (:,:, 2:Nz) = (P (:,:, 1:Nz-1)-P(:,:, 2:Nz)).*TZ(:,:,2:Nz);
    # replace 2:Nz by 1-Nz
    Grid$V$z[,, 1-Nz] = (P[, , 1:Nz-1] - P[,, 1-Nz]) * TZ[,, 1-Nz]
    Grid$V
}

TPFA11 <- function(Grid, K, q) {
    # Compute transmissibilities by harmonic averaging.
    Nx = Grid$Nx; Ny = Grid$Ny; Nz = Grid$Nz;
    N = Nx * Ny * Nz;
    hx = Grid$hx; hy = Grid$hy; hz = Grid$hz;
    L <- K^(-1)      # 3x8x8 array

    tx = 2 *hy*hz/hx; TX <- zeros(Nx+1,Ny,Nz);
    ty = 2 *hx*hz/hy; TY <- zeros(Nx,Ny+1,Nz);
    tz = 2 *hx*hy/hz; TZ <- zeros(Nx, Ny, (Nz+1));

    TX[2:Nx,,] = tx / (L[1, 1:(Nx-1), ] + L[1, 2:Nx, ])
    TY[,2:Ny,] = ty / (L[2, , 1:Ny-1] + L[2, ,2:Ny])
    TZ[,,1-Nz] = tz / (L[3,, Nz-1] + L[3, , 1-Nz])

    # Assemble TPFA discretization matrix.
    x1 = pracma::Reshape(TX[1:Nx,,],N,1); x2 = pracma::Reshape(TX[2:(Nx+1),,],N,1);
    y1 = pracma::Reshape(TY[,1:Ny,],N,1); y2 = pracma::Reshape(TY[,2:(Ny+1),],N,1);
    z1 = pracma::Reshape(TZ[,,1:Nz],N,1); z2 = pracma::Reshape(TZ[,,2:(Nz+1)],N,1);

    DiagVecs <-  cbind(cbind(-z2,-y2,-x2,x1+x2+y1+y2+z1+z2,-x1,-y1,-z1))
    DiagIndx  <- c(-Nx*Ny,-Nx,-1,0,1,Nx,Nx*Ny)

    # the sparse matrix function m.spdiags returns two components:
    # B and d
    spA <-  m.spdiags(DiagVecs, DiagIndx, N, N)
    A <- spA$B   # get component "B" like in Matlab
    # In original Matlab code sum(Grid.K(:,1,1,1)) means that it will add
    # all the rows in the first column of the first panel
    # sum(Grid.K(:,1,1,1)) = sum(Grid$K[, 1, 1])
    # sum(Grid.K(1,:,1,1)) = sum(Grid$K[1, , 1])
    # sum(Grid.K(1,1,:,1)) = sum(Grid$K[1, 1, ])
    A[1, 1] <-  A[1, 1] + sum(Grid$K[, 1, 1])

    # Solve linear system and extract interface ﬂuxes.
    u = solve(A, q)
    # P: pressure
    # V: fluxes
    # make P an 8x8 matrix as in the original function
    # P <-  matrix(rreshape(u, Nx, Ny, Nz), nrow = Nx, ncol = Ny) # 8x8
    P <- rreshape(u, Nx, Ny, Nz) # 8x8x1
    Grid$V$x <- zeros(Nx+1, Ny, Nz);
    Grid$V$y <- zeros(Nx, Ny+1, Nz);
    Grid$V$z <- zeros(Nx, Ny, Nz+1);

    # V.x(2:Nx ,:,:) = (P(1:Nx-1,:,:)-P(2:Nx,:,:)).*TX(2:Nx,:,:);
    # V.y (:,2:Ny,:) = (P(:,1:Ny-1,:)-P(:,2:Ny,:)).*TY(:,2:Ny,:);
    # V.z (:,:, 2:Nz) = (P (:,:, 1:Nz-1)-P(:,:, 2:Nz)).*TZ(:,:,2:Nz);
    Grid$V$x[2:Nx,,]  = (P[1:Nx-1,, ] - P[2:Nx, ,]) * TX[2:Nx,,]
    Grid$V$y[,2:Ny,]  = (P[,1:Ny-1,] - P[,2:Ny,]) * TY[,2:Ny,]
    Grid$V$z[,, 1-Nz] = (P[, , 1:Nz-1] - P[,, 1-Nz]) * TZ[,, 1-Nz] # 2:Nz by 1-Nz

    P
}
