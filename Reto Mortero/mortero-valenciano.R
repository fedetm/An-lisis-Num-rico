open3d()
bg3d("white")
material3d(col = "black")

x <- seq(-1, 1, length = 30)
y <- x
f <- function(x, y) { r <- 2*x^2 + 2*y^2; r^2 }
z <- outer(x, y, f)
z[is.na(z)] <- 1
persp3d(x, y, z-1.5, aspect = c(1, 1, 0.5), col = "red",
        xlab = "X", ylab = "Y", zlab = "z", 
        xlim = c(-1, 1), ylim = c(-1, 1), zlim = c(-1.5, -0.5),
        polygon_offset = 1)
persp3d(x, y, z-1.5, front = "lines", back = "lines",
        xlim = c(-1, 1), ylim = c(-1, 1), zlim = c(-1.5, -0.5),
        lit = FALSE, add = TRUE)

## BEZIER CURVES ##
## SPECIFY PARAMETRIC VALUES FROM 0 TO 1 FOR SAMLPING A BEZIER CURVE
t <- seq(0, 1, length=100)

## PRIMERA PUNTA DEL MORTERO VALENCIANO
xi <- 0
yi <- 0
zi <- 0
for(p in 1:25) {
  ## BEZIER CONTROL POINTS
  p1 <- matrix(c(0.68,0.2-yi,-0.5-zi, 0.9-xi,0.15,-0.5-zi, 1-xi,0,-0.5-zi, 0.9-xi,-0.15,-0.5-zi, 0.68-xi,-0.2-yi,-0.5-zi), nrow=5, ncol=3, byrow=TRUE)
  ## PLOT A BEZIER CURVE
  plot3d(bezier_points <- bezier(t=t, p=p1[1:3, ]), col = "#8A0808", size = 1, add = TRUE)
  ## VARIABLE INCREASE
  if(p>12) {
    xi <- xi + 0.015
  } else {
    xi <- xi + 0.01
  }
  yi <- yi + 0.005
  zi <- zi + 0.01
}

xi <- 0
yi <- 0
zi <- 0
for(p in 1:25) {
  ## BEZIER CONTROL POINTS
  p2 <- matrix(c(0.68,-0.2+yi,-0.5-zi, 0.9-xi,-0.15,-0.5-zi, 1-xi,0,-0.5-zi, 0.9-xi,0.15,-0.5-zi, 0.68-xi,0.2+yi,-0.5-zi), nrow=5, ncol=3, byrow=TRUE)
  ## PLOT A BEZIER CURVE
  plot3d(bezier_points <- bezier(t=t, p=p2[1:3, ]), col = "#8A0808", size = 1, add = TRUE)
  ## VARIABLE INCREASE
  if(p>12) {
    xi <- xi + 0.015
  } else {
    xi <- xi + 0.01
  }
  yi <- yi + 0.005
  zi <- zi + 0.01
}
## BEZIER CONTROL POINTS
p3 <- matrix(c(0.68,0,-0.75, 0.9,0,-0.65, 1,0,-0.5, 0.9,0,-0.65, 0.68,0,-0.75), nrow=5, ncol=3, byrow=TRUE)
## PLOT A BEZIER CURVE
plot3d(bezier_points <- bezier(t=t, p=p3[1:3, ]), col = "#8A0808", size = 1, add = TRUE)

## Segunda oreja del mortero valenciano
xi <- 0
yi <- 0
zi <- 0
for(p in 1:25) {
  ## BEZIER CONTROL POINTS
  p1 <- matrix(c(-0.68,0.2-yi,-0.5-zi, -0.9+xi,0.15,-0.5-zi, -1+xi,0,-0.5-zi, -0.9+xi,-0.15,-0.5-zi, -0.68+xi,-0.2-yi,-0.5-zi), nrow=5, ncol=3, byrow=TRUE)
  ## PLOT A BEZIER CURVE
  plot3d(bezier_points <- bezier(t=t, p=p1[1:3, ]), col = "#8A0808", size = 1, add = TRUE)
  ## VARIABLE INCREASE
  if(p>12) {
    xi <- xi + 0.015
  } else {
    xi <- xi + 0.01
  }
  yi <- yi + 0.005
  zi <- zi + 0.01
}

xi <- 0
yi <- 0
zi <- 0
for(p in 1:25) {
  ## BEZIER CONTROL POINTS
  p2 <- matrix(c(-0.68,-0.2+yi,-0.5-zi, -0.9+xi,-0.15,-0.5-zi, -1+xi,0,-0.5-zi, -0.9+xi,0.15,-0.5-zi, -0.68+xi,0.2+yi,-0.5-zi), nrow=5, ncol=3, byrow=TRUE)
  ## PLOT A BEZIER CURVE
  plot3d(bezier_points <- bezier(t=t, p=p2[1:3, ]), col = "#8A0808", size = 1, add = TRUE)
  ## VARIABLE INCREASE
  if(p>12) {
    xi <- xi + 0.015
  } else {
    xi <- xi + 0.01
  }
  yi <- yi + 0.005
  zi <- zi + 0.01
}
## BEZIER CONTROL POINTS
p3 <- matrix(c(-0.68,0,-0.75, -0.9,0,-0.65, -1,0,-0.5, -0.9,0,-0.65, -0.68,0,-0.75), nrow=5, ncol=3, byrow=TRUE)
## PLOT A BEZIER CURVE
plot3d(bezier_points <- bezier(t=t, p=p3[1:3, ]), col = "#8A0808", size = 1, add = TRUE)

## Tercera oreja del mortero valenciano
xi <- 0
yi <- 0
zi <- 0
for(p in 1:25) {
  ## BEZIER CONTROL POINTS
  p1 <- matrix(c(0.2-yi,0.68,-0.5-zi,0.15, 0.9-xi,-0.5-zi, 0,1-xi,-0.5-zi, -0.15,0.9-xi,-0.5-zi, -0.2-yi,0.68-xi,-0.5-zi), nrow=5, ncol=3, byrow=TRUE)
  ## PLOT A BEZIER CURVE
  plot3d(bezier_points <- bezier(t=t, p=p1[1:3, ]), col = "#8A0808", size = 1, add = TRUE)
  ## VARIABLE INCREASE
  if(p>12) {
    xi <- xi + 0.015
  } else {
    xi <- xi + 0.01
  }
  yi <- yi + 0.005
  zi <- zi + 0.01
}

xi <- 0
yi <- 0
zi <- 0
for(p in 1:25) {
  ## BEZIER CONTROL POINTS
  p2 <- matrix(c(-0.2+yi,0.68,-0.5-zi, -0.15,0.9-xi,-0.5-zi, 0,1-xi,-0.5-zi, 0.15,0.9-xi,-0.5-zi, 0.2+yi,0.68-xi,-0.5-zi), nrow=5, ncol=3, byrow=TRUE)
  ## PLOT A BEZIER CURVE
  plot3d(bezier_points <- bezier(t=t, p=p2[1:3, ]), col = "#8A0808", size = 1, add = TRUE)
  ## VARIABLE INCREASE
  if(p>12) {
    xi <- xi + 0.015
  } else {
    xi <- xi + 0.01
  }
  yi <- yi + 0.005
  zi <- zi + 0.01
}

## BEZIER CONTROL POINTS
p3 <- matrix(c(0,0.68,-0.75, 0,0.9,-0.65, 0,1,-0.5, 0,0.9,-0.65, 0,0.68,-0.75), nrow=5, ncol=3, byrow=TRUE)
## PLOT A BEZIER CURVE
plot3d(bezier_points <- bezier(t=t, p=p3[1:3, ]), col = "#8A0808", size = 1, add = TRUE)

## Cuarta oreja del mortero valenciano
xi <- 0
yi <- 0
zi <- 0
for(p in 1:25) {
  ## BEZIER CONTROL POINTS
  p1 <- matrix(c(0.2-yi,-0.68,-0.5-zi, 0.15,-0.9+xi,-0.5-zi, 0,-1+xi,-0.5-zi, -0.15,-0.9+xi,-0.5-zi, -0.2-yi,-0.68+xi,-0.5-zi), nrow=5, ncol=3, byrow=TRUE)
  ## PLOT A BEZIER CURVE
  plot3d(bezier_points <- bezier(t=t, p=p1[1:3, ]), col = "#8A0808", size = 1, add = TRUE)
  ## VARIABLE INCREASE
  if(p>12) {
    xi <- xi + 0.015
  } else {
    xi <- xi + 0.01
  }
  yi <- yi + 0.005
  zi <- zi + 0.01
}

xi <- 0
yi <- 0
zi <- 0
for(p in 1:25) {
  ## BEZIER CONTROL POINTS
  p2 <- matrix(c(-0.2+yi,-0.68,-0.5-zi, -0.15,-0.9+xi,-0.5-zi, 0,-1+xi,-0.5-zi, 0.15,-0.9+xi,-0.5-zi, 0.2+yi,-0.68+xi,-0.5-zi), nrow=5, ncol=3, byrow=TRUE)
  ## PLOT A BEZIER CURVE
  plot3d(bezier_points <- bezier(t=t, p=p2[1:3, ]), col = "#8A0808", size = 1, add = TRUE)
  ## VARIABLE INCREASE
  if(p>12) {
    xi <- xi + 0.015
  } else {
    xi <- xi + 0.01
  }
  yi <- yi + 0.005
  zi <- zi + 0.01
}

## BEZIER CONTROL POINTS
p3 <- matrix(c(0,-0.68,-0.75, 0,-0.9,-0.65, 0,-1,-0.5, 0,-0.9,-0.65, 0,-0.68,-0.75), nrow=5, ncol=3, byrow=TRUE)
## PLOT A BEZIER CURVE
plot3d(bezier_points <- bezier(t=t, p=p3[1:3, ]), col = "#8A0808", size = 1, add = TRUE)

halfsphere.area = function(r)
{
  h = r
  
  area = 2 * pi * r * h
  
  area
}

pyramid.area = function(l)
{
  area = (l*l/2)*3
  
  area
}

mortero.area = function() 
{
  area = halfsphere.area(1.00) + 4 * pyramid.area(0.30)
  
  area
}

mortero.area()

halfsphere.volume = function(r)
{
  volume = (4/3 * pi * r^3)/2
  
  volume
}

pyramid.volume = function(l) 
{
  h = sqrt(l^2+(l/2)^2)
  b = l*l/2
  volume = 1/3 * b * h
  
  volume
}

mortero.volume = function()
{
  volume = (pyramid.volume(0.30) -  pyramid.volume(0.29)) * 4 + (halfsphere.volume(1.00) - halfsphere.volume(0.99))
  
  volume
}

mortero.volume()