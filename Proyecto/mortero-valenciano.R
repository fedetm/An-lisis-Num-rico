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
        xlim = c(-1, 1), ylim = c(-1, 1), zlim = c(-1.5, 2),
        polygon_offset = 1)
persp3d(x, y, z-1.5, front = "lines", back = "lines",
        xlim = c(-1, 1), ylim = c(-1, 1), zlim = c(-1.5, 2),
        lit = FALSE, add = TRUE)

# Dibuja la primera oreja del mortero valenciano
## BEZIER CURVES ##
## SPECIFY PARAMETRIC VALUES FROM 0 TO 1 FOR SAMLPING A BEZIER CURVE
t <- seq(0, 1, length=100)
## BEZIER CONTROL POINTS
p1 <- matrix(c(0.94,0.25,1.95, 1.2,0.2,1.95, 1.3,0,1.95, 1.2,-0.2,1.95, 0.94,-0.25,1.95), nrow=5, ncol=3, byrow=TRUE)
## PLOT A BEZIER CURVE
plot3d(bezier_points <- bezier(t=t, p=p1[1:3, ]), col = "#8A0808", size = 6, add = TRUE)
## BEZIER CONTROL POINTS
p2 <- matrix(c(0.94,-0.25,1.95, 1.2,-0.2,1.95, 1.3,0,1.95, 1.2,0.2,1.95, 0.94,0.25,1.95), nrow=5, ncol=3, byrow=TRUE)
## PLOT A BEZIER CURVE
plot3d(bezier_points <- bezier(t=t, p=p2[1:3, ]), col = "#8A0808", size = 6, add = TRUE)
## BEZIER CONTROL POINTS
p3 <- matrix(c(0.9,0,1, 1.25,0,1.35, 1.3,0,1.95, 1.25,0,1.35, 0.9,0,1.3), nrow=5, ncol=3, byrow=TRUE)
## PLOT A BEZIER CURVE
plot3d(bezier_points <- bezier(t=t, p=p3[1:3, ]), col = "#8A0808", size = 6, add = TRUE)

# Dibuja la segunda oreja del mortero valenciano
## BEZIER CONTROL POINTS
p1 <- matrix(c(-0.94,0.25,1.95, -1.2,0.2,1.95, -1.3,0,1.95, -1.2,-0.2,1.95, 0.94,-0.25,1.95), nrow=5, ncol=3, byrow=TRUE)
## PLOT A BEZIER CURVE
plot3d(bezier_points <- bezier(t=t, p=p1[1:3, ]), col = "#8A0808", size = 6, add = TRUE)
## BEZIER CONTROL POINTS
p2 <- matrix(c(-0.94,-0.25,1.95, -1.2,-0.2,1.95, -1.3,0,1.95, -1.2,-0.2,1.95, -0.94,-0.25,1.95), nrow=5, ncol=3, byrow=TRUE)
## PLOT A BEZIER CURVE
plot3d(bezier_points <- bezier(t=t, p=p2[1:3, ]), col = "#8A0808", size = 6, add = TRUE)
## BEZIER CONTROL POINTS
p3 <- matrix(c(-0.9,0,1, -1.25,0,1.35, -1.3,0,1.95, -1.25,0,1.35, -0.9,0,1.3), nrow=5, ncol=3, byrow=TRUE)
## PLOT A BEZIER CURVE
plot3d(bezier_points <- bezier(t=t, p=p3[1:3, ]), col = "#8A0808", size = 6, add = TRUE)

# Dibuja la tercera oreja del mortero valenciano
## BEZIER CONTROL POINTS
p1 <- matrix(c(0.25,0.94,1.95, 0.2,1.2,1.95, 0,1.3,1.95, 0.2,-1.2,1.95, 0.25,-0.94,1.95), nrow=5, ncol=3, byrow=TRUE)
## PLOT A BEZIER CURVE
plot3d(bezier_points <- bezier(t=t, p=p1[1:3, ]), col = "#8A0808", size = 6, add = TRUE)
## BEZIER CONTROL POINTS
p2 <- matrix(c(-0.25,0.94,1.95, -0.2,1.2,1.95, 0,1.3,1.95, 0.2,1.2,1.95, 0.25,0.94,1.95), nrow=5, ncol=3, byrow=TRUE)
## PLOT A BEZIER CURVE
plot3d(bezier_points <- bezier(t=t, p=p2[1:3, ]), col = "#8A0808", size = 6, add = TRUE)
## BEZIER CONTROL POINTS
p3 <- matrix(c(0,0.9,1, 0,1.25,1.35, 0,1.3,1.95, 0,1.25,1.35, 0,0.9,1.3), nrow=5, ncol=3, byrow=TRUE)
## PLOT A BEZIER CURVE
plot3d(bezier_points <- bezier(t=t, p=p3[1:3, ]), col = "#8A0808", size = 6, add = TRUE)

# Dibuja la cuarta oreja del mortero valenciano
## BEZIER CONTROL POINTS
p1 <- matrix(c(0.25,-0.94,1.95, 0.2,-1.2,1.95, 0,-1.3,1.95, 0.2,1.2,1.95, 0.25,0.94,1.95), nrow=5, ncol=3, byrow=TRUE)
## PLOT A BEZIER CURVE
plot3d(bezier_points <- bezier(t=t, p=p1[1:3, ]), col = "#8A0808", size = 6, add = TRUE)
## BEZIER CONTROL POINTS
p2 <- matrix(c(-0.25,-0.94,1.95, -0.2,-1.2,1.95, 0,-1.3,1.95, 0.2,-1.2,1.95, 0.25,-0.94,1.95), nrow=5, ncol=3, byrow=TRUE)
## PLOT A BEZIER CURVE
plot3d(bezier_points <- bezier(t=t, p=p2[1:3, ]), col = "#8A0808", size = 6, add = TRUE)
## BEZIER CONTROL POINTS
p3 <- matrix(c(0,-0.9,1, 0,-1.25,1.35, 0,-1.3,1.95, 0,-1.25,1.35, 0,-0.9,1.3), nrow=5, ncol=3, byrow=TRUE)
## PLOT A BEZIER CURVE
plot3d(bezier_points <- bezier(t=t, p=p3[1:3, ]), col = "#8A0808", size = 6, add = TRUE)

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
  area = halfsphere.area(1) + 4 * pyramid.area(0.5)
  
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
  volume = pyramid.volume(0.5) * 4 + halfsphere.volume(1)
  
  volume
}

mortero.volume()