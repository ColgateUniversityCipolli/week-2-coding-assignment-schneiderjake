integrand <- function(x){
  f <- 7- 2 *x^2
  return(f)
}

a <- 0
b <- 2
n.rect <- 100
(delta.x <- (b-a)/n.rect)

left.points <- a + 0:99*(delta.x)

right.points <- a + 1:100*(delta.x)

mid.points <- (left.points+right.points)/2


(left.area <- sum(delta.x*(integrand(left.points))))

(right.area <- sum(delta.x*(integrand(right.points))))

(mid.area <- sum(delta.x*(integrand(mid.points))))

(trap.area <- (delta.x / 2) * (integrand(a) + 2 * sum(integrand(left.points[-1])) + integrand(b)))


 

