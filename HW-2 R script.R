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


riemann.sums <- function(fnct,                        # function to integrate
                         a,                           # lower bound of integral
                         b,                           # upper bound of integral
                         n.rect,                      # number of  bound of integral
                         method = "Trapezoidial"){    # method to use (trap by default)
  ######################################
  # Check Input
  ######################################
  if(!is.numeric(a)){ # if a is not numeric
    stop("The lower bound of the integral (a) must be numeric.")
  }
  if(!is.numeric(b)){ # if b is not numeric
    stop("The lower bound of the integral (a) must be numeric.")
  }
  if(!(is.numeric(n.rect)) | (n.rect%%1!=0)){ # if n.rect is not a whole number
    stop("The number of rectangles must be a positive whole number.")
  }
  ######################################
  # Compute Area
  ######################################
  (delta.x <- (b-a)/n.rect)
  
  left.points <- a + 0:(n.rect -1)*(delta.x)
  
  right.points <- a + 1:n.rect * (delta.x)
  
  mid.points <- (left.points+right.points)/2
  
  if(method == "Left"){
    area <- sum(delta.x*fnct(left.points))
  }else if(method == "Right"){
    area <- sum(delta.x*fnct(right.points))
  }else if(method == "Midpoint"){
    area <- sum(delta.x*fnct(mid.points))
  }else if(method == "Trapezoidial"){
    area <- (delta.x / 2) * (fnct(a) + 2 * sum(fnct(left.points[-1])) + fnct(b))
  }else{
    stop("Please select a valid method (e.g., 'Left', 'Right', 'Midpoint', 'Trapezoidial')")
  }
  ######################################
  # Return the area
  ######################################
  return(area)
}
######################################
# Test the function
######################################
riemann.sums(fnct = integrand,
             a = 0,
             b = 2,
             n.rect = 100)


