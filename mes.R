input <- readline(prompt = "n: ")
n <- as.integer(input)
integrate <- function(f, a, b){
  return((b-a)/2*(f((b-a)/2*(1/sqrt(3)) + (b+a)/2) + f((b-a)/2*(-1/sqrt(3)) + (b+a)/2)))
}

e <- function(x, i){
  h <- 3/n
  x_i1 <- h * (i-1)
  x_i2 <- h * i
  x_i3 <- h * (i+1)
  if(x > x_i1 && x < x_i2){
    return((x-x_i1)/h)
  }
  else if(x >= x_i2 && x <= x_i3){
    return((x_i3-x)/h)
  }
  else{
    return(0)
  }
}


e_prim <- function(x, i){
  h <- 3/n
  x_i1 <- h * (i-1)
  x_i2 <- h * i
  x_i3 <- h * (i+1)
  if(x > x_i1 && x <= x_i2){
    return(1/h)
  }
  else if(x > x_i2 && x <= x_i3){
    return(-1/h)
  }
  else{
    return(0)
  }
}

e_r <- function(x){
  if (x <= 1 && x >= 0){
    return(10)
  }
  else if (x <= 2 && x > 1){
    return(5)
  }
  else {
    return(1)
  }
}

B <- function(i,j){
  h = 3/n
  a = max(0,(i-1)*h,(j-1)*h)
  b = min((i+1)*h,(j+1)*h,3)
  
  return(e(0,i)*e(0,j) - integrate(function(x) e_prim(x,i)*e_prim(x,j),a,b))
}

L <- function(i){
  return(5*e(0,i) + integrate(function(x) -1/e_r(x) * e(x,i),0,3))
}

fem <- function(){
  M <- matrix(0,nrow=n,ncol=n)
  for (i in 1:n){
    for (j in 1:n){
      M[i,j] <- B(i-1,j-1)
    }
  }
  L_prim <- vector()
  for (i in 1:n){
    L_prim = c(L_prim,L(i-1) - 2*B(n,i-1))
  }
  w <- solve(M,L_prim)
  res <- function(x,v=w){
    r = 0
    for (i in 1:n){
      r = r + v[i]*e(x,i-1)
    }
    r = r + 2 * e(x,n)
    return (r)
  }
  return(res)
}

plot_result <- function(){
  u = fem()
  plot(seq(0, 3, 1/(100*n)), 
       mapply(u, seq(0, 3, 1/(100*n))),
       main = 'rozwiązanie równania',
       xlab=' ',
       ylab=' ',
       type='l')
}

plot_result()




