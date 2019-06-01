R1 <- function(x,z) {
  tmp <- min(x,z)
  tmp+exp(-x)+exp(-z)-exp(tmp-x)-exp(tmp-z)-exp(-(x+z))/2+exp(2*tmp-x-z)/2
}



R1 <- function(x,z) {
  tmp <- min(x,z)
  -(tmp)^3/6+x*z*tmp/2-abs(x-z)-sin(x)-sin(z)+x*cos(z)+z*cos(x)+tmp*cos(z-x)/2+5*sin(abs(x-z))/4-sin(x+z)/4
}

q <- matrix(NA,5,5)
for (i in 1:5) {
for (j in 1:5) {
  q[i,j] <- R1(x[i],x[j])
}}

static double
rk_linP(double x, double y){
   double val, tmp;
   tmp=(x+y-fabs(x-y))/2.0;
   val= -tmp*tmp*tmp/6.0 + x*y*tmp/2.0 - fabs(x-y);
   val += -sin(x) - sin(y) + x*cos(y) + y*cos(x);
   val += tmp*cos(y-x)/2.0 + 1.25*sin(fabs(x-y)) - 0.25*sin(x+y);
   return(val);
 }


