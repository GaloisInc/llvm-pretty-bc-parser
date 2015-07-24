 
define half @f(double %x) {
       %y = fptrunc double %x to half
       ret half %y
}
