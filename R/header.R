.onAttach <- function(...) {

   mydate <- date()
   x <- regexpr("[0-9]{4}", mydate)
   this.year <- substr(mydate, x[1], x[1] + attr(x, "match.length") - 1)

   packageStartupMessage("\n## This package supports the book Analyzing Spatial Models of Choice and Judgment with R") 
   #packageStartupMessage("## Copyright 2013 - ", this.year)
   packageStartupMessage("## Authors: David A. Armstrong, II, Ryan Bakker, Royce Carroll, Christopher Hare, Keith T. Poole, Howard Rosenthal (2020)")
   packageStartupMessage("## Pacakge creator: David A. Armstrong")
   packageStartupMessage("## Pacakge contributors: David A. Armstrong (https://github.com/davidaarmstrong/asmcjr), Yen-Chieh Liao (https://github.com/yl17124/asmcjr)")

}

.onUnload <- function(libpath) {
    library.dynam.unload("asmcjr", libpath)
}


