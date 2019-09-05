# R-script containing the class "associationrules" and its generics and methods


#### class

setClass("associationrules",
         slots = list(
           items = "character",
           antecedent = "ngCMatrix",
           consequent = "ngCMatrix",
           measurements = "matrix"
         ))



#### generics

#### methodes


setMethod("show",
          "associationrules",
          function (object) {
            cat(nrow(object@antecedant),"rules can be generated from the given data and the respective parameters.")
          })








