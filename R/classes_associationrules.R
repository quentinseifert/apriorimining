# R-script containing the class "associationrules" and its generics and methods


#### class

setClass("associationrules",
         slots = list(
           antecedent = "ngCMatrix",
           consequent = "ngCMatrix",
           measurements = "matrix"
         ))



#### generics

#### methodes