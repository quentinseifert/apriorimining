# R-script containing the class "transaction" and its generics and methods


#### class

setClass("transactions",
         slots = list(
           data = "ngCMatrix",
           items = "character"
         ),
         validity = function(object) {
           if (is.character(object) == TRUE) {
            
              cat("Basic data type needs to be numeric or logical.")
           }
           
           if (ncol(object@data) != length(object@items)) {
             
             cat("ncol(data) and length(items) do not match.")
           }
           
         })

#### generics

setGeneric("show.items",
           function (object) {
           standardGeneric("show.items")
           }
           )

#### methodes

setMethod("show.items",
          "transctions",
          function (object) {
            
          } )





