# presentation 
library("arules")
data("Groceries")
bsp<-as(Groceries,"matrix")
bsp[1:15,1:15]*1 # fenster groß genug machen damit man es sehen kann

# this is the way that the transaction matrix looks like,
# if we look in line 12 for example the person bought citrus fruit and tropical fruit 


# okay if we have the data we are inserting it into the main function which is generating
# or mining the assoction rules called apriorimining


# the main function consists of three functions : 


apriorimining <- function(input, support, confidence) {
  
  #
  a <- create_purchasematrix(input)
  
  #
  a <- freq_items(purchase = a, supp = support)

  #
  a <- rules(a, support, confidence)
  
  
  return(a)
}



# first step function for creating object of class purchase 
# wir brauchen für die class purchase noch einen slot wo der vom benutzer eingegebene support mitgenommen wird
# schaut man in die funktion " rareitems.mean(x) und rareitems.absolut(x)" erkennt man 
#das dort noch der support händisch eingetragen ist dieser muss jedoch mit @support rausgegriffen werden 
#können aus der klasse

x <- create_purchasematrix(bsp)

show(x)
summary(x)
rareitems.mean(x)
rareitems.absolut(x)
plot(x)

# second step function for generating the frequent itemsets

y <- freq_items(x,0.01)
show(y)
summary(y)
plot(y)

# third step function for generating the rules

z <- apriorimining(bsp, 0.01, 0.3)

show(z)
summary(z)


##############################################



