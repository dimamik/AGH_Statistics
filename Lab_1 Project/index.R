data_c = (read.csv("./Wzrost_i_Waga.csv", header = TRUE))

wzrost = data_c[,1]
#Wzrost

#Srednia
mean(wzrost)
#Median
median(wzrost)
#Odchylenie przecietne
avg.dev <- function(x) mean(abs(x - mean(x)))
c(avg.dev(wzrost))
# (minimum, lower-hinge, median, upper-hinge, maximum)
fivenum(wzrost)
# Histograma
hist(wzrost)  
# Boxplot :)
boxplot(wzrost)

#Waga
waga = data_c[,2]
#Srednia
mean(waga)
#Median
median(waga)
#Odchylenie przecietne
avg.dev <- function(x) mean(abs(x - mean(x)))
c(avg.dev(waga))
# (minimum, lower-hinge, median, upper-hinge, maximum)
fivenum(waga)
# Histograma
hist(waga)  
# Boxplot :)
boxplot(waga)



