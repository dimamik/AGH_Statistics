data_c = (read.csv("D:\\STUDIA_2020_2021\\Statystyka\\AGH_Statistics\\Lab_1 Project\\Wzrost_i_Waga.csv", header = TRUE))
wzrost = data_c[,1]
waga = data_c[,2]


#Wzrost
mean(wzrost)
median(wzrost)

avg.dev <- function(x) mean(abs(x - mean(x)))
c(avg.dev(wzrost))

fivenum(wzrost)

hist(wzrost)

boxplot(wzrost)

#Waga
mean(waga)
median(waga)

avg.dev <- function(x) mean(abs(x - mean(x)))
c(avg.dev(waga))

fivenum(waga)

hist(waga)

boxplot(waga)



