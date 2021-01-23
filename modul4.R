library(dslabs)
data(murders)
pop<-(murders$population)
#1
pop <- murders$population
sort(pop)
#2
index <- order(pop, decreasing = TRUE)
head(index,1)
#3
popmin<-which.min(murders$population)
#4
murders$state[popmin]
#5
ranks<-rank(pop)
ranks
my_df<-data.frame(ranking=ranks, population=pop)
#6
ind<-order(my_df$ranking)
ind
#7
population_in_millions <- murders$population/10^6 
total_gun_murders <- murders$total 
totalgun<-log(total_gun_murders)
plot(population_in_millions,totalgun)

#8

pop_hist<-with(murders, pop/10000)
hist(pop_hist)

regionn<-murders$region
boxplot(pop, data=regionn)

