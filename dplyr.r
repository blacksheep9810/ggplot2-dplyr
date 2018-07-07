library(dplyr)
library(hflights)
hflights
data(hflights)
head(hflights)

#convert to local data frame 
flights <- tbl_df(hflights)
# specify that you want to see more rows 
print(flights, n = 30)
# convert to a normal data frame to see all columns 
data.frame(head(flights))

# R approach to view all flights on Jan 1 
flights[flights$Month == 1 & flights$DayOfWeek == 1,]

# dplyr approach 
filter(flights, Month == 1, DayofMonth == 1)

#use pipe for OR condition 
updated <- filter(flights, UniqueCarrier == "AA" | UniqueCarrier == "UA")
print(updated, n = 50)

#can use %in% operator 
filter(flights, UniqueCarrier %in% c("AA", "UA"))

# R approach to select DepTime, ArrTime, and FlightNum columns 
flights[c("DepTime", "ArrTime", "FlightNum")]

#dplyr approach 
select(flights, DepTime, ArrTime, FlightNum)

select(flights, Year:DayofMonth, contains("Taxi"), contains("Delay"))

#chaining or pipelining 
# %>% (can be pronouced as then ) - R approach 
filter(select(flights, UniqueCarrier, DepDelay), DepDelay > 60)

#chaining method 
flights %>%
  select(UniqueCarrier, DepDelay) %>%
  filter(DepDelay > 150)

#arrange: Reorder rows 
flights[order(flights$DepDelay), c("UniqueCarrier", "DepDelay")]

#dplyr approach 
flights %>%
  select("UniqueCarrier", "DepDelay") %>%
  arrange(UniqueCarrier)

#mutate: Add new variables 
#Using R approach to create a new variable speed
flights$Speed <- flights$Distance / flights$AirTime * 60
speed1 <- flights[, c("Distance", "AirTime", "Speed")]
speed1

#dplyr approach 
new1 <- flights %>%
  select (Distance, AirTime) %>%
  mutate(Speed = Distance / AirTime * 120)

new1

#dplyr approach: summarise each group by calculating the mean of ArrDelay 
flights %>% 
  group_by(Dest) %>%
  summarise(avg_delay = mean(ArrDelay, na.rm = TRUE))

#summarise_each, for each carrier, calculate the percentage of flights cancelled or diverted 
flights %>%
  group_by(UniqueCarrier) %>%
  summarise_each(funs(mean), Cancelled, Diverted)

#For each carrier, calculate min and max arrival and department delays 
flights %>%
  group_by(UniqueCarrier) %>%
  summarise_each(funs(min(.,na.rm=TRUE), max(.,na.rm=TRUE)), matches('delay'))

#n() -> counts the number of rows in a group 
flights %>%
  group_by(Month, DayofMonth) %>%
  summarise(flight_count= n()) %>%
  arrange(desc(flight_count))

#rewrite in a simpler way with tally()
flights %>%
  group_by(Month, DayofMonth) %>%
  tally(sort=TRUE)

#count the number of total flights and distinct plane numbers 
#n_distrinct() -> count number of distinct rows in a group 
flights %>%
  group_by(Dest) %>%
  summarise(flight_count = n(), plane_count = n_distinct(TailNum))

#grouping is useful without summarising 
flights %>%
  group_by(Dest) %>%
  select(Cancelled) %>%
  table() %>%
  head()

#using min_rank and filter 
flights %>%
  group_by(UniqueCarrier) %>%
  select(Month, DayofMonth, DepDelay) %>%
  filter(min_rank(desc(DepDelay)) <=2) %>%
  arrange(UniqueCarrier, desc(DepDelay))

#rewrite with top_n function (simpler)
flights %>%
  group_by(UniqueCarrier) %>%
  select(Month, DayofMonth, DepDelay) %>%
  top_n(4) %>%
  arrange(UniqueCarrier, desc(DepDelay))

#calculate change in total number of flights compared to previous month 
flights %>%
  group_by(Month) %>%
  summarise(flight_count = n()) %>%
  mutate(change = flight_count - lag(flight_count))

#^ using tally()
flights %>%
  group_by(Month) %>%
  tally() %>%
  mutate(change = n - lag(n))

#Other useful functions 
#randomly sample fixed number of rows, without replacement 
flights %>% sample_n(5)
#sample a fraction of rows, with replacement 
flights %>% sample_frac(0.25, replace = TRUE)

#Use R approach to view structure of object 
str(flights)
#Using dplyr approach 
glimpse(flights)

