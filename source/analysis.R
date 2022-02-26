#load data
incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
library(dplyr)
library(ggplot2)

test <- incarceration %>%
  select(year, black_female_prison_pop, black_male_prison_pop, 
         white_female_prison_pop, white_male_prison_pop)

# Difference between average incarceration rates of Black people in 1995 and 2018
diff_1995_2018 <- incarceration %>%
  filter(year == 1995 |year == 2018) %>%
  group_by(year) %>%
  mutate(perc = black_jail_pop/total_jail_pop) %>%
  summarize(avg = mean(perc, na.rm = T)) %>%
  summarize(diff = round(abs(avg[1] -avg[2]),4)) %>%
  pull(diff) * 100
  

# Difference in average percentage of Black people in jail 
# between Northeast and South in 2018
avg_black_rate_ne <- incarceration %>%
  filter(region == "Northeast", year == 2018) %>%
  select(total_jail_pop, black_jail_pop) %>%
  filter(complete.cases(.))%>%
  mutate(rate = black_jail_pop/total_jail_pop) %>%
  summarize(avg = mean(rate, na.rm = T)) %>%
  pull(avg) *100

avg_black_rate_south <- incarceration %>%
  filter(region == "South", year == 2018) %>%
  select(total_jail_pop, black_jail_pop) %>%
  filter(complete.cases(.))%>%
  mutate(rate = black_jail_pop/total_jail_pop) %>%
  summarize(avg = mean(rate, na.rm = T)) %>%
  pull(avg) *100

diff_ne_south <- round(abs(avg_black_rate_south - avg_black_rate_ne), 2)

# Percentage of incarcerated people who are Black in 2018
black_jail_pop_perc <- incarceration %>%
  filter(year == 2018) %>%
  select(total_jail_pop, black_jail_pop) %>%
  filter(complete.cases(.))%>%
  summarize(black = sum(black_jail_pop) * 100, 
            total = sum(total_jail_pop) *100)%>%
  summarize(rate = round(black/total,4)) %>%
  pull(rate) *100

# Percentage of Black population in 2018
black_pop_perc <- incarceration %>%
  filter(year == 2018) %>%
  select(total_pop_15to64, black_pop_15to64) %>%
  filter(complete.cases(.))%>%
  summarize(black = sum(black_pop_15to64) * 100, 
            total = sum(total_pop_15to64) *100)%>%
  summarize(rate = round(black/total,4)) %>%
  pull(rate) *100

# Overall Black women to White women incarceration percentage
black_women <- incarceration %>%
  summarize(black = sum(black_female_prison_pop, na.rm = T), 
            white = sum(white_female_prison_pop, na.rm = T)) %>%
  summarize(perc = round(black/(black+white),4)) %>%
  pull(perc) *100
  

#Trends over time
# Jail population in each region every 5 years from 1995 to 2015
region_5yr <- incarceration %>%
  filter(year == 1995| year == 2000 | year == 2005 |
         year == 2010| year == 2015) %>%
  select(year, region, total_jail_pop) %>%
  mutate(total = sum(total_jail_pop, na.rm = T)) %>%
  group_by(year, region) %>%
  summarize(pop = sum(total_jail_pop, na.rm = T))

trend_plot <- ggplot(data = region_5yr) +
  geom_line(mapping = aes(x = year, y = pop, color = region)) +
  labs(title = "Jail Population in Each Region 1995-2015")+
  ylab("Jail Population") +
  xlab("Year") +
  labs("Region")



# Variable Comparison
# rated capacity of jail vs. total jail population in 2018
total_vs_white <- incarceration %>%
  filter(year == 2018) %>%
  select(total_pop, white_jail_pop) %>%
  filter(complete.cases(.))

comparsion <- ggplot(data = total_vs_white) +
  geom_point(mapping = aes(x = white_jail_pop, y = total_pop)) +
  scale_x_continuous() + 
  scale_y_continuous() +
  xlab("White Jail Population") +
  ylab("Total Population") +
  labs(title = "Correlation Between Total Population and White Jail Population" )




# Map
# Percentages of Black jail population by state in 2018
# Categories: 0-10%, 10-20%, 20-30%, 30-40%, >40%
black_jail_pop_states <- incarceration %>%
  filter(year == 2018) %>%
  group_by(state) %>%
  summarize(total_jail = sum(total_jail_pop, na.rm = T), 
            black_jail = sum(black_jail_pop, na.rm = T)) %>%
  mutate(per = round((black_jail/ total_jail)*100, 2)) %>%
  mutate(state = tolower(state.name[match(state,state.abb)]))
  
state_shape <- map_data("state") %>%
  rename(state = region) %>%
  left_join(black_jail_pop_states, by = "state")

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        
    axis.text = element_blank(),        
    axis.ticks = element_blank(),       
    axis.title = element_blank(),       
    plot.background = element_blank(),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.border = element_blank()    
  )

map <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = per),
    color = "white",
    size = .1
  ) +
  coord_map() +
  scale_fill_continuous(low = "#9effe8", high = "Blue") +
  labs(title = "Black Incarceration Rate by State in 2018",
    fill = "Percentage") +
  blank_theme


