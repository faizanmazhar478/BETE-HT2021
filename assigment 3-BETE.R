## ####Assignment 3: The Monty Hall problem -BETE HT2021 #######
# This code is to simulate Alternate solution of Monty hall Problem 
# Alternate solution methods:Forgetful Monty and the extreme number
#******** Code written by Faizan Mazhar ******************
#Student of Master´s Programme in Decision Analysis and Data Science
### Researcher, Dept of Epidemiology, KI ; faizan.mazhar@ki.se###


#### lod required PACKAGE (install if required) ####
list.of.packages <- c("table1", "dplyr", "ggplot2", "ggthemes")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(table1) # for table
library(dplyr) # for various pipe function and to get cumulative percentage by mutating
library(ggplot2) # for graph
library(ggthemes) # for graph theme


# STEP 1- Create THREE doors of uniformly distribution occurrences and hide car

n <- 1000000 # create number of one million game 
car <- sample(3, n, replace = TRUE) # hide car
frequency <- table(car) 

barplot(counts, main='Car Distribution', # make bar plot to visulaize distr
        col='grey',
        names.arg=c('Door-1', 'Door-2', 'Door-3'))

df <- data.frame(car)
head(df)



# STEP 2- et Player select the door
first_choice <- sample(3, n, replace = T)
df$first_choice <- first_choice
head(df)


# STEP 3- Let Monty open the door after player choice
# STEP 3- Let Monty open the door after player choice
df$player_guess <- ifelse(df$car == df$first_choice, TRUE, FALSE) 
df$door_Opened_by_monty <- ifelse(df$player_guess,  apply(df, 1, function(a) c(1:3)[c(1:3) != a['car']][sample(2, 1)]), 6 - df$car - df$first_choice)
head(df, 10)
## check the distribution of opened door
barplot(table(df$door_Opened_by_monty), 
        main = 'Door_Opened_by_Monty', 
        col='grey',
        names.arg=c('Door-1', 'Door-2', 'Door-3'))


# STEP 4- Strategy A- STAY with the initial choice , don't consider switching

# STEP 4- Strategy A- STAY with the initial choice, don't consider switching
# we will simply compute descriptive stats for player initial choice for this strategy 
table1(~ player_guess, data=df)

# make plot of player initial choice
dont_switch <- table(df$player_guess)

pie(dont_switch, 
    main = 'Dont Switch Strategy',
    labels = c('Wins Goat (Failed with first choice)', 'Wins Car (Guessed right with first choice)'),
    col = c('green', 'red'))

# STEP 5- Simulate with Strategy B- Always Switch

# we will  exclude door_Opened_by_monty and  first choice made by player.

df$always_switch <- 6 - df$first_choice - df$door_Opened_by_monty

always_switch <- as.data.frame(df$always_switch == df$car)  ## data set to get descriptive stats
always_switch <- table(df$always_switch == df$car)  ## for plot


table1(~ df$always_switch == df$car, data=always_switch)


pie(always_switch, 
    main = 'Always Switch Strategy',
    labels = c('Wins Goat (by switching from first choice)', 'Wins Car (by switching from first choice)'),
    col = c('red', 'green'))

# STEP 6-Head to head comparison of two strategies

### save the result of two strategies in values again from dataframe
dont_switch <- (df$player_guess)
always_switch <- (df$always_switch == df$car) 


# Get Cumulative percentage of game won by two stratigies


results = data.frame(
    game = seq_len(n),
    cumulative_wins_without_switching = cumsum(dont_switch),
    cumulative_wins_with_switching = cumsum(always_switch)
) %>% mutate(cumulative_per_dont_switch = round(cumsum(dont_switch)/game*100,1)) %>%
 mutate(cumulative_per_always_switch = round(cumsum(always_switch)/game*100,1))

# crate a function that returns values into  percentages
format_percentage = function(values, digits = 1) {
    return(paste0(formatC(values * 100, digits = digits, format = 'f'), '%'))
}


# insert  summary title based on result of two stratigies (taken n(%) of each strategy)
title = paste(
    paste0('Always switching strategy wins player ', sum(always_switch), ' of ', n, ' games (', format_percentage(mean(always_switch)), ')'),
    paste0('as compared to ', sum(dont_switch), ' games (', format_percentage(mean(dont_switch)), ') staying with first choice'),
    sep = '\n'
)

# define main plot parameter
linesize = 1 # size of the plotted lines
x_breaks = y_breaks = seq(from = 0, to = n, length.out = 10 + 1) # breaks of the axes
y_limits = c(0, n) # limits of the y axis - makes y limits match x limits
w = 8 # width for saving plot
h = 5 # height for saving plot
palette = setNames(c('green', 'pink'), nm = c('switching', 'without switching')) # make a named color scheme


# Create a line plot with head to head comparison of two strategies
ggplot(data = results) +
    geom_line(aes(x = game, y = cumulative_per_always_switch, col = names(palette[1])), size = linesize) +
    geom_line(aes(x = game, y = cumulative_per_dont_switch, col = names(palette[2])), size = linesize) +
    scale_x_continuous(breaks = x_breaks) +
    scale_color_manual(values = palette) +
    theme_minimal() +
    theme(legend.position = c(1, 1), legend.justification = c(1, 1), legend.background = element_rect(fill = 'white', color = 'transparent')) +
    labs(x = 'Number of games played') +
    labs(y = '% of games won') +
    labs(col = NULL)+
    labs(title = title) + theme_economist()




# ---------xxxxx END OF SCRIPT -xxxx-------------------
