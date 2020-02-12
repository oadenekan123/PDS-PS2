#1
for (num in 1:7) {
  print(num^3)
}

#2
set.seed(14) # set seed
num_iters = 1000
num_total_casts = 0
for (num in 1:num_iters) { # number of simulations
  num_casts_for_game = 0 # count the number of casts in individual game
  keep_going = TRUE
  while (keep_going) {
    coin_toss = sample(1:6, 2)
    coin_1 = coin_toss[1]
    coin_2 = coin_toss[2]
    toss_sum = coin_1 + coin_2
    sum_criteria = (toss_sum == 8)|(toss_sum == 9)|(toss_sum == 10)|(toss_sum == 11)|(toss_sum == 12)
    if (num == 1 & sum_criteria) { # check if 8,9,10,11,12 at first iteration
      keep_going = FALSE
    }
    num_criteria = (coin_1 == 2)|(coin_1 == 6)|(coin_2 == 2)|(coin_1 == 6)
    if (num > 1 & num_criteria) { # check if past 1st iteration and if coins are either 2 or 6
      keep_going = FALSE
    }
    num_casts_for_game = num_casts_for_game + 1 # count number of rolls
  }
  num_total_casts = num_total_casts + num_casts_for_game
}

ave_num_dice_casts = num_total_casts/num_iters
print(ave_num_dice_casts)
# averge number of dice casts per game: 2.148

#3
# read in data
setwd("C:/Users/oyina/src/senior_2019-2020/spring_2020/political_data_science/problem_sets/ps2")
df = read.csv('GSS-data.csv')

vote.choice = function(name) {
  if (name == "Clinton" | name == "Trump") { # if name is clinton or trump
    num_peeps = length(which(df$pres16 == name))
    return(num_peeps)
  }
  else if (name == "Other") { # if person enters other, get entries other than clinton and trump
    num_peeps = length(which((df$pres16 != "Clinton") & (df$pres16[1:10] != "Trump")))
    return(num_peeps)
  }
  else {
    print("Please enter either 'Trump' 'Clinton' or 'Other' into the function
          to return a valid response")
    return()
  }
    
}

#4
library(fivethirtyeight)
# head(cabinet_turnover)
cab_turn = cabinet_turnover

which_president = function(name, num_days) {
  if (name == "Clinton") {
    return(num_days/2922)
  }
  else if (name == "Carter") {
    return(num_days/1461)
  }
  else if (name == "Reagan") {
    return(num_days/2922)
  }
  else if (name == "Bush 41") {
    return(num_days/1461)
  }
  else if (name == "Bush 43") {
    return(num_days/2922)
  }
  else if (name == "Obama") {
    return(num_days/2922)
  }
  else {
    return(num_days/1105) 
  }
}

appoint = function(name) {
  valid_name = (name == "Carter")|(name == "Reagan")|(name == "Bush 41")|(name == "Clinton")|(name == "Bush 43")|(name == "Obama")|(name == "Trump")
  if (valid_name) {
    # data with president name and no NA in length
    relevant_data  = cab_turn[which((cab_turn$president == name) & !is.na(cab_turn$length)),]
    # sum relevant data at length
    ave_num_days = sum(relevant_data$length)/nrow(relevant_data)
    return(ave_num_days)
    # return(which_president(name, num_days))
  }
  else {
    return("invalid input")
  }
}

appoint("Reagan")

#5
congress_stats = function(stat) {
  
}


