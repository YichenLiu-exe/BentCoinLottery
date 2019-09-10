#
# Author: Graham Smith
#
# A simulation of a lottery based on flipping a bent coin 10 times
#
# Currently written with questionable coding practices
#


library(stats)

toss_coins_ten_times <- function() {
  results <- array(0, 10)
  for (i in 1:10) {
    results[i] <- if (runif(1) < 0.1)
      1
    else
      0
  }
  return(results)
}

toss_coints_hundred_times <- function() {
  for (j in 1:100) {
    results <- array(0, 10)
    for (i in 1:10) {
      results[i] <- if (runif(1) < 0.1)
        1
      else
        0
    }
    print(results)
  }
}

toss_coins <- function(numberTossed) {
  results <- rbinom(numberTossed, 1, 0.1)
  return(results)
}

generate_ticket <- function(yourNumber) {
  name_temp <- class(yourNumber)
  if (name_temp != "character") {
    return("NOT A STRING INPUT!TRY AGAIN")
  } else {
    if (nchar(yourNumber) != 30) {
      return("CAN YOU COUNT? NOT 30 CHARACTER INPUT!TRY AGAIN")
    }
    for (i in 1:30) {
      if (substr(yourNumber, i, i) != "0" &
          substr(yourNumber, i, i) != "1") {
        return("WE WANT 0S AND 1S DUMMY!TRY AGAIN")
      }
    }
    return(yourNumber)
  }
}

generate_winning_number <- function() {
  winner <- rbinom(30, 1, 0.1)
  winner <- as.character(winner)
  return(winner)
}

check_winner <- function(your_Number, winning_number) {
  if (your_Number == winning_number) {
    return("YOU JUST WON $10,000,000. CONGRATS!")
  } else {
    return("YOU LOST, BUT IT'S ONLY $!, TRY AGAIN")
  }
}