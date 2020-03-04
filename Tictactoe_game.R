# Tic Tac Toe game 
# by Eden Kim

# # triples # #
# the 8 ways to win
triples <- list(
  c(1,2,3),
  c(4,5,6),
  c(7,8,9),
  c(1,4,7),
  c(2,5,8),
  c(3,6,9),
  c(1,5,9),
  c(3,5,7)
)

######################
# # display(state) # #
# displays the current state of the board.
display <- function (state){
  cat( "\n" )
  cat( " ", state[1]," | ", state[2]," | ", state[3], sep = "" )
  cat( "\n---+---+---\n" )
  cat( " ", state[4]," | ", state[5]," | ", state[6], sep = "" )
  cat( "\n---+---+---\n" )
  cat( " ", state[7]," | ", state[8]," | ", state[9], sep = "" )
  cat( "\n" )
}

###############################
# # prompt_user(who, state) # #
# prompts the user for where they want to play.
# checks to see if the move is legal or not
# if the move is legal returns the pos
prompt_user <- function (who, state){
  pos <- 0
  while( pos == 0 ){
    pos <- as.numeric( readline( "Choose a position between 1-9: " ) )
    if ( !( pos %in% 1:9 ) ) { 
      cat( "Please enter integers from 1 to 9.\n\n" )
      pos <- 0
    }
    # check if the position is taken
    else if ( state[pos] != pos ) {
      cat( "Invalid position try again.\n\n" )
      pos <- 0
    }
    else break
  }
  return (pos)
}

###############################
# # update(state, who, pos) # #
# updates the state of the board by putting an x or o (who) 
# in the designated position (pos)
# returns the updated state
update <- function (state, who, pos) {
  if ( who == 1 ) state[[pos]] <- "x"
  else if ( who == 2 ) state[pos] <- "o"
  return (state)
}

#################################
# # computer_turn(state, who) # #
# the computer takes a turn. The input is the state and who
# The function returns the updated state.
computer_turn <- function (state, who){
  # computer is the second player, "o"
  if ( who == 1 ){
    # # attack strategy
    # make an aribitrary movement and check if it is the winning move
    for ( i in 1:9 ) {
      if ( state[i] == i ){
        state[i] <- "o"
        if ( check_winner(0, state) == 2 ){
          return(state)
        } else { state[i] <- i }
      }
    }
    # # defense strategy
    # make an arbitrary movement as "x" and check if it is 
    # the winning move for the "x" and replace with "o"
    for ( i in 1:9 ) {
      if ( state[i] == i ){
        state[i] <- "x"
        if ( check_winner(0, state) == 1 ){
          state[i] <- "o"
          return(state)
        } else { state[i] <- i }
      }
    }
    if ( sum(state == "x") > sum(state == "o") ){ # check if computer made a move above
      if ( state[5] == 5 ){ # if 5 is empty put make a move
        state[5] <- "o"
       } else {
        for( i in sample(1:9, 9) ){ # make a random move if empty
          if ( state[i] == i ){
            state[i] <- "o"
            break
          }
        }
      }
    }
  } 
  # computer is the first player, "x"
  else if ( who == 2 ){
    # # attack strategy
    for ( i in 1:9 ) {
      if ( state[i] == i ){
        state[i] <- "x"
        if ( check_winner(0, state) == 1 ){
          return(state)
        } else { state[i] <- i }
      }
    }
    # # defense strategy
    for ( i in 1:9 ) {
      if ( state[i] == i ){
        state[i] <- "o"
        if ( check_winner(0, state) == 2 ){
          state[i] <- "x"
          return(state)
        } else { state[i] <- i }
      }
    }
    if ( sum(state == "x") == sum(state == "o") ){ # check if computer made move from above
      if ( state[5] == 5 ){ # if 5 is empty make a move
        state[5] <- "x"
      } else {
        for( i in sample(1:9, 9) ){ # make a move if empty
          if ( state[i] == i ){
            state[i] <- "x"
            break
          }
        }
      }
    }
  }
  return(state)
}

###########################
# # check_winner(state) # #
# checks if there is a winner.
# if there is a winner changes the value of winner. otherwise winner = 0
# returns winner
check_winner <- function (winner, state){
  if ( any( state == 1:9 ) == FALSE ) winner <- 3 # draw
  for( i in 1:length(triples) ){
    if( all( state[triples[[i]]] == "x" ) ) winner <- 1 # "x" is the winner
    else if ( all( state[triples[[i]]] == "o" ) ) winner <- 2 # "o" is the winner
  }
  return(winner)
}

##############
# # play() # #
# The function first asks if there is one or two human players. 
# If there is one human player, it asks if the human will play first or second.
play <- function(){
# # initialize game board
winner <- 0
state <- 1:9
  
# # determine game conditons: 1 or 2 players.
players <- as.numeric(readline("Enter 1 for single player or 2 for 2 players: "))

# # Single player # #
if ( players == 1 ){
  # # determine who goes first
  who <- as.numeric(readline("If you want to go first input 1 or else input 2: "))
  while ( (who != 1) & (who != 2) ){
    who <- readline("Error please input an integer 1 or 2: ")
  }
  while( winner == 0 ){
    # # User goes first
    if ( who == 1 ){
      # # x's turn (user's turn)
      display(state) # display board
      cat("x's turn.\n")
      pos <- prompt_user(who, state) # get user's position
      state <- update(state, who, pos) # update board
      winner <- check_winner(winner, state) # check winner
      # if x wins, loop quits
      if ( winner == 1 | winner == 3 ) break
      
      # # o's turn (computer's turn)
      display(state) # display board
      state <- computer_turn(state, who) # use strategy and update board
      winner <- check_winner(winner, state) # if o wins, loop quits
    } 
    
    # # Computer goes first
    else {
        # # x's turn (computer's turn)
        state <- computer_turn(state, who)
        winner <- check_winner(winner, state)
        if ( winner == 1 | winner == 3 ) break
        
        # # o's turn (user's turn)
        display(state)
        cat("o's turn.\n")
        pos <- prompt_user(who, state)
        state <- update(state, who, pos)
        winner <- check_winner(winner, state)
      }
  }
} 

# # Two human players # #
else if ( players == 2 ){
  while( winner == 0 ){
    # # x's turn (first player)
    display(state)
    cat("x's turn.\n")
    pos <- prompt_user(who = 1, state)
    state <- update(state, who = 1, pos)
    winner <- check_winner(winner, state) 
    # if x wins - quit loop
    if ( winner == 1 | winner == 3 ) break
    
    # # o's turn (second player)
    display(state)
    cat("o's turn.\n")
    pos <- prompt_user(who = 2, state)
    state <- update(state, who = 2, pos)
    winner <- check_winner(winner, state)
  }
}

# # display final board state and who is the winner 
display(state)
if( winner == 1 ) cat( "x wins!\nTo play again enter play()\n" )
else if ( winner == 2 ) cat( "o wins!\nTo play again enter play()\n" )
else cat( "Draw!\nTo play again enter play()\n" )
}

