#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'   Contestant selects a door.
#'
#' @description
#'   `select_door()` randomly selects one of the three doors as the initial pick of the contestant.
#' 
#' @details
#'   This function creates the initial door selection for the contestant. This selection will influcence the following functions.
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns the number of the door selected.
#' 
#' @examples
#'   select_door()
#' 
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   The host opens a goat door.
#' 
#' @description
#'   `open_goat_door()` simulates the game show host opening a goat door after the contestant makes their initial pick.
#' 
#' @details
#'   The principal of the Monty Hall game is knowing the host will select a goat door which influences whether the contestant should stay with their initial pick or switch doors.
#' 
#' @param ... no arguments are used by the function.
#' @return The function returns the number of the goat door opened by the host.
#' 
#' @examples
#'   open_goat_door()
#' 
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'   Does the contestant stay with their initial pick or switch doors?
#' 
#' @description
#'   `change_door()` provides the conditions if the contestant decides to stay with their initial pick or change doors.
#' 
#' @details
#'   If the contestant chooses to stay with their initial pick, the function retains their initial pick. If the contestant chooses to switch doors, the function selects a door that is not their initial pick or a door opened by the host.
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns the door number of the initial pick if the contestant chose to stay or the number of the door the contestant switched to.
#' 
#' @examples
#'   change_door()
#' 
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#'   Did the contestant win?
#' 
#' @description
#'   `determine_winner()` provides the conditions to determine if the contestant won from staying with their intial pick or switching doors.
#' 
#' @details
#'   If the contestant's final selected door is a car, they win. If the contestant's final selected door is a goat, they lose.
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns "WIN" or "LOSE" depending on the contestant's final door selection.
#' 
#' @examples
#'   determine_winner()
#' 
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'   The whole Monty Hall Problem game
#' 
#' @description
#'   `pkay_game()` combines all of the game functions to quickly and easily generate game conditions and the subsequent win/lost outcome.
#' 
#' @details
#'   In one function, a game is set-up, an initial door selected, a goat door opened, the stay/switch decision made, and the final outcome associated with the final selected door is revealed.
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns "WIN" or "LOSE" as the game.results
#' 
#' @examples
#'   play_game()
#' 
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'   100 rounds of the Monty Hall Problem game
#' 
#' @description
#'   `play_n_games()` runs the game 100 times and reports the results of wins and losses.
#' 
#' @details
#'   The function uses a loop to run the game 100 times and reports the number of wins and losses. 
#' 
#' @param ... no arguments are used by the function.
#'  
#' @return The function returns the number of wins and losses in a data frame.
#' 
#' @examples
#'   play_n_games()
#' 
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
