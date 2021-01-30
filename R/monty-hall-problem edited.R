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
#'   The Contestant Makes and Initial Choice.
#'
#' @description
#'   `select_door` creates a vector for the 3 doors in the game, (1, 2, 3), 
#'   and randomly selects one to represent the contestant's intial choice.
#' 
#' @details
#'   This function first creates a sample of the three doors, 
#'   a numeric vector of (1, 2, 3). The items ordered in create_game 
#'   are hidden behind these doors. Then it randomly samples one of the 
#'   doors as the initial choice of the contestant. At this point, only 
#'   the host knows what is behind the door.
#'
#' @param 
#'  No arguments are used in this function.
#'
#' @return 
#'   The function returns a single numeric value from the vector (1, 2, 3).
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
#'   Host Opens Another Door.
#' 
#' @description
#'   `open_goat_door` matches the vector created in create_game to a vector 
#'   of doors (1, 2, 3), and chooses a door that contains a goat 
#'   but was not chosen in `select_door`.
#'
#' @details
#'   At this point in the game, the host opens of the two doors
#'   which were not chosen by the contestant. The host only opens
#'   a door with a goat behind it. The contestat still does not know 
#'   what is behind the other two doors.
#'
#' @param 
#'   This function has two argments. The first is `game` which is a character vector
#'   created in the `create_game` function, and the second is `a.pick` which is
#'   a single numeric value that results from the `select_door` function.
#'
#' @return 
#'   This function returns a single numeric value between 1 and 3, from the doors vector (1, 2, 3).
#'
#' @examples
#'   open_goat_door( this.game, my.initial.pick )
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
#'   Contestant Chooses Whether to Change their Selection
#' 
#' @description
#'   During this step, the contestant is given the opportunity to 
#'   either keep their original door selection, or switch to the 
#'   other unopened door. They do not know what is behind the two 
#'   doors, just that one is a goat and one is a car.
#' 
#' @details
#'   This function creates an `if` scenario regarding whether `stay=T` and the 
#'   contestant's final choice is the same as the initial choice, or if `stay=F` 
#'   and the contestant's final choice then changes to the other unopened door.
#'
#' @param 
#'   This function has three arguments, `stay=T, opened.door, a.pick`.
#'   `stay=T` is a logical value. `stay=T` is the default and will keep the contestant's choice unchanged.
#'   `stay=F` will randomly choose one of the two unopened doors as the contestant's new choice.
#'   `opened.door` is a numeric value, usually stored in an object, that is the return of 'opened_goat_door`.
#'   `a.pick` is the return of `select_door`, a numeric value stored as an object
#'
#' @return 
#'   The return of this function is a numeric value between 1 and 3. 
#'
#' @examples
#'   change_door(stay=T, opened.goat.door, initial.pick)
#'   change_door(stay=F, montys.goat.door, first.pick)
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
#'   Determine Whether the Contesant has Won
#'
#' @description
#'   This function checks whether the contestant's final door choice hides the car.
#'
#' @details
#'   #' The function reveals whether the contestant chose the door with the car 
#'   or a goat. It will match the doors 1, 2, 3 to the `game` vector 
#'   and then see if `final.pick` is the car, the contestant
#'   will be determined a winner. If it is a goat, they will be a loser.
#'
#' @param 
#'   This function has two arguments, `final.pick` and `game`. 
#'  `final.pick` is the return of `change_door`, a numeric value that is usually stored as an object.
#'  `game` is the return of `create_game`, a vector of character values, also stored as an object.
#'
#' @return 
#'   The return is a character, either `WIN` or `LOSE`.
#' 
#' @examples
#'   determine_winner( my.final.pick, this.game )
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
#'   Playing the Monty Hall Game
#'  
#' @description
#'   `play is a wrapper function that plays the entire Monty Hall game from start to finish.
#'
#' @details
#'    This function combines the functions `create_game`, `select_door`, 
#'   `open_goat_door`, `change_door`, and `determine_winner` to complete the 
#'   entire Monty Hall game from the initial placing of goats and car to the final selection
#'   and result from the contestant.
#'   
#' @param 
#'   This function does not have any arguements.
#'   
#' @return
#'   The return is a table showing the contestant's choice and whether they won or lost.
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
#'   Loop the Monty Hall Game
#'
#' @description
#'   `play_n_games` creates a looped simulation to see what the results
#'   of the Monty Hall Game are over `n` simulations, and compare whether
#'   staying with the contestant's original choice or switching to the other
#'   unopened door provides a better chance of winning the car.
#'   
#' @details
#'   This function adds a loop feature into the `play_game` function, so that 
#'   in the result you can see whether the contestant won the car more often by chooseing
#'   `STAY=T` or `STAY=F`.
#'
#' @param
#'   This function has one argument, which is a numeric value for the number of times to recreate
#'   the simulation, such as n=10,000.
#' 
#' @return 
#'   The return is a table showing the decimal percentages of how often 
#'   the contestan won or lost.
#'   
#' @examples
#'   play_n_games( n=100 )
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
