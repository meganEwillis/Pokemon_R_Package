# for all functions, you can return values based on the first or random record if there are multiple matches,

# 1.  Write a function to print some information about a pokemon, Print its name, classification, and primary and secondary types

#' Function for getting data about a single pokemon
#' @param pokemon name of the pokemon we want information about
#' @examples pokeDex("Charmander")
#' @examples pokeDex("Litwick")
#' @return None
#' @export pokeDex
pokeDex<-function(pokemon){
  return(pokemon_data[pokemon_data$Pokemon.Name==pokemon,c(3,4, 10, 11)][1,])  #[1,] gives the first match
}

# 2. Write a function that takes in 2 pokemon and determines if the attack stat of the first pokemon is higher than the defense stat of the second.
#' Function that takes in two pokemon and returns true if the attack stat of the first pokemon is higher
#' @param attacker name of the attacking pokemon
#' @param defender name of the defending pokemon
#' @examples atkCheck("Charmander","Urshifu" )
#' @return None
#' @export
atkCheck<-function(attacker, defender){
  a_attack = pokemon_data[pokemon_data$Pokemon.Name == attacker, c(26)]
  d_attack = pokemon_data[pokemon_data$Pokemon.Name == defender, c(26)]
  if (a_attack > d_attack){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}


# 3. Write a function that returns the result of trying to catch the pokemon at a particular health
# using a modification of the formula found below catchProb=((3 * HPmax - 2 * HPcurrent) * ratemodified / (3 * HPmax))/100
#from here: https://bulbapedia.bulbagarden.net/wiki/Catch_rate#General_capture_method_.28Generation_II_onwards.29
#calculate the percent likelihood then sample c(TRUE,FALSE) with probabilities c(catchProb,1-catchProb) and return the result
#get the HPmax from the data

#' Function to see if you catch the pokemon or not
#' @param pokemon name of the pokemon we want to catch
#' @param HPcurrent current health of pokemon
#' @examples HPcurrent("Charmander", 20 )
#' @return None
#' @export
pokeBall<-function(pokemon, HPcurrent){
  HPmax <-pokemon_data[pokemon_data$Pokemon.Name==pokemon, ]$Health.Stat[[1]]
  ratemodified <-pokemon_data[pokemon_data$Pokemon.Name==pokemon, ]$Catch.Rate[[1]]
  catchProb <-((3 * HPmax - 2 * HPcurrent) * ratemodified / (3 * HPmax))/100
  catchStatus<-sample(c(T, F),size=1, prob=c(catchProb,1-catchProb),replace=T)
  return(catchStatus)
}



# Then
#1.  create a package with the data set and functions
#2.  roxygen notes, generate the documentation,
#3.  install the package
#4.  view your documentaion with ?pokeDex etc
#5.  use your functions

# Pokemon R6 Class
library("R6")
#' R6 Class representing a pokemon
#'
#' A pokemon has a name, two types, and HP.
#' @importFrom R6 R6Class
#' @export
pokemon = R6Class("pokemon",
                  public =
                    list(
                      #' @field name The name of the pokemon
                         name = NULL,
                      #' @field type1 The primary type of the pokemon
                         type1 = NULL,
                      #' @field type2 The secondary type of the pokemon
                         type2 = NULL,
                      #' @field startingHP The starting HP of the pokemon
                         startingHP = NULL,
                      #' @field awake The awake status of the pokemon
                         awake = NULL,

                      #' @description
                      #' Create a new pokemon object.
                      #' @param name Name.
                      #' @return A new `pokemon` object.
                         initialize = function(name = NA){
                           if (any(pokemon_data==name)==TRUE){
                             self$name = name
                             #pokemon_data[pokemon_data$Pokemon.Name==self$name, ]$Pokemon.Name[[1]] #pokemon_data[pokemon_data$Pokemon.Name == p,c(3)]
                             #self$type1 = pokemon_data[pokemon_data$Pokemon.Name==self$name,c(10)]
                             self$type1 = pokemon_data[pokemon_data$Pokemon.Name==self$name, ]$Primary.Type[[1]]
                             self$type2 = pokemon_data[pokemon_data$Pokemon.Name==self$name, ]$Secondary.Type[[1]]
                             self$startingHP = pokemon_data[pokemon_data$Pokemon.Name==self$name, ]$Health.Stat[[1]]
                             self$awake = TRUE
                           }

                         },
                      #' @description Show the pokemon and it's attributes
                      #' @examples
                      #' myPokemon = new$pokemon("Squirtle")
                      #' myPokemon$show()
                         show = function(){
                           cat("Name: ", self$name, "\nPrimary Type: ", self$type1, "\nSecondary Type: ", self$type2,
                               "\nStarting HP: ", self$startingHP, "\nAwake: ", self$awake)
                           #cat("Name: ", self$name, "\nPrimary Type: ", self$type1)
                         },
                      #' @description Change the HP of the pokemon by a user provided value, updates awake if HP<=0
                      #' @param n Amount to change the HP by (positive or negative)
                      #' @examples
                      #' myPokemon = new$pokemon("Squirtle")
                      #' myPokemon$changeHP(5)
                      #' @return None
                         changeHP = function(n){
                           self$startingHP = self$startingHP + n
                           if (self$startingHP<=0){
                             self$startingHP = 0
                             self$awake = FALSE
                           }
                         },
                      #' @description Takes in another pokemon, has the two pokemon 'fight', and randomly decreases their HP
                      #' If HP<=0, awake is set to FALSE
                      #' @param p A `pokemon` object
                      #' @examples myPokemon = new$pokemon("Squirtle")
                      #' myPokemon2 = new$pokemon("Charmander")
                      #' myPokemon$fight(myPokemon2)
                      #' @return None
                         fight = function(p){
                           num1 = sample(5:20, 1)
                           num2 = sample(5:20, 1)
                           if (self$awake == TRUE && p$awake == TRUE){
                             self$startingHP = self$startingHP - num1
                             p$startingHP = p$startingHP - num2
                             if (self$startingHP <=0){
                               self$awake = FALSE
                             }
                             else if(p$startingHP <=0){
                               p$awake = FALSE
                             }
                           }
                           else{
                             message("These pokemon can't fight!")
                           }
                         }
                         )
                  )

#' Data about Pokemon
#' a dataset about pokemon from source
#' @format Pokemon dataframe with 1,076 entries, 47 columns
#' @source Kaggle
"pokemon_data"

#adding a comment to test git
