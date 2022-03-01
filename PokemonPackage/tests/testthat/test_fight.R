library("PokemonPackage")
data(pokemon_data)

test_that("pokemon with startingHP<=0 is not awake",{
  p = pokemon$new("Squirtle")
  p$changeHP(-p$startingHP)
  expect_equal(p$awake, FALSE)
})

test_that("if one or both pokemon has HP of 0 or less, they won't fight",{
  p1 = pokemon$new("Squirtle")
  p2 = pokemon$new("Ivysaur")
  while(p1$startingHP > 0 & p2$startingHP > 0){
    p1$fight(p2)
  }
  if(p1$startingHP < p2$startingHP){
    expect_equal(p1$awake, FALSE)
  }
  else{
    expect_equal(p2$awake, FALSE)
  }
})

test_that("if pokemon is not awake, fight doesn't happen",{
  p1 = pokemon$new("Squirtle")
  p2 = pokemon$new("Ivysaur")
  p1$awake = FALSE
  expect_message(p1$fight(p2))
  expect_message(p2$fight(p1))
})
