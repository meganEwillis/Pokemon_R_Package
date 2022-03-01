library("PokemonPackage")
data(pokemon_data)

test_that("initialize function works for pokemon that exists",{
  p = pokemon$new("Litwick")
  expect_equal(p$name, "Litwick")
  expect_equal(p$startingHP, 50)
  expect_equal(p$type1, "Ghost")
  expect_equal(p$type2, "Fire")
  expect_equal(p$awake, TRUE)
})

test_that("initialize function doesn't work for pokemon that don't exist",{
  p = pokemon$new("Litwickss")
  expect_equal(p$name, NULL)
  expect_equal(p$startingHP, NULL)
  expect_equal(p$type1, NULL)
  expect_equal(p$type2, NULL)
  expect_equal(p$awake, NULL)
})
