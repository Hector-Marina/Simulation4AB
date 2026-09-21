
library(AlphaSimR)


simulate_breeding <- function(prop_males) {
  
  ## simulate founder genomes
  founders <- runMacs(nInd = 500,
                      nChr = 5)

  ## settings
  simparam <- SimParam$new(founders)
  simparam$setSexes("yes_sys")

  ## set up a trait
  simparam$addTraitA(nQtlPerChr = 100)
  simparam$setVarE(h2 = 0.4)
  
  ## create the first population
  basepop <- newPop(founders,
                    simparam)
  
  ## set up data structures
  n_gen <- 40
  
  generations <- vector(length = n_gen,
                        mode = "list")
  
  generations[[1]] <- basepop
  
  ## main breeding loop
  for (gen in 2:n_gen) {
    sires <- selectInd(pop = generations[[gen - 1]],
                       nInd = 250 * prop_males,
                       simParam = simparam)
    dams <- selectInd(pop = generations[[gen - 1]],
                      nInd = 250,
                      simParam = simparam)
    offspring <- randCross2(females = dams,
                            males = sires,
                            nCrosses = 250,
                            nProgeny = 2,
                            simParam = simparam)
    generations[[gen]] <- offspring
  }
  generations
} 



## run the simulation once
generations <- simulate_breeding(prop = 0.6)
