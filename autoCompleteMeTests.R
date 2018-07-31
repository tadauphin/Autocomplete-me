# Testing for AutoComplete-Me challenge
# By: Tanguy Dauphin
# Requirements: autoCompleteMeCode.R and the text file is in the directory.

source("autoCompleteMeCode.R")

if (!require(testthat)) {
  install.packages("testthat")
  library(testthat)
}

################################## Testing specific cases ##################################

pokemonData = getData("pokemon.txt")
babyNamesData = getData("baby-names.txt")
moviesData = getData("movies.txt")
wikiData = getData("wiktionary.txt")


# Rotom should come before Rottom-Frost but not before Rottom-Wash
test_that("Rotom should come before Rottom-Frost but not before Rottom-Wash", {
expect_true(all.equal(autocomplete("Ro", "pokemon.txt", 6), 
              data.frame("weight" = c(1241320, 109412, 51597, 25328, 9180, 9005), 
                         "word" = c("Rotom-Wash", "Roserade", "Rotom-Heat", 
                                    "Rotom-Mow", "Rotom", "Rotom-Frost"))))
expect_true(all.equal(autocomplete("Rot", "pokemon.txt", 3), 
                      data.frame("weight" = c(1241320, 51597, 25328), 
                                 "word" = c("Rotom-Wash", "Rotom-Heat", "Rotom-Mow"))))
})
# If the prefix is "" return top words
test_that("If the prefix is '' return top words", {
expect_true(all.equal(autocomplete("", "pokemon.txt", 2), 
                      data.frame("weight" = c(2194440, 1968270), 
                                 "word" = c("Scizor", "Ferrothorn"))))
})
# Test that it returns the right answers for various prefixes
test_that("it returns the right answers for various prefixes", {
expect_true(all.equal(autocomplete("abs", "wiktionary.txt", 5),
                      data.frame("weight" = c(5350280, 5139440, 3909310, 2571890, 2402990),
                                 "word" = c("absence", "absolutely", "absolute", "absurd", "absent") )))
expect_true(all.equal(autocomplete("Keish", "baby-names.txt", 3),
                      data.frame("weight" = c(60, 20, 11),
                                 "word" = c("Keisha", "Keishawn", "Keishaun") )))

expect_true(all.equal(autocomplete("Dim", "baby-names.txt", 4),
                      data.frame("weight" = c(195, 47, 32, 20),
                                 "word" = c("Dimitri", "Dimitrios", "Dimas", "Dima") ))) 
})
# only 6 answers
test_that("If there are less than k words, it returns however many there are", {
expect_true(all.equal(suppressWarnings(autocomplete("nov", "wiktionary.txt", 7)),
                      data.frame("weight" = c(5355180, 2350610, 1199040, 1116360, 897779, 517085),
                                 "word" = c("november", "novel", "novels", "nov", "novelty", "novelist") )))
})
# check that it works for non-words
test_that("It words for non-letter characters", {
expect_true(all.equal(autocomplete("'", "wiktionary.txt", 3),
                      data.frame("weight" = c(7025670, 3553690, 513367),
                                 "word" = c("'em", "'tis", "'ll") )))
})
# return right answer even if we don't capitalize first letter
test_that("It's not case sensitive", {
expect_true(all.equal(autocomplete("sant", "baby-names.txt", 5),
                      data.frame("weight" = c(3036, 400, 234, 210, 25),
                                 "word" = c("Santiago", "Santino", "Santana", "Santos", "Santonio") )))
})
# santa and santina both have weight 11 and are tied for 8. We don't know which one comes first 
# but they should both be in if we ask for 9
# Check that words with same weights can be returned
samePriority = autocomplete("sant", "baby-names.txt", 9)

test_that("words with same weights can be returned", {
expect_true("Santa" %in% samePriority$word)
expect_true("Santina" %in% samePriority$word)
expect_true(all.equal(autocomplete("Ky", "pokemon.txt", 4), 
                      data.frame("weight" = c(562832, 84960, 0, 0), 
                                 "word" = c("Kyurem-Black", "Kyurem", "Kyurem-W", "Kyogre")))) })

# test that wrong prefixes return NULL
test_that("wrong prefixes return NULL", {
expect_null(suppressWarnings(autocomplete("sss", "pokemon.txt", 5))) })

###################################### Randomized Testing #################################

# used to get information from the trie for the words in the file only
filterIsWord = function(x) !is.null(x$isWord)

files = c("baby-names.txt", "movies.txt", "pokemon.txt", "wiktionary.txt")
fileLengths = c(31109, 10000, 729, 10000)
numberOfFiles = length(files)

# use subset of data because it's too long
moviesSubData = moviesData[sample(1:dim(moviesData)[1], 10000),] 
filesData = list(babyNamesData, moviesSubData, pokemonData, wikiData)

pokemonTrie = getTrie(pokemonData)
babyNamesTrie = getTrie(babyNamesData)
moviesTrie = getTrie(moviesSubData)
wikiTrie = getTrie(wikiData)
tries = list(babyNamesTrie, moviesTrie, pokemonTrie, wikiTrie)

randomTrieTest = function(fileData, fileLength) {
  # Use different size of data every time
  numberOfObservations = sample(1:fileLength, 1)
  dat = fileData[sample(1:fileLength, numberOfObservations),]
  
  trie = getTrie(dat)
  wordsInTrie = trie$Get('word', filterFun = filterIsWord)
  weightsInTrie = trie$Get('weights', filterFun = filterIsWord)
  
  # make sure the tree entred word and weight for each word
  test_that("Trie entered the right aount of words and weights", {
  expect_true(length(wordsInTrie) == numberOfObservations)
  expect_true(length(weightsInTrie) == numberOfObservations) })
  
  # if a word is in the dataset it should be in the trie
  randomWord = dat[sample(1:numberOfObservations, 1), 'words']
  randomWeight = dat[dat$words == randomWord, 'weights']
  
  test_that("If a word is in the data set, it's word and weight were entered correctly", {
  expect_true(randomWord %in% wordsInTrie)
  expect_true(weightsInTrie[which(wordsInTrie == randomWord)] == randomWeight) })
}

randomQueryTest = function(fileTrie, fileLength) {
  # Get a random prefix everytime
  dat = filesData[[i]]
  rowIndex = sample(1:fileLength, 1)
  row = dat[rowIndex, ]
  word = row$words
  weight = row$weights
  lengthOfQuery = sample(1:7, 1)
  query = substr(word, 1, lengthOfQuery)
  
  queryNode = getQueryNode(fileTrie, query)
  expect_false(is.null(queryNode)) # the prefix is in the trie 
  wordsInQueryTrie = queryNode$Get('word', filterFun = filterIsWord)
  weightsInQueryTrie = queryNode$Get('weights', filterFun = filterIsWord)
  
  # word should be one of the prefix's children
  test_that("The word used to make the prefix is in the subtrie along with it's weight", {
  expect_true(word %in% wordsInQueryTrie)
  expect_true(weight %in% weightsInQueryTrie) 
  expect_true(weightsInQueryTrie[which(wordsInQueryTrie == word)] == weight) })
  
  # use a prefix that should not be in the trie
  falseQuery = paste(query, query, query, query, "query means prefix in my code")
  # supress the warning that the prefix is not in the trie
  falseQueryNode = suppressWarnings(getQueryNode(fileTrie, falseQuery))
  
  test_that("If we use a false prefix, it returns Null", {
  expect_null(falseQueryNode) })
}

randomAutocompleteFromNodeTests = function(fileTrie, word, weight, query, queryNode) {
  k = sample(1:25, 1)
  answer = suppressWarnings(autocompleteFromWordNode(fileTrie, queryNode, k, query))
  
  test_that("The answer has the right dimensions", {
  expect_false(is.null(answer))
  expect_true(dim(answer)[1] > 0) # There is at least one answer (word)
  expect_true(dim(answer)[1] <= k) })
  
  test_that("The weights are sorted and the top weight is at least equal to the weight of a word in the subtrie", {
  expect_true(answer$weight[1] >= weight) # the maximum weight can't be lower than the weight of word
  expect_true(all.equal(sort(answer$weight, decreasing = TRUE), answer$weight)) })
  
  # find lowest answer reported
  lowestAnswer = tail(answer, n = 1)
  lowestWeight = lowestAnswer$weight
  lowestWord = lowestAnswer$word
  
  expect_true(answer$weight[which(answer$word == lowestWord)] == lowestWeight)
  
  # if word has higher weight than the lowest answer reported
  if (lowestWeight < weight) {
    test_that("word is in the result along with it's weight", {
    expect_true(word %in% answer$word)
    expect_true(weight %in% answer$weight) })
  }
  
  # test that if there were no solution it returns NULL
  falseAnswer = autocompleteFromWordNode(fileTrie, queryNode = NULL, k, query)
  test_that("if there are no words starting with the prefix it returns NULL", {
  expect_null(falseAnswer) })
}

numberOfTests = 100
for (i in 1:numberOfFiles) {
  fileData = filesData[[i]] 
  fileTrie = tries[[i]]
  fileLength = fileLengths[i]
  
  # the next part is needed for testing the autocomplete only
  rowIndex = sample(1:fileLength, 1) 
  row = fileData[rowIndex, ]
  word = row$words
  weight = row$weights
  lengthOfQuery = sample(1:7, 1)
  query = substr(word, 1, lengthOfQuery)
  queryNode = getQueryNode(fileTrie, query)
  
  randomTrieTest(fileData, fileLength) # takes too long to test 100 times
  for (j in 1:numberOfTests) {
    randomQueryTest(fileTrie, fileLength)
    randomAutocompleteFromNodeTests(fileTrie, word, weight, query, queryNode)
  }
}