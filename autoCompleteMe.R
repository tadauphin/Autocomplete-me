# Wrapper Function for AutoComplete-Me challenge
# By: Tanguy Dauphin
# Requirements: autoCompleteMeCode.R and the text file is in the directory.
# Order of Arguments:
#   1. query to be autocompleted
#   2. textfile with weights as the first collumm and words as the second
#   3. number of autocompleted words to return (if there are that many)

source("autoCompleteMeCode.R")

Arguments = commandArgs(TRUE)
query = unlist(Arguments)[1]
fileName = unlist(Arguments)[2]
numberOfWords <- as.numeric(unlist(Arguments)[3])

autocomplete(query, fileName, numberOfWords)
