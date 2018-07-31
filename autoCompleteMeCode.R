# Tanguy Dauphin
# AutoComplete-me
# 10/19/17

# In this file "query" means "prefix" and "word" means "query".
# I did not have enough time to fix this
suppressMessages(
suppressWarnings(
if (!require(data.tree)) {
  install.packages("data.tree", repos = "http://cran.us.r-project.org")
  library(data.tree)
}
))

suppressMessages(
suppressWarnings(
  if (!require(liqueueR)) {
    install.packages("liqueueR")
    library(liqueueR)
  }
))

# Get data takes a filename and returns a data frame with columnns 'weights' and 'words'
getData = function(fileName){
  dat = read.csv(fileName, header = FALSE, skip = 1, sep = "\t", quote = "", stringsAsFactors = FALSE)
  colnames(dat) = c("weights", "words")
  return(dat)
}

# getTrie takes a data frame of words and weights and returns a tree with all of those words in it
# It sets maxWeights to the maximum weight of its subtree for each node
# It sets weights to the weight of the node if it's a word
# It sets isWord to TRUE if its a word
# It sets word to the word of the node it represents if it's a word
# Input: dat - a dataframe with collumns "words" and "weights"
# Output: trie - a tree with all the words and weights
getTrie = function(dat) {
  trie = Node$new("")
  numberOfRows = nrow(dat)
  for(row in 1:numberOfRows) {
    weight = dat[row, "weights"]
    word = dat[row, "words"]
    # use to lower so that "aa" can still return Aaden etc.
    characters = tolower(strsplit(word, "")[[1]])
    # start at root of trie for each word
    node = trie
    for (char in characters) {
      # if that character if not a child of the previous one then add it
      if (is.null(node$children[[char]])) {
        newNode = node$AddChild(char)
      }
      # set node equal to current character
      node = node$children[[char]]
      # Update max weight
      if (is.null(node$maxWeights) || node$maxWeights < weight) {
        node$maxWeights = weight
      }
    } 
    # The last character is seen as the word
    node$weights = weight
    node$word = word
    node$isWord = TRUE
  }
  return(trie)
}


# Get the node for the query so that we can limit our search to its children only.
# This function takes a trie and a query and returns the node that represents the query.
# If the query is not a subword of a word in the file, then it gives a warning and returns NULL
getQueryNode = function(trie, query) {
  node = trie
  characters = tolower(strsplit(query, "")[[1]])
  for (char in characters) {
    newNode = node$children[[char]]
    if (!is.null(newNode)) {
      node = newNode
    }
    else {
      warning("There is no word in the file starting with '", query, "'")
      return(NULL)
    }
  }
  return(node) 
}

# After building the trie and getting the subtrie for the given query, find the best k autocompletes.
# 
autocompleteFromWordNode = function(trie, queryNode, k, query) {
  if (is.null(queryNode)) {
    return(NULL)
  }
  QQ = PriorityQueue()
  wordsQ = PriorityQueue()  
  if (is.null(queryNode$maxWeights)) {
    QQ$push(queryNode, 0)
  }
  else{
    QQ$push(queryNode, queryNode$maxWeights)
  }
  # Stop when we either have our k words or we found all of the words starting with query
  while (length(wordsQ$priorities) != k) {
    if (QQ$empty()) {
      break
    }
    currentNode = QQ$pop()
    # If we find a word, add it to our list
    if(is.character(currentNode)) {
      word = currentNode[1]
      weight = as.numeric(currentNode[2])
      wordsQ$push(word, weight)
    }
    else {
      # If it's a word, add it back to QQ but with it's own weight not the maxWeight
      if (!is.null(currentNode$isWord)) {
        # need to be able to have the word and weight when I pop it out
        QQ$push(c(currentNode$word, currentNode$weights), currentNode$weights)
      }
      children = currentNode$children
      # add all children in QQ
      for (child in children) {
        QQ$push(child, child$maxWeights)
      } 
    }
  }
  amountOfWordsFound = length(wordsQ$priorities)
  # If there were less than k words throw a warning
  if (amountOfWordsFound != k) {
    warning("There were only ", amountOfWordsFound, " words starting with '", query, "'")
  }
  df = data.frame(weight = wordsQ$priorities, word = unlist(wordsQ$data))
  return(df)
}



# wrapper function that get the autocomplete from the file
autocomplete = function(query, fileName, k) {
  dat = getData(fileName)
  trie = getTrie(dat)
  queryNode = getQueryNode(trie, query)
  result = autocompleteFromWordNode(trie, queryNode, k, query)
  return(result)
}
