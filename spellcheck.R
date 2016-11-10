#Spellcheck function 

spellcheck = function(word, word2 = word, column, db, max.dist = 3){
  # @param word = the pattern you wanna grep. character
  # @param word2 = what you actually want it to read. defaults to the same as the word 
    #you're looking for. character vector. 
  # @param column = the column from the database you want. character. 
  # @param db = database to use. right now i'm using the "all" db for EIDR. 
  # @param max.dist = number of misspellings that can go wrong 
  if (word2 == word){
    val = agrep(pattern = word, x = all[[column]], ignore.case = TRUE, value = FALSE, max.distance = max.dist)
    db[val, column] = word
  } else {
    val = agrep(pattern = word, x = all[[column]], ignore.case = TRUE, value = FALSE, max.distance = max.dist)
    db[val, column] = word2
  }
  return(db)
}
 
unique(all3$locationContinentVal)
all3 = spellcheckh("north america", column = "locationContinentVal", db=all)
all3 = all
strsplit(all3$locationContinentVal)

spellcheckh = function(word, word2, column = 'hostVal', db = all, max.dist = 3) {
  spellcheck(word, word2, column, db, max.dist = 3)
}

all3 = all = all2
all3
# testing on EIDR database
# all3 = all
# all3 = spellcheck("tanzania", "locationNationVal") # changed tanganyika to tanzania  