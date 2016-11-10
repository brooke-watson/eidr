# creating multiple binary variables from a single unstructured character variable.  

dummyv = function(val, db, col) {
    # @param val value that you want to grep out of the column - character vector 
    # @param db database that you're looking in - loaded object 
    # @param col column that you're looking in  - character vector
    inds = agrep(val, db[[col]])
    newvar = paste0(col, "_", val) 
    db[newvar] = numeric(length=nrow(db))
    db[[newvar]][inds] = 1
    return(db)
}
dummyvh = function(val, db = all, col = 'hostVal'){
  # specific version of the above but more ready to lapply to a bunch of stuff 
  dummyv(val, db, col)
}

 

# # dummyv example (on eidr data)
# inds = agrep('human', all[['hostVal']])
# name = paste0(column, "_", value)
# all[name] = numeric(length=nrow(all))
# all[inds, name] = 1