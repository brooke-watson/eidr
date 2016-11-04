# creating binary variables for the thing!  
 

inds = grep(val, all[['hostVal']])
all['hostVal_human'] = numeric(length=nrow(all))
all[inds, 'hostVal_human'] = 1

dummyv = function(db, col, val) {
    # @param val value that you want to grep out of the column - character vector 
    # @param db database that you're looking in - loaded object 
    # @param col column that you're looking in  - character vector
    inds = grep(val, db[col])
    newvar = paste0(col, "_", val)
    newcol = db[newvar]
    db[newvar] = numeric(length=nrow(db))
    db[inds, newvar] = 1
}
alltest = dummyv(all, 'hostVal', 'human')


all['hostVal_human'] = numeric(length=nrow(all))

unique(all$hostVal)

