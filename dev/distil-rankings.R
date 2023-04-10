library("ClimMobTools")
library("gosset")
library("PlackettLuce")

data("breadwheat")

head(breadwheat)

dat = breadwheat

dat$id = 1:nrow(dat)

# get the list of traits in breadwheat. The pattern for the data is c("_best", "_worst")
traits = getTraitList(dat, pattern = c("_best", "_worst"))

pack_index = paste0("variety_", letters[1:3])

# now transform into PlackettLuce rankings
# if you use group = TRUE the ranking can be used for PL trees
R = lapply(traits, function(x){
  rank_tricot(dat, 
              items = pack_index,
              input = x$string,
              group = FALSE)
})

# Now you have the PL rankings that can be used to fit PL model 
mod = lapply(R, PlackettLuce)

lapply(mod, summary)

# Now you can convert to ranking 
# (not really needed, only for data storage or publication)
rank_data = data.frame()

for (i in seq_along(traits)) {
  
  r = unclass(R[[i]])
  
  for (j in seq_along(dat$id)) {
    
    id = dat$id[j]
    
    plots = as.vector(unlist(dat[dat$id == id, pack_index]))
    
    x = r[j, plots]
    
    d = data.frame(id = id, 
                   plot = LETTERS[1:3],
                   tech_name = plots,
                   trait = as.vector(traits[[i]]$trait_label),
                   rank = x)
    
    rank_data = rbind(rank_data, d)
    
  }
  
}

rank_data

rownames(rank_data) = 1:nrow(rank_data)

head(rank_data)


