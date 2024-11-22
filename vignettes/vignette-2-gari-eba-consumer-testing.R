## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE) 

## ----starting, message = FALSE, eval = TRUE, echo = TRUE----------------------
library("gosset")
library("ClimMobTools")
library("PlackettLuce")
library("ggplot2")
library("patchwork")

data("cassava", package = "gosset")

dat = cassava

head(dat[, 1:11])


## ----select, message = FALSE, eval = TRUE, echo = TRUE------------------------
keep = unlist(lapply(dat[1:ncol(dat)], function(x) sum(is.na(x))))

keep = keep == 0

dat = dat[, keep]

names(dat)


## ----traitlist, message = FALSE, eval = TRUE, echo = TRUE---------------------
# extract list of traits from the data
trait_list = getTraitList(dat, pattern = c("_pos", "_neg"))

# trait names extracted from the function 
traits = unlist(lapply(trait_list, function(x) x$trait_label))

# clean trait names and put them title case
traits = gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", traits, perl = TRUE)

traits

## ----prep, message = FALSE, eval = TRUE, echo = TRUE--------------------------
pack = c("option_a", "option_b", "option_c")

items = sort(unique(unlist(dat[pack])))

ref = "Akpu"

ov = which(traits %in% "Overall")

table(unlist(dat[pack]), rep(dat$country, 3))

table(unlist(dat[pack]), rep(dat$gender, 3))

# rmv = data.frame(table(unlist(dat[pack]), rep(dat$country, 3)))
# 
# rmv = as.vector(rmv[rmv$Freq == 0, "Var1"])
# 
# dat[pack] = lapply(dat[pack], function(x){
#   x[x %in% rmv] = NA
#   x
# })
# 
# keep = colSums(apply(dat[pack], 1, is.na)) < 2
# 
# dat = dat[keep, ]

## ----rank, message = FALSE, eval = TRUE, echo = TRUE--------------------------
R = lapply(trait_list, function(x) {
  rank_tricot(dat, 
              items = pack, 
              input = x$string,
              validate.rankings = TRUE)
})

## ----plfit, message = FALSE, eval = TRUE, echo = TRUE-------------------------
mod = lapply(R, PlackettLuce)

plot(mod[[ov]],
     ref = 1,
     log = TRUE,
     levels = rev(items))


## ----reliability, message = FALSE, eval = TRUE, echo = TRUE-------------------
rel = reliability(mod[[ov]], ref = "TMS1")

ggplot(data = rel,
       aes(x = reliability, 
           y = item,
           fill = "white")) +
  geom_bar(stat = "identity",
           width = 0.7,
           position = "dodge", 
           show.legend = FALSE) +
  scale_fill_manual(values = "#5aae61") +
  geom_vline(xintercept = 0.5,
             colour = "grey40",
             linewidth = 1) +
  theme_classic() +
  labs(x = "Probability of outperforming",
       y = "")


## ----worth, message = FALSE, eval = TRUE, echo = TRUE-------------------------
worth_map(mod, labels = traits) +
  labs(x = "", y = "") +
  scale_fill_distiller(palette = "BrBG", 
                       direction = 1, 
                       na.value = "white", 
                       name = "")

## ----llr, message = FALSE, eval = TRUE, echo = TRUE---------------------------
# by gender
llr1 = lapply(R, function(x){
  likelihood_ratio(x, split = dat$gender)
})

llr1 = do.call("rbind", llr1)

llr1$trait = traits

llr1

# by country
llr2 = lapply(R, function(x){
  likelihood_ratio(x, split = dat$country)
})

llr2 = do.call("rbind", llr2)

llr2$trait = traits

llr2 


## ----worth2, message = FALSE, eval = TRUE, echo = TRUE------------------------

# get the slice variable as a vector
slice = dat$country

# and get the unique values
slice_lvs = unique(slice)


trait_plot = list()

for (i in seq_along(slice_lvs)) {
  
  # fit the model also applying the slice
  mod_i = lapply(R, function(x) {
    PlackettLuce(x[slice == slice_lvs[i], ])
  })
  
  # plot the worth map
  trait_plot[[i]] = worth_map(mod_i, 
            labels = traits) +
    labs(x = "", 
         y = "",
         title = slice_lvs[i]) +
    scale_fill_distiller(palette = "BrBG", 
                         direction = 1, 
                         na.value = "white", 
                         name = "")
  
    
}

# plot the two maps using patchwork
trait_plot[[1]] + trait_plot[[2]] + plot_layout(ncol = 1)


## ----selection, message = FALSE, eval = TRUE, echo = TRUE---------------------

weights = c(0.20, 0.20, 0.30, 0.30)

select = data.frame()

for (i in seq_along(slice_lvs)) {
  
  # fit the model
  mod_i = lapply(R, function(x) {
    PlackettLuce(x[slice == slice_lvs[i], ])
  })
  
  # extract the coefficients
  coeffs = lapply(mod_i, function(x) {coefficients(x, log = FALSE)})
  
  coeffs = do.call("rbind", coeffs)
  
  # apply the weights within the coefficients
  coeffs = apply(coeffs, 2, function(x) {x * weights})
  
  coeffs = colSums(coeffs)
  
  # put it in a data.frame
  select_i = data.frame(item = names(coeffs),
                        slice = slice_lvs[i],
                        score = as.vector(coeffs))
  
  select = rbind(select, select_i)
  
  
}


ggplot(data = select,
       aes(x = score, 
           y = item,
           fill = slice)) +
  geom_bar(stat = "identity",
           width = 0.7,
           position = "dodge") +
  scale_fill_manual(values = c("#d73027", "#4575b4")) +
  geom_vline(xintercept = mean(select$score),
             colour = "grey40",
             linewidth = 1) +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  labs(x = "Selection score",
       y = "")


