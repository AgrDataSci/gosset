library("ClimMobTools")
library("gosset")
library("PlackettLuce")
library("ggplot2")

source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")

key = "424bc7dd-7ea5-43fe-ac91-830f5f657bde"

server = "climmob3"

owner = "luciletoniutti"

proj_id = "REVAIWP3"

# download the data
?getDataCM
dat = getDataCM(key,
                proj_id, 
                owner, 
                server = server, 
                tidynames = TRUE, 
                pivot.wider = TRUE)

# prepare the data to transform tricot rankings into PlackettLuce rankings

# 1. find the columns with package names
items_index = paste0("package_item_", LETTERS[1:3])

items = unique(unlist(dat[items_index]))

items

# 2. find the columns with trait (ranking tricot) data
traits = getTraitList(dat, pattern = c("_pos", "_neg"))

traits_names = unlist(lapply(traits, function(x) x$trait_label))

traits_names

# write by hand the trait labels
traits_labels = c("Adaptation after planting - Juvenil 2",
                  "Robustesse - Juvenil 2", "", "")

#traits_labels = traits_names

# run again the function getTraits adding labels
# look at the traits based on the given pattern c("_pos", "_neg")
# validate the rankings checking for consistency and data availability 
traits = getTraitList(dat, pattern = c("_pos", "_neg"), 
                      trait.labels = traits_labels)

traits[[1]]$string

# 3. tricot rankings into ordinal rankings (PlackettLuce)
R = lapply(traits, function(x){
  rank_tricot(data = dat, 
              items = items_index,
              input = x$string,
              validate.rankings = TRUE, 
              additional.rank = dat[c("juvenil2_item_A_vs_local",
                                      "juvenil2_item_B_vs_local", 
                                      "juvenil2_item_C_vs_local")])
})


R[[1]]

# 4. fit the model 
mod = lapply(R, PlackettLuce)

lapply(mod, summary)

lapply(mod, function(x){
  qvcalc(itempar(x, log = FALSE))
})

# letters come from this function 
?multcompView::multcompLetters2

plot_logworth(mod[[1]], log = FALSE, ref = "TMEB693")

worth_map(mod, labels = traits_labels) +
  scale_fill_distiller(palette = "BrBG", 
                       direction = 1, 
                       na.value = "white", 
                       name = "") + 
  theme_bw() +  
  theme(axis.text.x = element_text(size = 10, 
                                   angle = 45, 
                                   vjust = 1, 
                                   hjust = 1,
                                   color = "grey20"), 
        axis.text.y = element_text(size = 10, 
                                   angle = 0, 
                                   vjust = 1, 
                                   hjust = 1,
                                   color = "grey20"), 
        panel.grid = element_blank(),
        strip.background.x = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 10, color = "grey20"),
        legend.text = element_text(size = 10, color = "grey20"),
        axis.title = element_text(size = 10, color = "grey20")) +
  labs(x = "", y = "", fill = "")



















