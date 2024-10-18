library("gosset")
library("ClimMobTools")
library("PlackettLuce")
library("ggplot2")
library("patchwork")
library("multcompView")

data("cassava", package = "gosset")

#dat = cassava[cassava$country == "Nigeria", ]

dat = cassava

keep = unlist(lapply(dat[1:ncol(dat)], function(x) sum(is.na(x))))

keep = keep == 0

keep

dat = dat[, keep]

names(dat)

pack = c("option_a", "option_b", "option_c")

items = sort(unique(unlist(dat[pack])))

items

trait_list = getTraitList(dat, c("_pos", "_neg"))

traits = unlist(lapply(trait_list, function(x) x$trait_label))

traits = gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", traits, perl = TRUE)

ov = which(traits %in% "Overall")

table(unlist(dat[pack]), rep(dat$gender, 3))

table(unlist(dat[pack]), rep(dat$country, 3))

country = sort(unique(dat$country))

R = lapply(trait_list, function(x) {
  rank_tricot(dat, 
              items = pack, 
              input = x$string,
              validate.rankings = TRUE)
})

lapply(R, function(x){
  likelihood_ratio(x, split = dat$gender)
})

lapply(R, function(x){
  likelihood_ratio(x, split = dat$country)
})


G = lapply(trait_list, function(x) {
  rank_tricot(dat, 
              items = pack, 
              input = x$string,
              validate.rankings = TRUE,
              group = TRUE)
})


trait_plot = list()

for (i in seq_along(country)) {
  
  mod_i = lapply(R, function(x) {
    PlackettLuce(x[dat$country == country[i], ])
  })
  
  trait_plot[[i]] = worth_map(mod_i, 
            labels = traits) +
    labs(x = "", 
         y = "",
         title = country[i]) +
    scale_fill_distiller(palette = "BrBG", 
                         direction = 1, 
                         na.value = "white", 
                         name = "")
  
    
}


trait_plot[[1]] + trait_plot[[2]]



plot_logworth(mod[[ov]], 
              ref = "Akpu",
              levels = rev(items),
              multcomp = T,
              ci.level = 0.05)

rel = reliability(mod[[ov]], ref = "Akpu")

rel

rel$improvement = round((rel$reliability / 0.5 - 1), 2)

ggplot(data = rel,
         aes(x = reliability, 
             y = item,
             fill = "white")) +
  geom_bar(stat = "identity",
           width = 0.7,
           position = "dodge", 
           show.legend = FALSE) +
  scale_fill_manual(values = "#b2df8a") +
  geom_vline(xintercept = 0.5,
             colour = "#1f78b4",
             linewidth = 1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        strip.background =element_rect(fill="white"),
        text = element_text(color = "grey20"),
        strip.background.x = element_blank(),
        strip.placement = "outside",
        legend.position = "bottom",
        strip.text = element_text(size = 12, color = "grey20"),
        legend.text = element_text(size = 12, color = "grey20"),
        axis.text = element_text(size = 12, color = "grey20"),
        axis.title = element_text(size = 12, color = "grey20"),
        legend.title = element_blank()) +
  labs(x = "Probability of outperforming",
       y = "")

