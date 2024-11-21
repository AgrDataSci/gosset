library("gosset")
library("ClimMobTools")
library("PlackettLuce")
library("ggplot2")
library("patchwork")


data("cassava", package = "gosset")

dat = cassava

keep = unlist(lapply(dat[1:ncol(dat)], function(x) sum(is.na(x))))

keep = keep == 0

keep

dat = dat[, keep]

names(dat)

trait_list = getTraitList(dat, c("_pos", "_neg"))

traits = unlist(lapply(trait_list, function(x) x$trait_label))

traits = gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", traits, perl = TRUE)

traits



pack = c("option_a", "option_b", "option_c")

items = sort(unique(unlist(dat[pack])))

ref = "Akpu"

ov = which(traits %in% "Overall")



table(unlist(dat[pack]), rep(dat$gender, 3))

table(unlist(dat[pack]), rep(dat$country, 3))

R = lapply(trait_list, function(x) {
  rank_tricot(dat, 
              items = pack, 
              input = x$string,
              validate.rankings = TRUE)
})

# Assess the full data
mod = lapply(R, PlackettLuce)

plot(mod[[ov]],
     ref = "Akpu",
     log = TRUE,
     levels = rev(items))

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


# Slice the data

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


slice = dat$country

slice_lvs = unique(slice)


trait_plot = list()

for (i in seq_along(slice_lvs)) {
  
  mod_i = lapply(R, function(x) {
    PlackettLuce(x[slice == slice_lvs[i], ])
  })
  
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


trait_plot[[1]] + trait_plot[[2]]



weights = c(0.20, 0.20, 0.30, 0.30)

select = data.frame()

for (i in seq_along(slice_lvs)) {
  
  mod_i = lapply(R, function(x) {
    PlackettLuce(x[slice == slice_lvs[i], ])
  })
  
  coeffs = lapply(mod_i, function(x) {coefficients(x, log = FALSE)})
  
  coeffs = do.call("rbind", coeffs)
  
  coeffs = apply(coeffs, 2, function(x) {x * weights})
  
  coeffs = colSums(coeffs)
  
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
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white"),
        text = element_text(color = "grey20"),
        strip.background.x = element_blank(),
        strip.placement = "outside",
        legend.position = "bottom",
        strip.text = element_text(size = 12, color = "grey20"),
        legend.text = element_text(size = 12, color = "grey20"),
        axis.text = element_text(size = 12, color = "grey20"),
        axis.title = element_text(size = 12, color = "grey20"),
        legend.title = element_blank()) +
  labs(x = "Selection score",
       y = "")

