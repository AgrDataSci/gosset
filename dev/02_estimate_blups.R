library("lme4")
library("car")
library("caret")
library("tidyverse")
library("magrittr")

feat1 <- read.csv("data/cassava-biophysical-features.csv")

feat2 <- read.csv("data/sweetpotato-biophysical-features.csv")

str(feat1)
str(feat2)

# coerce to factors and make the check variety to be the first

unique(feat1$genotype)

lvl1 <- union("IITATMSIBA920326", sort(unique(feat1$genotype)))

feat1$genotype <- factor(feat1$genotype,
                         levels = lvl1)


lvl2 <- union("NASPOT 8", sort(unique(feat2$genotype)))
feat2$genotype <- factor(feat2$genotype,
                         levels = lvl2)

str(feat1)
str(feat2)

# compute the linear estimates
features1 <- data.frame(genotype = lvl1)

features1

metrics1 <- names(feat1)[-1]

for(i in seq_along(metrics1)){
  form <- paste0(metrics1[i], " ~ (1|genotype)")
  
  form <- as.formula(form)
  
  mod_i <- lmer(form, data = feat1)
  
  coeffs <- coef(mod_i)[1][[1]]
  
  coeffs$genotype <- rownames(coeffs)
  
  features1 <- merge(features1, coeffs, by = "genotype", all.x = TRUE)
  
}

names(features1)[-1] <- metrics1

head(features1)

keep <- unlist(lapply(features1[1:ncol(features1)], function(x){
  !any(is.na(x))
}))

features1 <- features1[keep]

# now the same for the other data.frame
features2 <- data.frame(genotype = lvl2)

features2

metrics2 <- names(feat2)[-1]

for(i in seq_along(metrics2)){
  form <- paste0(metrics2[i], " ~ (1|genotype)")
  
  form <- as.formula(form)
  
  mod_i <- lmer(form, data = feat2)
  
  coeffs <- as.vector(coef(mod_i)[1][[1]][,1])
  
  features2 <- cbind(features2, coeffs)
  
}

names(features2)[-1] <- metrics2

rmv <- nearZeroVar(features2)

features2 <- features2[-rmv]

write.csv(features1, "data/blups-cassava.csv", row.names = FALSE)

write.csv(features2, "data/blups-sweetpotato.csv", row.names = FALSE)


feat1$genotype[feat1$genotype == "TMS13F1307P0016"] =	"TMS1"
feat1$genotype[feat1$genotype == "TMS13F1343P0044"] =	"TMS2"
feat1$genotype[feat1$genotype == "TMS14F1278P0003"] =	"TMS3"
feat1$genotype[feat1$genotype == "TMS13F1160P0004"] =	"Game Changer"
feat1$genotype[feat1$genotype == "TMS13F1343P0022"] =	"Obasanjo-2"
feat1$genotype[feat1$genotype == "TMS30572"] =	"TMS6"
feat1$genotype[feat1$genotype == "TMEB1_MS6"] =	"TMEB1"
feat1$genotype[feat1$genotype == "IITATMSIBA920326"] =	"TMSIBA"

sel <- c("genotype", 'cohesiveness', 'adhesiveness_g_sec_1', 'hardness_g', 
         'gumminess', 'chewiness', "resilience_percent", "springiness_percent",
         "percent_swelling_power" , "percent_soluble", 
         'color_l', 'color_a', 'color_b')

feat1 = feat1[sel]


items = unique(feat1$genotype)
sel = sel[-1]

values = matrix(NA, ncol = 1, nrow = length(items), 
                dimnames = list(items))

for(i in seq_along(sel)) {
  
  d = matrix(NA, ncol = 2, nrow = length(items), 
             dimnames = list(items, 
                             paste0(sel[i], c("Average", "SD"))))
  
  for(j in seq_along(items)) {
    v = feat1[, sel[i]][feat1$genotype == items[j]]
    m = mean(v, na.rm = TRUE)
    s = sd(v, na.rm = TRUE)
    d[items[j], ] = c(m, s)
  }
  
  values = cbind(values, d)
}

values = values[, -1]

write.csv(values, "output/lab-values.csv")


