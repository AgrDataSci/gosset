library("ClimMobTools")
library("gosset")
library("PlackettLuce")

key = ""
server = "1000farms"

projects = getProjectsCM(key, server = server)

dat = getDataCM(key, 
                project = "Gnut2022",
                userowner = "Sanusi",
                server = server,
                as.data.frame = TRUE,
                pivot.wider = TRUE,
                tidy.names = TRUE)

pack_index = paste0("package_item_", LETTERS[1:3])

# getTraitList(dat, c("_pos", "_neg"))

R = rank_tricot(data = dat, 
                items = pack_index,
                input = c("physiologicalmaturity_overall_pos", 
                          "physiologicalmaturity_overall_neg"),
                validate.rankings = FALSE)


R

mod = PlackettLuce(R)

mod






