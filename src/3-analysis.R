library(blme)

# Main models by the three DVs and the different neighbor items -----

dvs = c("sld", "ard", "hdd")
ivs = c("neighb_diffrace", "neighb_immig", "neighb_difflang", "wnneighb")


Models = list()
for (i in dvs){
  for (j in ivs){
    Models[[paste0(i, ": ", j)]] <- bglmer(as.formula(paste(i, "~", "z_age + I(z_age^2) + female + collegeed + 
                                                           z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
                                                           z_lemanc +", j,"  + (1 | year)")), 
                                          data=subset(Data, raceethnic == "White"), family = binomial(link="logit"),
                                          control=glmerControl(optimizer="bobyqa",
                                                               optCtrl=list(maxfun=2e5)))
  }
}



# Working on education now. First: the interactions -----

educ_models = lapply(setNames(dvs, dvs), function(var) {
  form = paste(var, "~ z_age + I(z_age^2) + female +
              collegeed + z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
              + z_lemanc  + wnneighb*collegeed + (1 | year)")
  bglmer(form, data=subset(Data, raceethnic == "White"), family=binomial(link = "logit"),
         control=glmerControl(optimizer="bobyqa",
                              optCtrl=list(maxfun=2e5)))
})

names(educ_models) <- c("sld: interact-educ", "ard: interact-educ", "hdd: interact-educ")

# Then: the random slopes for education levels -----

rs_models = lapply(setNames(dvs, dvs), function(var) {
  form = paste(var, "~  z_age + I(z_age^2) + female +
               z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
               + z_lemanc  + wnneighb  + (1 | year) + (1 + wnneighb | educatrmd)")
  bglmer(form, data=subset(Data, raceethnic == "White"), family=binomial(link = "logit"),
         control=glmerControl(optimizer="bobyqa",
                              optCtrl=list(maxfun=2e5)))
})

names(rs_models) <- c("sld: rs-educ", "ard: rs-educ", "hdd: rs-educ")

c(Models, educ_models, rs_models) -> Models



saveRDS(Models, "data/models.rds")
