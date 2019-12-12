dvs = c("sld", "ard", "hdd")

# allFit Models -----
# Note: no lmercontrols here. It won't matter much.



afmodels = lapply(setNames(dvs, paste0(dvs,": wnneighb-allfit")), function(var) {
  form = paste(var, "~ z_age + I(z_age^2) + female +
              collegeed + z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
              + z_lemanc  + wnneighb + (1 | year)")
  glmer(form, data=subset(Data, raceethnic == "White"), family=binomial(link = "logit"))
})


allfitslliks = as_tibble()
for(i in 1:length(afmodels)){
  AF <- allFit(afmodels[[i]], verbose=F)
  is_ok <- sapply(AF,is,"merMod")
  AF_ok <- AF[is_ok]
  AF_lliks <- sort(sapply(AF_ok,logLik))
  allfitslliks = bind_rows(allfitslliks, AF_lliks)
  #Temp = tidy(tmodels_harder[[i]]) %>% mutate(model = as.character(tidy(tmodels_harder[[i]])[12,1]))
  # allfits = bind_rows(allfits, Temp)
}


allfits_wnneighb = as_tibble()
for(i in 1:length(afmodels)){
  AF <- allFit(afmodels[[i]], verbose=F)
  is_ok <- sapply(AF,is,"merMod")
  AF_ok <- AF[is_ok]
for(j in 1:length(AF)) {
  tidy(AF_ok[[j]]) %>% filter(term == "wnneighb") %>% mutate(optim = names(AF_ok[j])) -> Temp
  allfits_wnneighb = bind_rows(allfits_wnneighb, Temp)
}
}

allfits_wnneighb %>%
  mutate(model = c(
    rep("Strong Leader", 7),
    rep("Army Rule", 7),
    rep("Oppose Democracy", 7)
  )) -> allfits_wnneighb


list("afmodels" = afmodels,
     "allfitslliks" = allfitslliks,
     "allfits_wnneighb" = allfits_wnneighb) -> aflist

saveRDS(aflist, "data/aflist.rds")


# Models for non-white respondents -----

nwmodels = lapply(setNames(dvs, paste0(dvs, ": non-white")), function(var) {
  form = paste(var, "~ z_age + I(z_age^2) + female +
              collegeed + z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
              + z_lemanc  + wnneighb + (1 | year)")
  bglmer(form, data=subset(Data, raceethnic != "White"), family=binomial(link = "logit"),
         control=glmerControl(optimizer="bobyqa",
                              optCtrl=list(maxfun=2e5)))
})

saveRDS(nwmodels, "data/nwmodels.rds")


# Models for Jews-Muslims responses -----

jmmodels = lapply(setNames(dvs, paste0(dvs, ": wnneighbjm")), function(var) {
  form = paste(var, "~ z_age + I(z_age^2) + female +
              collegeed + z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
              + z_lemanc  + wnneighbjm + (1 | year)")
  bglmer(form, data=subset(Data, raceethnic == "White"), family=binomial(link = "logit"),
         control=glmerControl(optimizer="bobyqa",
                              optCtrl=list(maxfun=2e5)))
})

neighb_jews_models = lapply(setNames(dvs, paste0(dvs, ": neighb_jews")), function(var) {
  form = paste(var, "~ z_age + I(z_age^2) + female +
              collegeed + z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
              + z_lemanc  + neighb_jews")
  glm(form, data=subset(Data, raceethnic == "White"), family=binomial(link = "logit"))
})

neighb_muslim_models = lapply(setNames(dvs, paste0(dvs, ": neighb_muslim")), function(var) {
  form = paste(var, "~ z_age + I(z_age^2) + female +
              collegeed + z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
              + z_lemanc  + neighb_muslim + factor(year)")
  glm(form, data=subset(Data, raceethnic == "White"), family=binomial(link = "logit"))
})

# library(purrr)
purrr::flatten(list(jmmodels, neighb_jews_models, neighb_muslim_models)) -> jmmodels

# list("jmmodels" = jmmodels,
#      "jmmodelssep" = jmmodelssep) -> jmlist

saveRDS(jmmodels, "data/jmmodels.rds")


# Models for 'militant minority' responses -----

mmmodels = lapply(setNames(dvs, paste0(dvs, ": wnneighbmm")), function(var) {
  form = paste(var, "~ z_age + I(z_age^2) + female +
              collegeed + z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
              + z_lemanc  + wnneighbmm + (1 | year)")
  bglmer(form, data=subset(Data, raceethnic == "White"), family=binomial(link = "logit"),
         control=glmerControl(optimizer="bobyqa",
                              optCtrl=list(maxfun=2e5)))
})

neighb_mm_models = lapply(setNames(dvs, paste0(dvs, ": neighb_mm")), function(var) {
  form = paste(var, "~ z_age + I(z_age^2) + female +
              collegeed + z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
              + z_lemanc  + neighb_mm")
  glm(form, data=subset(Data, raceethnic == "White"), family=binomial(link = "logit"))
})

purrr::flatten(list(mmmodels, neighb_mm_models)) -> mmmodels

# list("mmmodels" = mmmodels,
#      "neighb_mm_models" = neighb_mm_models) -> mmlist

saveRDS(mmmodels, "data/mmmodels.rds")


# Census-year models -----

censusryearmodels = lapply(setNames(dvs, paste0(dvs,": Region, Year, and Region-Year Random Effects")), function(var) {
  form = paste(var, "~ z_age + I(z_age^2) + female +
              collegeed + z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
              + z_lemanc  + wnneighb + (1 | year) + (1 | censusr) + (1 | censusr:year)")
  bglmer(form, data=subset(Data, raceethnic == "White"), family=binomial(link = "logit"),
         control=glmerControl(optimizer="bobyqa",
                              optCtrl=list(maxfun=2e5)))
})

saveRDS(censusryearmodels, "data/censusryearmodels.rds")

# LGBT models -----



lgbtmodels = lapply(setNames(dvs, paste0(dvs, ": wnneighblgbt")), function(var) {
  form = paste(var, "~ z_age + I(z_age^2) + female +
              collegeed + z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
              + z_lemanc  + wnneighblgbt + (1 | year)")
  bglmer(form, data=subset(Data, raceethnic == "White"), family=binomial(link = "logit"),
         control=glmerControl(optimizer="bobyqa",
                              optCtrl=list(maxfun=2e5)))
})

neighb_lgbt_models = lapply(setNames(dvs, paste0(dvs, ": neighb_lgbt")), function(var) {
  form = paste(var, "~ z_age + I(z_age^2) + female +
              collegeed + z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
              + z_lemanc  + neighb_lgbt + (1 | year)")
  bglmer(form, data=subset(Data, raceethnic == "White"), family=binomial(link = "logit"),
         control=glmerControl(optimizer="bobyqa",
                              optCtrl=list(maxfun=2e5)))
})

purrr::flatten(list("lgbtmodels" = lgbtmodels,
     "neighb_lgbt_models" = neighb_lgbt_models)) -> lgbtmodels

saveRDS(lgbtmodels, "data/lgbtmodels.rds")

#  There's no words there!  What does that mean? "To play us out?" -----
# 1) manuscript results (already have that in Models)
# 2) region & year fixed effects
# 3) region, year, and region-year random effects (already did that above)
# 4) year subsets
# 5) year fixed effects

ryfixefmodels = lapply(setNames(dvs, paste0(dvs,": Region and Year Fixed Effects")), function(var) {
  form = paste(var, "~ z_age + I(z_age^2) + female +
              collegeed + z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
              + z_lemanc  + wnneighb +  factor(year) + factor(censusr)")
  glm(form, data=subset(Data, raceethnic == "White"), family=binomial(link = "logit"))
})

saveRDS(ryfixefmodels, "data/ryfixefmodels.rds")

# I don't know what that means 'to play us out!' What does that mean? To end the show?

yfixefmodels = lapply(setNames(dvs, paste0(dvs,": Year Fixed Effects")), function(var) {
  form = paste(var, "~ z_age + I(z_age^2) + female +
              collegeed + z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
              + z_lemanc  + wnneighb +  factor(year)")
  glm(form, data=subset(Data, raceethnic == "White"), family=binomial(link = "logit"))
})

saveRDS(yfixefmodels, "data/yfixefmodels.rds")

# All right. Go. Go. Go.

years = c("1995", "1999", "2006", "2011")

ysmodels = list()
for (i in dvs){
  for (j in years){
    ysmodels[[paste0(i, ": Year = ", j)]] <- glm(as.formula(paste(i, "~", "z_age + I(z_age^2) + female + collegeed + 
                                                           z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
                                                           z_lemanc + wnneighb")), 
                                           data=subset(Data, raceethnic == "White" & year == j),
                                           family = binomial(link="logit"))
  }
}

saveRDS(ysmodels, "data/ysmodels.rds")


# That's tomorrow and that is uh

Oreilly <- rbind(tidy(Models[[4]]) %>% mutate(model = names(Models[4])),
             tidy(Models[[8]]) %>% mutate(model = names(Models[8])),
             tidy(Models[[12]]) %>% mutate(model = names(Models[12]))) %>%
  filter(term == "wnneighb")

# That's tomorrow and that is it for us today. And we will leave you with a ugh


for (i in 1:length(ryfixefmodels)) {
  Temp = tidy(ryfixefmodels[[i]]) %>%
    filter(term == "wnneighb") %>%
    mutate(model = names(ryfixefmodels[i]))
  Oreilly = bind_rows(Oreilly, Temp)
  
}

# I can't do it. We'll do it live.

for (i in 1:length(yfixefmodels)) {
  Temp = tidy(yfixefmodels[[i]]) %>%
    filter(term == "wnneighb") %>%
    mutate(model = names(yfixefmodels[i]))
  Oreilly = bind_rows(Oreilly, Temp)
  
}

# We'll do it live! Fuck it!

for (i in 1:length(ysmodels)) {
  Temp = tidy(ysmodels[[i]]) %>%
    filter(term == "wnneighb") %>%
    mutate(model = names(ysmodels[i]))
  Oreilly = bind_rows(Oreilly, Temp)
  
}

# Do it live! I'll write it and we'll do it live!

for (i in 1:length(censusryearmodels)) {
  Temp = tidy(censusryearmodels[[i]]) %>%
    filter(term == "wnneighb") %>%
    mutate(model = names(censusryearmodels[i]))
  Oreilly = bind_rows(Oreilly, Temp)
  
}

# Fucking. Thing. Sucks!

saveRDS(Oreilly,file="data/oreilly.rds")




# Ordinal models -----
Data %>%
  mutate(havedemr = carr(havedem, "1=4;2=3;3=2;4=1")) -> Data

library(ordinal)
OM1 <- clmm(factor(strongleader) ~ z_age + I(z_age^2) + female +
              collegeed + z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
              + z_lemanc  + wnneighb + (1 | year),
            data=subset(Data, raceethnic == "White"),
            link="logit")

OM2 <- clmm(factor(armyrule) ~ z_age + I(z_age^2) + female +
              collegeed + z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
              + z_lemanc  + wnneighb + (1 | year),
            data=subset(Data, raceethnic == "White"), link="logit")

OM3 <- clmm(factor(havedemr) ~ z_age + I(z_age^2) + female +
              collegeed + z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
              + z_lemanc  + wnneighb + (1 | year),
            data=subset(Data, raceethnic == "White"),link="logit")

list("sld: ordinal" = OM1,
     "ard: ordinal" = OM2,
     "hdd: ordinal" = OM3) -> ordinalmodels

saveRDS(ordinalmodels, "data/ordinalmodels.rds")

# Demimp models -----

AM13 <- lm(demimp ~ z_age + I(z_age^2) + female +
             collegeed + z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
             + z_lemanc  + wnneighb + factor(year),
           data=subset(Data, raceethnic == "White"))

AM14 <- lm(demimp ~ z_age + I(z_age^2) + female +
             collegeed + z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
             + z_lemanc  + wnneighb*collegeed + factor(year),
           data=subset(Data, raceethnic == "White"))

list("demimp: standard" = AM13,
     "demimp: interact" = AM14) -> demimpmodels

saveRDS(demimpmodels, "data/demimpmodels.rds")



Data %>%
  select_at(vars(contains("year"), contains("neighb_"))) %>%
  gather(item,y,2:ncol(.)) %>%
  group_by(item) %>%
  filter(!is.na(y)) %>%
  summarize(n = n_distinct(year)) -> neighb_years

neighb_years %>%
  filter(n == 1) %>%
  pull(item) -> oneyear_neighbitems

neighb_years %>%
  filter(n == 2) %>%
  pull(item) -> twoyear_neighbitems

neighb_years %>%
  filter(n == 4) %>%
  pull(item) -> fouryear_neighbitems


fouryearmodels = list()
for (i in dvs){
  for (j in fouryear_neighbitems){
    fouryearmodels[[paste0(i, ": ", j)]] <- bglmer(as.formula(paste(i, "~", "z_age + I(z_age^2) + female + collegeed + 
                                                           z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
                                                           z_lemanc +", j,"  + (1 | year)")), 
                                                  data=subset(Data, raceethnic == "White"), family = binomial(link="logit"),
                                                  control=glmerControl(optimizer="bobyqa",
                                                                       optCtrl=list(maxfun=2e5)))
  }
}

twoyearmodels = list()
for (i in dvs){
  for (j in twoyear_neighbitems){
    twoyearmodels[[paste0(i, ": ", j)]] <- glm(as.formula(paste(i, "~", "z_age + I(z_age^2) + female + collegeed + 
                                                           z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
                                                           z_lemanc + ", j,"  + factor(year)")), 
                                                  data=subset(Data, raceethnic == "White"), family = binomial(link="logit"))
  }
}

oneyearmodels = list()
for (i in dvs){
  for (j in oneyear_neighbitems){
    oneyearmodels[[paste0(i, ": ", j)]] <- glm(as.formula(paste(i, "~", "z_age + I(z_age^2) + female + collegeed + 
                                                           z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
                                                           z_lemanc +", j)), 
                                               data=subset(Data, raceethnic == "White"), family = binomial(link="logit"))
  }
}


purrr::flatten(list(fouryearmodels, twoyearmodels, oneyearmodels)) -> neighbmodels

saveRDS(neighbmodels, "data/neighbmodels.rds")

# Latent construct models -----


I1 <- blmer(index ~ z_age + I(z_age^2) + female + collegeed + 
             z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
             z_lemanc + wnneighb + (1 | year), data=subset(Data, raceethnic == "White"))

# Data %>%
#   select(uid, havedemr, strongleader, armyrule) %>% 
#   filter_all(all_vars(!is.na(.))) -> Index
# #  filter_at(vars(contains("a0")), all_vars(!is.na(.))) -> Autonomy
# 
# library(mirt)
# IndexM <- mirt(Index[ ,  2:ncol(Index)], model = 1,
#              itemtype = "graded", SE = TRUE, verbose = FALSE)
# 
# fscores(IndexM, full.scores = TRUE, full.scores.SE = TRUE) %>%
#   tbl_df() %>% 
#   rename(lindex = F1) %>%
#   bind_cols(Index, .) %>%
#   left_join(Data, .) -> Data


I2 <- blmer(lindex ~ z_age + I(z_age^2) + female + collegeed + 
              z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
              z_lemanc + wnneighb + (1 | year), data=subset(Data, raceethnic == "White"))


list("index: wnneighb" = I1,
     "lindex: wnneighb" = I2) -> indexmodels

saveRDS(indexmodels, "data/indexmodels.rds")


# Voice models -----


V1 <- blmer(lvoi ~ z_age + I(z_age^2) + female + collegeed + 
              z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
              wnneighb + (1 | year), data=subset(Data, raceethnic == "White"))

V2 <- blmer(lvoi ~ z_age + I(z_age^2) + female + collegeed + 
              z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
              z_laut + z_lequ + z_lcho +
               wnneighb + (1 | year), data=subset(Data, raceethnic == "White"))

list("lvoi: wnneighb" = V1,
     "lvoi: wnneighb + emanc" = V2) -> lvoimodels

saveRDS(lvoimodels, "data/lvoimodels.rds")


# separate emancvalues as IVs -----

emancmodels = lapply(setNames(dvs, paste0(dvs, ": separate emanc")), function(var) {
  form = paste(var, "~ z_age + I(z_age^2) + female +
              collegeed + z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
              + z_laut + z_lequ + z_lcho + z_lvoi + wnneighb + (1 | year)")
  bglmer(form, data=subset(Data, raceethnic == "White"), family=binomial(link = "logit"),
         control=glmerControl(optimizer="bobyqa",
                              optCtrl=list(maxfun=2e5)))
})


saveRDS(emancmodels, "data/emancmodels.rds")

# No emancvalues in the IVs -----

noemancmodels = lapply(setNames(dvs, paste0(dvs,": no emanc")), function(var) {
  form = paste(var, "~ z_age + I(z_age^2) + female +
              collegeed + z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
              +  wnneighb + (1 | year)")
  glmer(form, data=subset(Data, raceethnic == "White"), family=binomial(link = "logit"))
})


saveRDS(noemancmodels, "data/noemancmodels.rds")

# child autonomy index -----

caimodels = lapply(setNames(dvs, paste0(dvs, ": cai")), function(var) {
  form = paste(var, "~ z_age + I(z_age^2) + female +
              collegeed + z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
              + cai + wnneighb + (1 | year)")
  bglmer(form, data=subset(Data, raceethnic == "White"), family=binomial(link = "logit"),
         control=glmerControl(optimizer="bobyqa",
                              optCtrl=list(maxfun=2e5)))
})

saveRDS(caimodels, "data/caimodels.rds")

# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# library(modelr)
# 
# # First, let's do some first difference simulations for what will amount to items 1-12 in the Models object ----
# 
# # We'll need to create a newdata object, though.
# 
# data_grid(Data, .model=Models1[[1]], 
#           sld = 0, ard=0, hdd=0,
#           female = 0,
#           wnneighb = c(0,1),
#           wnw = c(0,1),
#           year = 0) %>%
#   mutate_at(vars(contains("z_")), list(~replace(., . != 0, 0))) %>%
#   mutate(neighb_immig = c(0, 0, 1,1),
#          neighb_difflang = c(0, 0, 1,1),
#          neighb_diffrace = c(0, 0, 1,1)) -> Newdat_fd
# 
# sims_fd = tibble()
# for(i in 1:12) {
#   get_sims(Models1[[i]], Newdat_fd, 1000, 8675309) %>% 
#     mutate(model = names(Models1)[i]) %>%
#     mutate(fit = boot::inv.logit(y)) -> Temp 
#   sims_fd = bind_rows(Temp, sims_fd)
# }
# 
# 
# sims_fd %>% 
#   separate(model,c("dv", "iv"), sep=": ") -> sims_fd
# 
# 
# Oreilly %>%
#   separate(model, c("dv", "model"), ": ") %>%
#   mutate(model = ifelse(model == "wnneighb", "Manuscript Results", model)) %>%
#   mutate(lwr = estimate - abs(qnorm(.05))*std.error,
#          upr = estimate + abs(qnorm(.05))*std.error)  %>%
#   mutate(dv = forcats::fct_recode(dv,
#                                   "Strong Leader" = "sld",
#                                   "Army Rule" = "ard",
#                                   "Oppose Democracy" = "hdd")) %>%
#   ggplot(.) + facet_wrap(~dv) +
#   theme_steve() +
#   coord_flip() +
#   geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
#   geom_pointrange(aes(x = model, y = estimate, ymin = lwr, # interval2
#                       ymax =upr), # interval2
#                   lwd = 1/2, position = position_dodge(width = 1/2),
#                   fill = "WHITE")
#   
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# AM4j <- glm(sld ~ z_age + I(z_age^2) + female +
#               collegeed + z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
#               + z_lemanc  + neighb_jews, data=subset(Data, raceethnic == "White"), family=binomial(link = "logit"))
# 
# AM5j <- glm(ard ~ z_age + I(z_age^2) + female +
#               collegeed + z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
#               + z_lemanc  + neighb_jews, data=subset(Data, raceethnic == "White"), family=binomial(link = "logit"))
# 
# AM6j <- glm(hdd ~ z_age + I(z_age^2) + female +
#               collegeed + z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
#               + z_lemanc  + neighb_jews, data=subset(Data, raceethnic == "White"), family=binomial(link = "logit"))
# 
# 
# AM4m <- glm(sld ~ z_age + I(z_age^2) + female +
#               collegeed + z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
#               + z_lemanc  + neighb_muslim + factor(year), data=subset(Data, raceethnic == "White"), family=binomial(link = "logit"))
# 
# AM5m <- glm(ard ~ z_age + I(z_age^2) + female +
#               collegeed + z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
#               + z_lemanc  + neighb_muslim + factor(year), data=subset(Data, raceethnic == "White"), family=binomial(link = "logit"))
# 
# AM6m <- glm(hdd ~ z_age + I(z_age^2) + female +
#               collegeed + z_ideo + I(z_ideo^2) + z_incscale + gop + dem + unemployed +
#               + z_lemanc  + neighb_muslim + factor(year), data=subset(Data, raceethnic == "White"), family=binomial(link = "logit"))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# allfits_wnneighb %>%
#   mutate(lwr = estimate - abs(qnorm(.025))*std.error,
#          upr = estimate + abs(qnorm(.05))*std.error) %>%
#   ggplot(.,aes(optim, estimate, ymin=lwr, ymax=upr)) +
#   geom_pointrange(position = position_dodge(width = .5)) + coord_flip() +
#   theme_steve() +
#   facet_wrap(~model) +
#   ylim(0, .8) +
#   geom_hline(yintercept = 0, linetype="dashed")
# 
#   ggplot(.,aes(optim, statistic)) + theme_steve() +
#   coord_flip() + geom_point() + facet_wrap(~model) +
#   ylim(0,6) +
#   geom_hline(yintercept = 1.96, linetype="dashed") +
#   ylab("z-statistic") +
#   labs(caption = "Vertical line represents a z-value of 1.96, the conventional cutoff for statistical significance.")
# 
# allfitslliks %>%
#   remove_rownames(.) %>%
#   mutate(model = c("Strong Leader", "Army Rule", "Oppose Democracy"))  %>%
#  # magrittr::set_colnames(c("nlopt: Nelder-Mead", "nlopt: BOBYQA", "Nelder-Mead",
#  #                        "nmkb", "L-BFGS-B", "nlminb", "BOBYQA", "model")) %>%
#   select(model, everything()) %>%
#   group_by(model) %>%
#   gather(Optimizer, llik, 2:ncol(.)) %>%
#   ggplot(.,aes(Optimizer, llik)) + geom_point() +
#   facet_wrap(~model) + coord_flip() + theme_steve() +
#   ylab("Log-Likelihood")
