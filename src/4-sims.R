library(modelr)

# First, let's do some first difference simulations for what will amount to items 1-12 in the Models object ----

# We'll need to create a newdata object, though.

data_grid(Data, .model=Models[[1]], 
          sld = 0, ard=0, hdd=0,
          female = 0,
          wnneighb = c(0,1),
          year = 0) %>%
  mutate_at(vars(contains("z_")), list(~replace(., . != 0, 0))) %>%
  mutate(neighb_immig = c(0,1),
         neighb_difflang = c(0,1),
         neighb_diffrace = c(0, 1)) -> Newdat_fd

sims_fd = tibble()
for(i in 1:12) {
  get_sims(Models[[i]], Newdat_fd, 1000, 8675309) %>% 
    mutate(model = names(Models)[i]) %>%
    mutate(fit = boot::inv.logit(y)) -> Temp 
  sims_fd = bind_rows(Temp, sims_fd)
}


sims_fd %>% 
  separate(model,c("dv", "iv"), sep=": ") -> sims_fd

# That was easy...
# Okay, let's do the education ones now, starting with the interaction -----

data_grid(Data, .model=Models[[13]], 
          sld = 0, ard=0, hdd=0,
          female = 0,
          nesting(wnneighb, collegeed),
          year = 0) %>%
  mutate_at(vars(contains("z_")), list(~replace(., . != 0, 0)))  -> Newdat_interact

sims_interact = tibble()
for(i in 13:15) {
  get_sims(Models[[i]], Newdat_interact, 1000, 8675309) %>% 
    mutate(model = names(Models)[i]) %>%
    mutate(fit = boot::inv.logit(y)) -> Temp 
  sims_interact = bind_rows(Temp, sims_interact)
}
Newdat_interact %>% 
  select(wnneighb, collegeed) -> to_add

cbind(sims_interact, to_add) %>%
  tbl_df() -> sims_interact

sims_interact %>% 
  separate(model,c("dv", "iv"), sep=": ") -> sims_interact



# Random slopes now ------

data_grid(Data, .model=Models[[16]], 
          sld = 0, ard=0, hdd=0,
          female = 0,
          nesting(educatrmd, wnneighb),
          year = 0) %>%
  na.omit %>%
  mutate_at(vars(contains("z_")), list(~replace(., . != 0, 0)))  -> Newdat_rs

sims_rs = tibble()
for(i in 16:18) {
  merTools::predictInterval(Models[[i]],
                            newdata=Newdat_rs, 
                            include.resid.var=F,
                            type="probability", 
                            n.sims=1000, seed=8675309,
                            returnSims = TRUE) -> hold_sims
  hold_sims %>% mutate(model = names(Models)[i]) -> hold_sims
  hold_sims %>% bind_cols(., Newdat_rs %>% select(educatrmd, wnneighb)) -> hold_sims

  # attributes(hold_sims)$sim.results %>%
  #   data.frame() %>% tbl_df() %>%
  #   bind_cols(.,Newdat_rs) %>%
  #   select(educatrmd, wnneighb, X1:X1000) %>%
  #   mutate_at(vars(contains("X")), boot::inv.logit) %>%
  #   group_by(wnneighb, educatrmd) %>%
  #   gather(sim, value, X1:X1000) %>%
  #   mutate(model = names(Models)[i]) -> extractedsims
  
 sims_rs = bind_rows(sims_rs, hold_sims)
}

sims_rs %>% 
  separate(model,c("dv", "iv"), sep=": ") -> sims_rs


list("sims_fd" = sims_fd,
     "sims_interact" = sims_interact,
     "sims_rs" = sims_rs) -> Sims

saveRDS(Sims, "data/sims.rds")
