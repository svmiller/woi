# Start with some background information like Census region -----

USA %>%
  mutate(uid = seq(1, nrow(.)),
         region = ifelse(x048wvs == -2, NA, x048wvs - 840000),
         region = ifelse(region >= 9, 9, region),
         censusd = dplyr::recode(region,
                                 "1" = "New England",
                                 "2" = "Middle Atlantic",
                                 "3" = "South Atlantic",
                                 "4" = "East South Central",
                                 "5" = "West South Central",
                                 "6" = "East North Central",
                                 "7" = "West North Central",
                                 "8" = "Mountain",
                                 "9" = "Pacific"),
         censusr = dplyr::recode(region,
                                 "1" = "Northeast",
                                 "2" = "Northeast",
                                 "3" = "South",
                                 "4" = "South",
                                 "5" = "South",
                                 "6" = "Midwest",
                                 "7" = "Midwest",
                                 "8" = "West",
                                 "9" = "West")) -> USA 

# Let's get race/ethnic variable now. -----

USA %>%
  mutate(wave = s002,
         wvsccode = s003,
         year = s020,
         raceethnic = ifelse(x051 == -2, NA, x051),
         # WVS *really* made a meal of this variable.
         raceethnic = dplyr::recode(raceethnic,
                                    "200" = "Black", # "Black African"
                                    "1250" = "Hispanic", # Spanish/Hispanic
                                    "1400" = "White",
                                    "8000" = "Other",
                                    "8001" = "Mixed Race",
                                    "840002" = "White", # White, not-Hispanic
                                    "840003" = "Black", # Black, not-Hispanic
                                    "840004" = "Other", # Other, not-Hispanic
                                    "840005" = "Hispanic", # Hispanic
                                    "840006" = "Mixed Race", # 2+ Races, Non-Hispanic
                                    "840007" = "Other", # South Asian,
                                    "840008" = "Other", # East Asian
                                    "840009" = "Other", # Arabic
                                    "840010" = "White",
                                    "840011" = "Black",
                                    "840012" = "Hispanic"),
         white = ifelse(raceethnic == "White", 1, 0),
         black = ifelse(raceethnic == "Black", 1, 0),
         hispanic = ifelse(raceethnic == "Hispanic", 1, 0)) -> USA


# Party now... -----

USA %>%
  mutate(party = NA,
         party = ifelse(e179wvs == 5, "Other", party),
         party = ifelse(e179wvs == 840001, "Republican", party),
         party = ifelse(e179wvs == 840002, "Democrat", party),
         party = ifelse(e179wvs == 840003, "Independent", party),
         party = ifelse(e179wvs == 840004, "Libertarian", party),
         party = ifelse(e179wvs == 840005, "Reform", party),
         gop = ifelse(party == "Republican", 1, 0),
         dem = ifelse(party == "Democrat", 1, 0)) -> USA

# The DVs... -----

USA %>%
  mutate(strongleader = carr(e114, "-5:-1=NA; 1=4; 2=3; 3=2; 4=1"),
         armyrule = carr(e115, "-5:-1=NA; 1=4; 2=3; 3=2; 4=1"),
         expertdecision = carr(e116,  "-5:-1=NA; 1=4; 2=3; 3=2; 4=1"),
         havedem = carr(e117, "-5:-1=NA; 1=4; 2=3; 3=2; 4=1"),
         havedemr = carr(e117, "-5:-1=NA"),
         index = havedemr + armyrule + strongleader,
         demimp = carr(e235, "-5:-1=NA")) -> USA

# Okay, let's create a quick index

USA %>%
  select(uid, havedemr, strongleader, armyrule) %>% 
  filter_all(all_vars(!is.na(.))) -> Index

IndexM <- mirt(Index[ ,  2:ncol(Index)], model = 1,
               itemtype = "graded", SE = TRUE, verbose = FALSE)

fscores(IndexM, full.scores = TRUE, full.scores.SE = TRUE) %>%
  tbl_df() %>% 
  rename(lindex = F1,
         se_lindex = SE_F1) %>%
  bind_cols(Index, .) %>%
  select(uid, lindex, se_lindex) %>%
  left_join(USA, .) -> USA




# Some IVs... ------

USA %>%
  mutate(age = carr(x003, "-5:-1=NA"),
         female = carr(x001, "-5:-1=NA; 1=0; 2=1"),
         unemployed = carr(x028, "-5:-1=NA; 1:6=0; 7=1; 8=0"),
         satisfin = ifelse(c006 < 0, NA, c006-1),
         ideo =  ifelse(e033 < 0, NA, e033-1),
         socialclass = carr(x045, "-5:-1=NA; 1=4; 2=3; 3=2; 4=1; 5=0"),
         incscale = ifelse(x047 < 0, NA, x047-1),
         collegeed = ifelse(educatrmd %in% c("Bachelors or Equivalent",
                                             "Advanced University Degree"), 1, 0)) -> USA


# Get emancipative values. -----

USA %>%
  mutate(emancvalues = y020,
         autonomy = carr(y021,  "-5:-1=NA"),
         equality = carr(y022,  "-5:-1=NA"),
         choice = carr(y023,  "-5:-1=NA"),
         voice = carr(y024,  "-5:-1=NA")) -> USA

# dat %>% filter_all(any_vars(!is.na(.)))
# Autonomy

USA %>%
  select(uid, autonomy, a029, a034, a042) %>% 
  rename(kid_ind = a029,
         kid_imag = a034,
         kid_obed = a042) %>%
  mutate(kid_obed = carr(kid_obed, "1=0;0=1")) %>%
  filter_all(all_vars(!is.na(.))) -> Autonomy
#  filter_at(vars(contains("a0")), all_vars(!is.na(.))) -> Autonomy

AutM <- mirt(Autonomy[ ,  3:ncol(Autonomy)], model = 1,
             itemtype = "graded", SE = TRUE, verbose = FALSE)

fscores(AutM, full.scores = TRUE, full.scores.SE = TRUE) %>%
  tbl_df() %>% 
  rename(laut = F1,
         se_laut = SE_F1) %>%
  bind_cols(USA, .) -> USA

# Equality
USA %>%
  select(uid, equality, c001, d059, d060) %>%
  mutate_at(vars(contains("0")), list(~replace(., . < 0, NA))) %>%
  # filter(is.na(c001) & is.na(d059) & is.na(d060))
  filter_at(vars(contains("0")), any_vars(!is.na(.))) %>%
  rename(menjob = c001,
         menleaders = d059,
         boycollege = d060) -> Equality

EquM <- mirt(Equality[ ,  3:ncol(Equality)], model = 1,
             itemtype = "graded", SE = TRUE, verbose = FALSE)

fscores(EquM, full.scores = TRUE, full.scores.SE = TRUE) %>%
  tbl_df() %>%
  rename(lequ = F1,
         se_lequ = SE_F1) %>%
  bind_cols(Equality, .) %>%
  #  summarize(cor = cor(equality, lequ, use="complete.obs"))
  left_join(USA, .) -> USA


# Choice
USA %>%
  select(uid, choice, f118, f120, f121) %>%
  mutate_at(vars(contains("f")), list(~replace(., . < 0, NA))) %>%
  filter_at(vars(contains("f")), any_vars(!is.na(.))) %>% 
  rename(hj = f118,
         aj = f120,
         dj = f121) -> Choice

ChoM <- mirt(Choice[ ,  3:ncol(Choice)], model = 1,
             itemtype = "graded", SE = TRUE, verbose = FALSE)

fscores(ChoM, full.scores = TRUE, full.scores.SE = TRUE) %>%
  tbl_df() %>%
  rename(lcho = F1,
         se_lcho = SE_F1) %>%
  bind_cols(Choice, .) %>%
  #  summarize(cor = cor(equality, lequ, use="complete.obs"))
  left_join(USA, .) -> USA


# Voice

USA %>%
  select(uid, voice, e001, e002, e003, e004) %>%
  mutate_at(vars(contains("e0")), list(~replace(., . < 0, NA))) %>%
  mutate(acsay = NA,
         acsay = ifelse(e001 == 3, 2, acsay),
         acsay = ifelse(e002 == 3, 1, acsay),
         acsay = ifelse(e001 != 3 & e002 != 3 & !is.na(e001), 0, acsay),
         apsay = NA,
         apsay = ifelse((e003 == 2  & e004 == 4) | (e003 == 4  & e004 == 2),
                        3, apsay),
         apsay = ifelse((e003 == 2  & e004 != 4) | (e003 == 4  & e004 != 2),
                        2, apsay),
         apsay = ifelse((e003 != 2  & e004 == 4) | (e003 != 4  & e004 == 2),
                        1, apsay),
         apsay = ifelse((e003 != 2  & e004 != 4) & (e003 != 4  & e004 != 2),
                        0, apsay)) %>%
  filter_at(vars(contains("say")), any_vars(!is.na(.))) -> Voice

VoiM <- mirt(Voice[ ,  7:ncol(Voice)], model = 1,
             itemtype = "graded", SE = TRUE, verbose = FALSE)

fscores(VoiM, full.scores = TRUE, full.scores.SE = TRUE) %>%
  tbl_df() %>%
  rename(lvoi = F1,
         se_lvoi = SE_F1) %>%
  bind_cols(Voice, .) %>%
  #  summarize(cor = cor(equality, lequ, use="complete.obs"))
  left_join(USA, .) -> USA

# Emancipative values, wrapping up
# duplicate emancvalues

USA %>%
  select(uid, emancvalues, laut, lequ, lcho, lvoi) %>%
  mutate(lemanc = (1/4)*(laut + lequ + lcho + lvoi)) -> Emanc

A1 <- lm(lemanc ~ lequ + lcho + lvoi, data=Emanc) # missing laut
A2 <- lm(lemanc ~ laut + lcho + lvoi, data=Emanc) # missing lequ
A3 <- lm(lemanc ~ laut + lequ + lvoi, data=Emanc) # missing lcho
A4 <- lm(lemanc ~ laut + lequ + lcho, data=Emanc) # missing lvoi
A1df <- tidy(A1)
A2df <- tidy(A2)
A3df <- tidy(A3)
A4df <- tidy(A4)


Emanc$lemanc <- as.numeric(with(Emanc, ifelse(is.na(laut) & is.na(lemanc),
                                   A1df[1,2] + A1df[2,2]*lequ +
                                     A1df[3,2]*lcho + A1df[4,2]*lvoi, lemanc)))

Emanc$lemanc <- as.numeric(with(Emanc, ifelse(is.na(lequ) & is.na(lemanc),
                                   A2df[1,2] + A2df[2,2]*laut +
                                     A2df[3,2]*lcho + A2df[4,2]*lvoi, lemanc)))

Emanc$lemanc <- as.numeric(with(Emanc, ifelse(is.na(lcho) & is.na(lemanc),
                                   A3df[1,2] + A3df[2,2]*laut +
                                     A3df[3,2]*lequ + A3df[4,2]*lvoi, lemanc)))

Emanc$lemanc <- as.numeric(with(Emanc, ifelse(is.na(lvoi) & is.na(lemanc),
                                   A4df[1,2] + A4df[2,2]*laut +
                                     A4df[3,2]*lequ + A4df[4,2]*lcho, lemanc)))

Emanc %>% 
  select(uid, lemanc) %>%
  left_join(USA, .) -> USA


# Neighbor questions ------

USA %>%
  dplyr::select(uid, a124_01:a124_61) %>%
  mutate_at(vars(contains("a124")), list(~replace(., . < 0, -9))) -> Neighb

# Neighb[,2:ncol(Neighb)] <- sapply(Neighb[,2:ncol(Neighb)],
#                                   function(x)ifelse(x<=-1,-9,x))

# What's remaining: 42, 43, 45, 01:10, 12:14, 18
# 01: criminals, 02: different race, 03: heavy drinkers, 04: emotionally unstable people, 
# 05: muslims, 06: immigrants/foreign workers, 07: people w/ AIDS, 08: drug addicts, 
# 09: homosexuals, 10: jews, 12, people of different religion, 13: people of same religion, 
# 14: militant minority, 18: political extremists, 42: unmarried couples living together, 
# 43: people who speak a different language

Neighb %>%
  #  mutate(wnneighb  = ifelse(a124_02 == 1 | a124_05 == 1 | a124_06 == 1 | 
  #                           a124_10 == 1 | a124_43 == 1, 1, 0)) %>%
  rename(neighb_crim = a124_01,
         neighb_diffrace = a124_02,
         neighb_drinkers = a124_03,
         neighb_emotion = a124_04,
         neighb_muslim = a124_05,
         neighb_immig = a124_06,
         neighb_aids = a124_07,
         neighb_drug = a124_08,
         neighb_lgbt = a124_09,
         neighb_jews = a124_10,
         neighb_diffrelig = a124_12,
         neighb_samerelig = a124_13,
         neighb_mm = a124_14,
         neighb_extremists = a124_18,
         neighb_unmarried = a124_42,
         neighb_difflang = a124_43) %>%
  dplyr::select(uid,  neighb_crim:neighb_jews, neighb_diffrelig:neighb_mm,
                neighb_extremists,neighb_unmarried, neighb_difflang ) -> Neighb

Neighb %>%
  mutate(wnneighb = ifelse(neighb_diffrace == 1 | 
                             neighb_immig == 1 | 
                             neighb_difflang == 1, 1, 0),
         wnneighblgbt = ifelse(neighb_diffrace == 1 | 
                             neighb_immig == 1 | 
                             neighb_difflang == 1 | 
                             neighb_lgbt == 1, 1, 0),
         wnneighbjm = ifelse(neighb_diffrace == 1 | neighb_muslim == 1 |
                               neighb_immig == 1 | neighb_jews == 1 | 
                               neighb_difflang == 1, 1, 0),
         wnneighbjmmm = ifelse(neighb_diffrace == 1 | neighb_muslim == 1 |
                                 neighb_immig == 1 | neighb_jews == 1 | 
                                 neighb_difflang == 1 | neighb_mm == 1, 1, 0),
         wnneighbmm = ifelse(neighb_diffrace == 1 | 
                               neighb_immig == 1 | 
                               neighb_difflang == 1 | neighb_mm == 1, 1, 0)) -> Neighb

# Neighb[,2:ncol(Neighb)] <- sapply(Neighb[,2:ncol(Neighb)],
#                                    function(x)ifelse(x<=-1,NA,x))

Neighb %>%
  mutate_at(vars(-uid), list(~replace(., . < 0, NA))) -> Neighb

USA <- left_join(USA, Neighb)

USA %>%
  dplyr::select(uid, educatrmd, region:wnneighbmm, y003) -> Data

Data %>% rename(cai = y003) %>%
  mutate(cai = cai*-1) -> Data

Data %>%
  mutate(z_age = r2sd(age),
         z_ideo = r2sd(ideo),
         z_lemanc = r2sd(lemanc),
         z_laut = r2sd(laut),
         z_lequ = r2sd(lequ),
         z_lcho = r2sd(lcho),
         z_lvoi = r2sd(lvoi),
         z_incscale = r2sd(incscale),
         sld = carr(strongleader, "1:2=0; 3:4=1"),
         ard = carr(armyrule, "1:2=0; 3:4=1"),
         hdd = carr(havedem, "1:2=1; 3:4=0")) -> Data

saveRDS(Data, "data/data.rds")
