Data %>% filter(raceethnic == "White") %>%
  select(sld, hdd, ard, age, female, collegeed, ideo, incscale, gop, dem, unemployed, lemanc, wnneighb) %>% 
  data.frame  -> descriptive

descriptive %>% tbl_df() %>%
  gather(variable, b, 1:ncol(.)) %>%
  group_by(variable) %>%
  summarize(mean = mean(b, na.rm=T),
            sd = sd(b, na.rm=T),
            min = min(b, na.rm=T),
            median = median(b, na.rm=T),
            max = max(b, na.rm=T),
            valid = sum(!is.na(b)),
            totaln= n(),
            percvalid = sum(!is.na(b))/n()) %>%
  mutate_if(is.numeric, ~round(.,3)) %>%
  mutate(percvalid = paste0(percvalid*100, "%")) %>% 
  mutate(variable = forcats::fct_recode(variable,
                                        "Strong Leader (Dummy)" = "sld",
                                        "Age" = "age",
                                        "Army Rule (Dummy)" = "ard",
                                        "College Educated" = "collegeed",
                                        "Democrat" = "dem",
                                        "Female" = "female",
                                        "Republican" = "gop",
                                        "Oppose Democracy (Dummy)" = "hdd",
                                        "Ideology" = "ideo",
                                        "Income Scale" = "incscale",
                                        "Emancipative Values" = "lemanc",
                                        "Unmployed" = "unemployed",
                                        "White Social Prejudice" = "wnneighb")) %>% 
  kable(., format="latex", booktabs = T,
        col.names=c("Variable", "Mean", "S.D.",
                       "Min.", "Median", "Max.", "N", "Total N", "Perc. Valid"),
        align = c("l","c","c","c","c","c","c","c","c"),
        label = "descript",
        caption = "Descriptive Statistics for Variables Used in Figure 2 in the Manuscript") %>%
  row_spec(0, bold=T) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) -> tab_descriptive


# Tables -----
# 1) tab:descript :: the descriptive statistics -----

# stargazer(descriptive, title="Descriptive Statistics for Variables Used in Figure 2 in the Manuscript", header=FALSE, label="tab:descript", summary.stat = c("n", "mean", "sd", "min","max"),
#           covariate.labels=c("Strong Leader (Dummy)",
#                              "Oppose Democracy (Dummy)",
#                              "Army Rule (Dummy)",
#                              "Age", "Female", "College Educated",
#                              "Ideology", "Income Scale", "Republican",
#                              "Democrat", "Unemployed", "Emancipative Values",
#                             "White Social Prejudice")) -> tab_descriptive

# 2) tab:nwmodelstab :: the non-white respondents -----


huxreg(nwmodels[[1]], nwmodels[[2]], nwmodels[[3]],
       coefs = c("Age" = "z_age",
                 "Age^2" = "I(z_age^2)",
                 "Female" = "female",
                 "College Educated" = "collegeed",
                 "Ideology" = "z_ideo",
                 "Ideology^2" = "I(z_ideo^2)",
                 "Income Scale" = "z_incscale",
                 "Republican" = "gop",
                 "Democrat" = "dem",
                 "Unemployed" = "unemployed",
                 "Emancipative Values" = "z_lemanc",
                 "Social Prejudice" = "wnneighb",
                 "sd(Year)" = "sd_(Intercept).year"),
       statistics = c(N = "nobs")) %>%
  filter_all(all_vars(!grepl('(NA)',.))) %>%
  set_caption('\\label{tab:nwmodelstab}The Covariates of Democratic Orientations of Non-White Americans in the World Values Survey (1995-2011)')  -> dd

position(dd) <- "center"
width(dd) <- .8
dd %>%
  mutate(model1 = ifelse(model1 == "(1)", "AM1", model1),
         model2 = ifelse(model2 == "(2)", "AM2", model2),
         model3 = ifelse(model3 == "(3)", "AM3", model3)) %>%
  # slice(-27) %>%
  insert_row(., "","","","", after=25) %>%
  insert_row(., "Random Effect", "", "", "", after =26) %>%
  insert_row(., "","","","", after=28) %>%
  set_italic(., 27, 1, TRUE)  %>% 
  insert_row(., "","Strong","Army","Oppose", after=1) %>%
  insert_row(., "","Leader","Rule","Democracy", after=2) %>%
  set_latex_float(., "!htbp") %>%
  set_all_borders(., 2:3, 1:4, 0, TRUE) %>%
  insert_row(., "","","","", after=3) %>%
  set_top_border(.,4, 1:4, .4, TRUE) %>% 
  set_bold(., 1, 2:4, TRUE) %>%
  set_align(1:nrow(.), 2:ncol(.), 'center')  %>%
  set_italic(., 2:3, 2:4, TRUE) -> tab_nwmodels

# 3) tab:jwmodelstab :: Jewish-Muslim responses included as wnneighbjm -----

huxreg(jmmodels[[1]], jmmodels[[2]], jmmodels[[3]],
       coefs = c("Age" = "z_age",
                 "Age^2" = "I(z_age^2)",
                 "Female" = "female",
                 "College Educated" = "collegeed",
                 "Ideology" = "z_ideo",
                 "Ideology^2" = "I(z_ideo^2)",
                 "Income Scale" = "z_incscale",
                 "Republican" = "gop",
                 "Democrat" = "dem",
                 "Unemployed" = "unemployed",
                 "Emancipative Values" = "z_lemanc",
                 "White Social Prejudice" = "wnneighbjm",
                 "sd(Year)" = "sd_(Intercept).year"),
       statistics = c(N = "nobs")) %>%
  filter_all(all_vars(!grepl('(NA)',.))) %>%
  set_caption('\\label{tab:jwmodelstab}The Covariates of Democratic Orientations of White Americans in the World Values Survey [with anti-Jews/Muslims Responses] (1995-2011)') -> dd

position(dd) <- "center"
width(dd) <- .6
# font_size(dd) <- 8

dd %>%
  mutate(model1 = ifelse(model1 == "(1)", "AM4", model1),
         model2 = ifelse(model2 == "(2)", "AM5", model2),
         model3 = ifelse(model3 == "(3)", "AM6", model3)) %>%
  # slice(-27) %>%
  insert_row(., "","","","", after=25) %>%
  insert_row(., "Random Effect", "", "", "", after =26) %>%
  insert_row(., "","","","", after=28) %>%
  set_italic(., 27, 1, TRUE)  %>% 
  insert_row(., "","Strong","Army","Oppose", after=1) %>%
  insert_row(., "","Leader","Rule","Democracy", after=2) %>%
  set_latex_float(., "!htbp") %>%
  set_all_borders(., 2:3, 1:4, 0, TRUE) %>%
  insert_row(., "","","","", after=3) %>%
  set_top_border(., 4, 1:4, .4, TRUE) %>% 
  set_bold(., 1, 2:4, TRUE) %>%
  set_align(1:nrow(.), 2:ncol(.), 'center')  %>%
  set_italic(., 2:3, 2:4, TRUE) -> dd

dd[28,1] <- "(w/ anti-Jews/Muslims Responses)"

dd %>%
  set_font_size(., 28, 1, 7, FALSE) -> tab_jmmodels

# 4) tab:lgbtmodelstab :: LGBT responses included as wnneighblgbt -----
# Note: I'm having to do this one myself. It's a new addition.

huxreg(lgbtmodels[[1]], lgbtmodels[[2]], lgbtmodels[[3]],
       coefs = c("Age" = "z_age",
                 "Age^2" = "I(z_age^2)",
                 "Female" = "female",
                 "College Educated" = "collegeed",
                 "Ideology" = "z_ideo",
                 "Ideology^2" = "I(z_ideo^2)",
                 "Income Scale" = "z_incscale",
                 "Republican" = "gop",
                 "Democrat" = "dem",
                 "Unemployed" = "unemployed",
                 "Emancipative Values" = "z_lemanc",
                 "White Social Prejudice" = "wnneighblgbt",
                 "sd(Year)" = "sd_(Intercept).year"),
       statistics = c(N = "nobs")) %>%
  filter_all(all_vars(!grepl('(NA)',.))) %>%
  set_caption('\\label{tab:lgbtmodelstab}The Covariates of Democratic Orientations of White Americans in the World Values Survey [with anti-LGBT Responses] (1995-2011)') -> dd


position(dd) <- "center"
width(dd) <- .6
# font_size(dd) <- 8

dd %>%
  mutate(model1 = ifelse(model1 == "(1)", "AM7", model1),
         model2 = ifelse(model2 == "(2)", "AM8", model2),
         model3 = ifelse(model3 == "(3)", "AM9", model3)) %>%
  # slice(-27) %>%
  insert_row(., "","","","", after=25) %>%
  insert_row(., "Random Effect", "", "", "", after =26) %>%
  insert_row(., "","","","", after=28) %>%
  set_italic(., 27, 1, TRUE)  %>% 
  insert_row(., "","Strong","Army","Oppose", after=1) %>%
  insert_row(., "","Leader","Rule","Democracy", after=2) %>%
  set_latex_float(., "!htbp") %>%
  set_all_borders(., 2:3, 1:4, 0, TRUE) %>%
  insert_row(., "","","","", after=3) %>%
  set_top_border(., 4, 1:4, .4, TRUE) %>% 
  set_bold(., 1, 2:4, TRUE) %>%
  set_align(1:nrow(.), 2:ncol(.), 'center')  %>%
  set_italic(., 2:3, 2:4, TRUE) -> dd

dd[28,1] <- "(w/ anti-LGBT Responses)"

dd %>%
  set_font_size(., 28, 1, 7, FALSE) -> tab_lgbtmodels


# 5) tab:mmmodelstab :: militant minority responses included as wnneighbmm -----

huxreg(mmmodels[[1]], mmmodels[[2]], mmmodels[[3]],
       coefs = c("Age" = "z_age",
                 "Age^2" = "I(z_age^2)",
                 "Female" = "female",
                 "College Educated" = "collegeed",
                 "Ideology" = "z_ideo",
                 "Ideology^2" = "I(z_ideo^2)",
                 "Income Scale" = "z_incscale",
                 "Republican" = "gop",
                 "Democrat" = "dem",
                 "Unemployed" = "unemployed",
                 "Emancipative Values" = "z_lemanc",
                 "White Social Prejudice" = "wnneighbmm",
                 "sd(Year)" = "sd_(Intercept).year"),
       statistics = c(N = "nobs")) %>%
  filter_all(all_vars(!grepl('(NA)',.))) %>%
  set_caption("\\label{tab:mmmodelstab}The Covariates of Democratic Orientations of White Americans in the World Values Survey [with 'Militant Minority' Responses] (1995-2011)") -> dd

position(dd) <- "center"
width(dd) <- .6
# font_size(dd) <- 8

dd %>%
  mutate(model1 = ifelse(model1 == "(1)", "AM10", model1),
         model2 = ifelse(model2 == "(2)", "AM11", model2),
         model3 = ifelse(model3 == "(3)", "AM12", model3)) %>%
  # slice(-27) %>%
  insert_row(., "","","","", after=25) %>%
  insert_row(., "Random Effect", "", "", "", after =26) %>%
  insert_row(., "","","","", after=28) %>%
  set_italic(., 27, 1, TRUE)  %>% 
  insert_row(., "","Strong","Army","Oppose", after=1) %>%
  insert_row(., "","Leader","Rule","Democracy", after=2) %>%
  set_latex_float(., "!htbp") %>%
  set_all_borders(., 2:3, 1:4, 0, TRUE) %>%
  insert_row(., "","","","", after=3) %>%
  set_top_border(., 4, 1:4, .4, TRUE) %>% 
  set_bold(., 1, 2:4, TRUE) %>%
  set_align(1:nrow(.), 2:ncol(.), 'center')  %>%
  set_italic(., 2:3, 2:4, TRUE) -> dd

dd[28,1] <- "(w/ anti-'Militant Minority' Responses)"

dd %>%
  set_font_size(., 28, 1, 7, FALSE) -> tab_mmmodels


# 6) tab:censusyeartab :: Region, Year, Region-Year random effects -----


huxreg(censusryearmodels[[1]], censusryearmodels[[2]], censusryearmodels[[3]],
       coefs = c("Age" = "z_age",
                 "Age^2" = "I(z_age^2)",
                 "Female" = "female",
                 "College Educated" = "collegeed",
                 "Ideology" = "z_ideo",
                 "Ideology^2" = "I(z_ideo^2)",
                 "Income Scale" = "z_incscale",
                 "Republican" = "gop",
                 "Democrat" = "dem",
                 "Unemployed" = "unemployed",
                 "Emancipative Values" = "z_lemanc",
                 "White Social Prejudice" = "wnneighb",
                 "sd(Year)" = "sd_(Intercept).year",
                 "sd(Census Region)" = "sd_(Intercept).censusr",
                 "sd(Census Region:Year)" = "sd_(Intercept).censusr:year"),
       statistics = c(N = "nobs")) %>%
  filter_all(all_vars(!grepl('(NA)',.))) %>%
  set_caption("\\label{tab:censusyeartab}The Covariates of Democratic Orientations of White Americans in the World Values Survey [with Spatial-Temporal Random Effects] (1995-2011)") -> dd

position(dd) <- "center"
width(dd) <- .75
# font_size(dd) <- 8

dd %>%
  mutate(model1 = ifelse(model1 == "(1)", "AM13", model1),
         model2 = ifelse(model2 == "(2)", "AM14", model2),
         model3 = ifelse(model3 == "(3)", "AM15", model3)) %>%
  # slice(-31) %>%
  # slice(-29) %>%
  # slice(-27) %>%
  insert_row(., "","","","", after=25) %>%
  insert_row(., "Random Effect", "", "", "", after =26) %>%
  insert_row(., "","","","", after=30) %>%
  set_italic(., 27, 1, TRUE)  %>% 
  insert_row(., "","Strong","Army","Oppose", after=1) %>%
  insert_row(., "","Leader","Rule","Democracy", after=2) %>%
  set_latex_float(., "!htbp") %>%
  set_all_borders(., 2:3, 1:4, 0, TRUE) %>%
  insert_row(., "","","","", after=3) %>%
  set_top_border(., 4, 1:4, .4, TRUE) %>% 
  set_bold(., 1, 2:4, TRUE) %>%
  set_align(1:nrow(.), 2:ncol(.), 'center')  %>%
  set_italic(., 2:3, 2:4, TRUE) -> tab_censusyear

# 7) tab:ordinaltab :: Ordinal models -----

library(texreg)
tab_ordinal <- capture.output(texreg(list(ordinalmodels[[1]], ordinalmodels[[2]], ordinalmodels[[3]]), digits=3,
                                    caption="Ordinal Mixed Effects Models of Democratic Orientations of White Americans",
                                    custom.coef.names=c("Age", "Age-squared",
                                                        "Female", "College Educated",
                                                        "Ideology", "Ideology-squared",
                                                        "Income Scale", "Republican",
                                                        "Democrat", "Unemployed",
                                                        "Emancipative Values",
                                                        "White Social Prejudice",
                                                        "1|2", "2|3", "3|4"),
                                    label = "tab:ordinaltab")
)

tab_ordinal[6] <- " & \\textbf{OM1} & \\textbf{OM2} & \\textbf{OM3} \\\\"

# 8) tab:demimptab :: Importance of Democracy -----

huxreg(demimpmodels[[1]], demimpmodels[[2]],
       coefs = c("Age" = "z_age",
                 "Age^2" = "I(z_age^2)",
                 "Female" = "female",
                 "College Educated" = "collegeed",
                 "Ideology" = "z_ideo",
                 "Ideology^2" = "I(z_ideo^2)",
                 "Income Scale" = "z_incscale",
                 "Republican" = "gop",
                 "Democrat" = "dem",
                 "Unemployed" = "unemployed",
                 "Emancipative Values" = "z_lemanc",
                 "White Social Prejudice" = "wnneighb",
                 "White Social Prejudice*College Education" = "collegeed:wnneighb",
                 "Year = 2011" = "factor(year)2011")) %>%
  filter_all(all_vars(!grepl('(NA)',.))) %>%
  set_caption("\\label{tab:demimptab}The Covariates of the Importance of Living in a Democracy for White Americans in the World Values Survey (2006, 2011)") -> dd


position(dd) <- "center"
width(dd) <- .75
# number_format(dd) <- NA
# font_size(dd) <- 8

dd %>%
  mutate(model1 = ifelse(model1 == "(1)", "AM16", model1),
         model2 = ifelse(model2 == "(2)", "AM17", model2)) %>%
  set_latex_float(., "!htbp") %>%
  set_align(1:nrow(.), 2:ncol(.), 'center')  %>%
  set_number_format(., 28, 1, NA, byrow = FALSE) -> tab_demimp


# cat(tab_ordinal, sep="\n")


# 9) tab:index :: index measures of blahblahblahblah this is more busy work -----

tidy(indexmodels[[1]]) %>%
  mutate(n = nobs(indexmodels[[1]]),
         p.value = 2*pt(abs(statistic), n-1, lower=FALSE)) -> tidyIM1

tidy(indexmodels[[2]]) %>%
  mutate(n = nobs(indexmodels[[2]]),
         p.value = 2*pt(abs(statistic), n-1, lower=FALSE)) -> tidyIM2

huxreg(tidyIM1, tidyIM2,
       coefs = c("Age" = "z_age",
                 "Age^2" = "I(z_age^2)",
                 "Female" = "female",
                 "College Educated" = "collegeed",
                 "Ideology" = "z_ideo",
                 "Ideology^2" = "I(z_ideo^2)",
                 "Income Scale" = "z_incscale",
                 "Republican" = "gop",
                 "Democrat" = "dem",
                 "Unemployed" = "unemployed",
                 "Emancipative Values" = "z_lemanc",
                 "White Social Prejudice" = "wnneighb")) %>%
  mutate(model1 = ifelse(names == "N", "3368", model1),
         model2 = ifelse(names == "N", "3368", model2)) %>%
  filter_all(all_vars(!grepl('(NA)',.))) %>%
  set_caption("\\label{tab:index}The Covariates of (Indexed) Democratic Orientations of White Americans in the World Values Survey (1995-2011)") -> dd

       

# huxreg(indexmodels[[1]], indexmodels[[2]],
#        coefs = c("Age" = "z_age",
#                  "Age^2" = "I(z_age^2)",
#                  "Female" = "female",
#                  "College Educated" = "collegeed",
#                  "Ideology" = "z_ideo",
#                  "Ideology^2" = "I(z_ideo^2)",
#                  "Income Scale" = "z_incscale",
#                  "Republican" = "gop",
#                  "Democrat" = "dem",
#                  "Unemployed" = "unemployed",
#                  "Emancipative Values" = "z_lemanc",
#                  "White Social Prejudice" = "wnneighb"),
#        statistics = c(N = "nobs")) 
#   filter_all(all_vars(!grepl('(NA)',.))) %>%
#   set_caption("\\label{tab:index}The Covariates of (Indexed) Democratic Orientations of White Americans in the World Values Survey (1995-2011)") -> dd
# 

position(dd) <- "center"
width(dd) <- .75
# caption_pos(dd) <- "topcenter"
# number_format(dd) <- NA
# font_size(dd) <- 8

dd %>%
  mutate(model1 = ifelse(model1 == "(1)", "Additive Index", model1),
         model2 = ifelse(model2 == "(2)", "Latent Estimate", model2)) %>%
  set_latex_float(., "!htbp") %>%
  set_align(1:nrow(.), 2:ncol(.), 'center')  -> tab_index




# 10) tab:voice :: voice as DV -----

tidy(lvoimodels[[1]]) %>%
  mutate(n = nobs(lvoimodels[[1]]),
         p.value = 2*pt(abs(statistic), n-1, lower=FALSE)) -> tidyVM1

tidy(lvoimodels[[2]]) %>%
  mutate(n = nobs(lvoimodels[[2]]),
         p.value = 2*pt(abs(statistic), n-1, lower=FALSE)) -> tidyVM2


huxreg(tidyVM1, tidyVM2,
       coefs = c("Age" = "z_age",
                 "Age^2" = "I(z_age^2)",
                 "Female" = "female",
                 "College Educated" = "collegeed",
                 "Ideology" = "z_ideo",
                 "Ideology^2" = "I(z_ideo^2)",
                 "Income Scale" = "z_incscale",
                 "Republican" = "gop",
                 "Democrat" = "dem",
                 "Unemployed" = "unemployed",
                 "Autonomy Values" = "z_laut",
                 "Equality Values" = "z_lequ",
                 "Choice Values" = "z_lcho",
                 "White Social Prejudice" = "wnneighb")) %>%
  filter_all(all_vars(!grepl('(NA)',.))) %>%
  mutate(model1 = ifelse(names == "N", "3405", model1),
         model2 = ifelse(names == "N", "3392", model2)) %>%
  set_caption("\\label{tab:lvoi}The Covariates of Voice Values of White Americans in the World Values Survey (1995-2011)") -> dd

position(dd) <- "center"
width(dd) <- .75
# caption_pos(dd) <- "topcenter"
# number_format(dd) <- NA
# font_size(dd) <- 8

dd %>%
  mutate(model1 = ifelse(model1 == "(1)", "AM20", model1),
         model2 = ifelse(model2 == "(2)", "AM21", model2)) %>%
  set_latex_float(., "!htbp") %>%
  set_align(1:nrow(.), 2:ncol(.), 'center')  %>%
  set_number_format(., 28, 1, NA, byrow = FALSE) -> tab_lvoi



# 10) tab:emanc :: emanc separate


huxreg(emancmodels[[1]], emancmodels[[2]], emancmodels[[3]],
       coefs = c("Age" = "z_age",
                 "Age^2" = "I(z_age^2)",
                 "Female" = "female",
                 "College Educated" = "collegeed",
                 "Ideology" = "z_ideo",
                 "Ideology^2" = "I(z_ideo^2)",
                 "Income Scale" = "z_incscale",
                 "Republican" = "gop",
                 "Democrat" = "dem",
                 "Autonomy Values" = "z_laut",
                 "Equality Values" = "z_lequ",
                 "Choice Values" = "z_lcho",
                 "Voice Values" = "z_lvoi",
                 "White Social Prejudice" = "wnneighb",
                 "sd(Year)" = "sd_(Intercept).year"),
       statistics = c(N = "nobs")) %>%
  filter_all(all_vars(!grepl('(NA)',.))) %>%
  set_caption('\\label{tab:emanc}The Covariates of Anti-Democratic Orientations of White Americans in the World Values Survey (1995-2011)')  -> dd

position(dd) <- "center"
width(dd) <- .8
dd %>%
  mutate(model1 = ifelse(model1 == "(1)", "AM22", model1),
         model2 = ifelse(model2 == "(2)", "AM23", model2),
         model3 = ifelse(model3 == "(3)", "AM24", model3)) %>%
  # slice(-27) %>%
  insert_row(., "","","","", after=29) %>%
  insert_row(., "Random Effect", "", "", "", after =30)  %>%
  insert_row(., "","","","", after=32) %>%
 # set_italic(., 30, 1, TRUE)  
  insert_row(., "","Strong","Army","Oppose", after=1) %>%
  insert_row(., "","Leader","Rule","Democracy", after=2) %>%
  set_latex_float(., "!htbp") %>%
  set_all_borders(., 2:3, 1:4, 0, TRUE) %>%
  insert_row(., "","","","", after=3) %>%
  set_top_border(.,4, 1:4, .4, TRUE) %>% 
  set_bold(., 1, 2:4, TRUE) %>%
  set_align(1:nrow(.), 2:ncol(.), 'center')  %>%
  set_italic(., 2:3, 2:4, TRUE) -> tab_emanc

aTables = mget(ls(pattern = "tab_"))

saveRDS(aTables, file="data/aTables.rds")
