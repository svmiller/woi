# Figures -----
# 1) fig:barraceyear :: Summary information for nobs by year/race and such -----


Data %>% 
  group_by(year, raceethnic) %>%
  summarize(sum = n()) %>%
  na.omit %>%
  mutate(labs = prettyNum(sum,big.mark=",",scientific=FALSE)) %>%
  ggplot(., aes(raceethnic, sum)) +
  theme_steve() +
  geom_bar(stat="identity", color="black", fill="#619cff", alpha=0.8) +
  geom_text(aes(label=labs), vjust=-.5, colour="black",
            position=position_dodge(.9), size=4) +
  facet_wrap(~year) +
  scale_y_continuous(limits = c(0, 1750)) +
  labs(y = "Number of Observations (by Survey Year)",
       x = "Racial/Ethnic Category") -> fig_barraceyear

# Data %>%
#   filter(raceethnic == "White") %>%
#   group_by(year) %>%
#   summarize(sum = n()) %>%
#   ungroup() %>%
#   rename(Category = year) %>%
#   mutate(facet = "Number of White Respondents by Survey Year",
#          Category = as.factor(Category)) -> yearinfo
# 
# Data %>%
#   group_by(raceethnic) %>%
#   summarize(sum = n()) %>% 
#   ungroup() %>%
#   rename(Category = raceethnic) %>%
#   mutate(facet = "Race/Ethnicity") %>% # Drop the 64 obs for which no R/E
#   na.omit() -> reinfo
# 
# bind_rows(yearinfo, reinfo) %>%
#   mutate(labs = prettyNum(sum,big.mark=",",scientific=FALSE)) %>%
#   ggplot(.,aes(Category, sum, group=facet)) + 
#   theme_steve() +
#   geom_bar(stat="identity", color="black", fill="#619cff", alpha=0.8) +
#   facet_wrap(~facet, nrow=2, scales="free") +
#   scale_y_continuous(labels = scales::comma, limits = c(0, 4900)) +
#   xlab("") + ylab("Number of Observations in the Combined Data") +
#   geom_text(aes(label=labs), vjust=-.5, colour="black",
#             position=position_dodge(.9), size=4) -> fig_barraceyear

# 2) fig:corr :: Correlation matrix -----

na.omit(descriptive) -> corrdat
names(corrdat) <- c("Strong Leader", "Oppose Democracy", "Army Rule",
                    "Age", "Female", "College Educated", "Ideology",
                    "Income Scale","Republican", "Democrat",
                    "Unemployed", "Emancipative Values",
                    "White Social Prejudice")
round(cor(corrdat), 2) -> corrdat

ggcorrplot::ggcorrplot(corrdat,
                       lab = T, hc.order = TRUE,
                       type = "lower", lab_size = 2,
                       tl.cex = 14) + 
  theme_steve() +
  theme(plot.margin = margin(0, 0, 0, 0, "cm"),
        legend.position = "none",
        #axis.text.x=element_text(size=8)
        axis.text.x=element_text(size=8, angle=45, vjust=1, hjust=1, 
                                 margin=margin(4,0,0,0)),
        axis.text.y=element_text(size=8)
  ) +
  xlab("") + ylab("") -> fig_corr

# 3) fig:optimllk :: Optimize log-likelihoods for allfit objects -----

aflist[["allfitslliks"]] %>%
  remove_rownames(.) %>%
  mutate(model = c("Strong Leader", "Army Rule", "Oppose Democracy"))  %>%
  # magrittr::set_colnames(c("nlopt: Nelder-Mead", "nlopt: BOBYQA", "Nelder-Mead",
  #                        "nmkb", "L-BFGS-B", "nlminb", "BOBYQA", "model")) %>%
  select(model, everything()) %>%
  group_by(model) %>%
  gather(Optimizer, llik, 2:ncol(.)) %>%
  ggplot(.,aes(Optimizer, llik)) + geom_point() +
  facet_wrap(~model) + coord_flip() + theme_steve() +
  ylab("Log-Likelihood") -> fig_optimllk


# 4) fig:optimwnneighb :: coefficients/standard errors for wnneighb across allfit objects. -----

aflist[["allfits_wnneighb"]] %>%
  mutate(lwr = estimate - abs(qnorm(.025))*std.error,
         upr = estimate + abs(qnorm(.025))*std.error) %>%
  ggplot(.,aes(optim, estimate, ymin=lwr, ymax=upr)) +
  geom_pointrange(position = position_dodge(width = .5)) + coord_flip() +
  theme_steve() +
  facet_wrap(~model) +
  ylim(0, .8) +
  xlab("Optimizer") + ylab("Coefficient (with 95% Intervals)") +
  geom_hline(yintercept = 0, linetype="dashed") -> fig_optimwnneighb

# 5) fig:jmseps :: Abbreviated Dot-and-Whisker Plots of the Effect of Intolerance Toward Jews and Muslims on Anti-Democratic Orientations  -----

tidy_neighb_jm = as_tibble()
for(i in 3:length(jmmodels)){
  Temp = tidy(jmmodels[[i]]) %>%
    filter(term %in%  c("neighb_muslim", "neighb_jews")) %>%
    mutate(n = nobs(jmmodels[[i]]),
           dv = names(jmmodels[i]))
  tidy_neighb_jm = bind_rows(tidy_neighb_jm, Temp)
}


tidy_neighb_jm %>%
  separate(dv, c("dv", "iv"), ": ") %>%
  mutate(dv = forcats::fct_recode(dv,
                                  "Strong Leader" = "sld",
                                  "Army Rule" = "ard",
                                  "Oppose Democracy" = "hdd")) %>%
  mutate(n = paste0("n: ", n))  %>%
  mutate(lwr = estimate - abs(qnorm(.025))*std.error,
         upr = estimate + abs(qnorm(.025))*std.error) %>%
  mutate(iv = ifelse(term == "neighb_jews", "Would Not Want\nJewish Neighbor", "Would Not Want\nMuslim Neighbor")) %>%
  mutate(x = c(1.1,1.1,1.1,2.1,2.1,2.1),
         y = 1.1) -> tidy_neighb_jm

tidy_neighb_jm %>%
  ggplot(.,aes(iv, estimate, ymin=lwr, ymax=upr, color=iv, shape=iv))  +
  theme_steve() +
  scale_colour_brewer(palette = "Set1") +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_pointrange(position = position_dodge(width = .5), size=1) + coord_flip() +
  facet_wrap(~dv) +
  geom_text(data=tidy_neighb_jm, aes(x=x, y=y, label=n), 
            colour="black", inherit.aes=FALSE, parse=FALSE, size=3) +
  labs(y = "Coefficient Estimate (with 95% Intervals)",
       x = "",
       shape = "Model",
       color = "Model",
       caption = "Results are abbreviated, faceted dot-whisker plots that communicate just the effect of the particular Social Prejudice coefficient. Full results are available in the replication file.\nNote: each dot and whisker is annotated with the number of observations from the statistical model.\nThe 'Jewish neighbor' prompt was available only in 1999. The 'Muslim neighbor' prompt is available only in 1995 and 1999.") -> fig_jmseps

#6) fig:mmseps :: Abbreviated Dot-and-Whisker Plots of the Effect of Intolerance Toward 'Militant Minorities'  -----

tidy_neighb_mm = as_tibble()

for(i in 3:length(mmmodels)){
  Temp = tidy(mmmodels[[i]]) %>%
    filter(term %in%  c("neighb_mm")) %>%
    mutate(n = nobs(mmmodels[[i]]),
           dv = names(mmmodels[i]))
  tidy_neighb_mm = bind_rows(tidy_neighb_mm, Temp)
}

tidy_neighb_mm %>%
  separate(dv, c("dv", "iv"), ": ") %>%
  mutate(dv = forcats::fct_recode(dv,
                                  "Strong Leader" = "sld",
                                  "Army Rule" = "ard",
                                  "Oppose Democracy" = "hdd")) %>%
  mutate(n = paste0("n: ", n))  %>%
  mutate(lwr = estimate - abs(qnorm(.025))*std.error,
         upr = estimate + abs(qnorm(.025))*std.error) %>%
  mutate(iv = ifelse(term == "neighb_mm", "Would Not Want\n'Militant Minority'\nas Neighbor", "hi mom!")) %>%
  mutate(x = 1.1,
         y = -.5) -> tidy_neighb_mm

tidy_neighb_mm %>%
  ggplot(.,aes(iv, estimate, ymin=lwr, ymax=upr))  +
  theme_steve() +
  scale_colour_brewer(palette = "Set1") +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_pointrange(position = position_dodge(width = .5), size=1) + coord_flip() +
  facet_wrap(~dv) +
  geom_text(data=tidy_neighb_mm, aes(x=x, y=y, label=n), 
            colour="black", inherit.aes=FALSE, parse=FALSE, size=3) +
  labs(y = "Coefficient Estimate (with 95% Intervals)",
       x = "",
       shape = "Model",
       color = "Model",
       caption = "Results are abbreviated, faceted dot-whisker plots that communicate just the effect of the particular Social Prejudice coefficient. Full results are available in the replication file.\nNote: each dot and whisker is annotated with the number of observations from the statistical model.\nThe question prompt appears in just the 2006 wave.") -> fig_mmseps

# 7) fig:oreilly :: Dot-and-Whisker Plots of the Effect of White Social Prejudice Across Multiple Specifications for Temporal/Spatial Heterogeneity ----

oreilly %>%
  separate(model, c("dv", "model"), ": ") %>%
  mutate(model = ifelse(model == "wnneighb", "Manuscript Results", model)) %>%
  mutate(lwr = estimate - abs(qnorm(.05))*std.error,
         upr = estimate + abs(qnorm(.05))*std.error)  %>%
  mutate(dv = forcats::fct_recode(dv,
                                  "Strong Leader" = "sld",
                                  "Army Rule" = "ard",
                                  "Oppose Democracy" = "hdd")) %>%
  ggplot(.) + facet_wrap(~dv) +
  theme_steve() +
  coord_flip() +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_pointrange(aes(x = model, y = estimate, ymin = lwr, # interval2
                      ymax =upr), # interval2
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  fill = "WHITE") +
  xlab("") + ylab("") -> fig_oreilly

# 8) Education-interaction and sims plot -----
# NOTE: this used to be part of the main paper, but, you know, peer review is what it is...
# Important: we're going to create two separate objects and just combine them with multiplot.
# Education-interaction plot
# Create simple dwplot
require(artyfarty)

Mdf <- rbind(tidy(Models[[13]]) %>% mutate(model = "m4"),
             tidy(Models[[14]]) %>% mutate(model = "m5"),
             tidy(Models[[15]]) %>% mutate(model = "m6")) %>%
  filter(term != "(Intercept)" & term != "sd_(Intercept).censusr:year" & 
           term != "sd_(Intercept).censusr" & term != "sd_(Intercept).year") %>%
  filter(term %in% c("wnneighb", "collegeed", "collegeed:wnneighb"))


figcap_educinteraction <- paste0("Number of observations: (Strong Leaders: ", prettyNum(nobs(Models[[13]]), big.mark=","),
                                  "; Army Rule: ", prettyNum(nobs(Models[[14]]), big.mark=","),
                                  "; Oppose Democracy: ",
                                  prettyNum(nobs(Models[[15]]), big.mark=","),").\nNote: Figure omits other coefficients to draw reader's attention to important covariates of interest in this section. See appendix for full models.")

dwplot(Mdf, dot_args = list(size = 3, aes(colour = model, shape = model))) %>%
  relabel_predictors(c(z_age = "Age",
                       `I(z_age^2)` = "Age^2",
                       female = "Female",
                       collegeed = "College Education",
                       z_ideo = "Ideology",
                       `I(z_ideo^2)` = "Ideology^2",
                       z_incscale = "Income Scale",
                       gop = "Republican",
                       dem = "Democrat",
                       unemployed = "Unemployed",
                       z_lemanc = "Emancipative Values",
                       wnneighb = "White Social Prejudice",
                       `collegeed:wnneighb` = "College Education*White Social Prejudice")) +
  theme_steve() + 
  geom_vline(xintercept = 0, colour = "grey30", linetype = 2) +
  xlab("Coefficient Estimate") +
  scale_colour_manual(labels = c("Support for a Strong Leader",
                                 "Support for Army Rule",
                                 "Oppose Having Democracy"),
                      name="Models",
                      values = pal("few_medium")) +
  scale_shape_discrete(labels = c("Support for a Strong Leader",
                                  "Support for Army Rule",
                                  "Oppose Having Democracy"),
                       name="Models") +
  labs(caption=figcap_educinteraction,
       title = "a) The Effect of Intolerance and College Education on Anti-Democratic Orientations") -> fig_interact_dw


#  make interact_sims_plot
library(ggridges)

Sims[["sims_interact"]] %>%
  group_by(dv, wnneighb, collegeed) %>%
  summarize(mean = mean(fit),
            lwr = quantile(fit, .025),
            upr = quantile(fit, .975)) %>%
  mutate(intervals = paste0(sprintf("%.3f", round(mean, 3)),"\n(",
                            sprintf("%.3f", round(lwr, 3)),", ",sprintf("%.3f", round(upr, 3)),")")) %>%
  ungroup() %>%
  mutate(dv = forcats::fct_recode(dv,
                                  "Strong Leader" = "sld",
                                  "Army Rule" = "ard",
                                  "Oppose Democracy" = "hdd")) %>%
  mutate(Category = NA,
         Category = ifelse(wnneighb == 0 & collegeed == 0, "Not College Educated,\nWhite Social Prejudice = 0", Category),
         Category = ifelse(wnneighb == 1 & collegeed == 0, "Not College Educated,\nWhite Social Prejudice = 1", Category),
         Category = ifelse(wnneighb == 0 & collegeed == 1, "College Educated,\nWhite Social Prejudice = 0", Category),
         Category = ifelse(wnneighb == 1 & collegeed == 1, "College Educated,\nWhite Social Prejudice = 1", Category)) %>%
  mutate(x = c(rep(.675, 4), rep(.3, 4), rep(.55, 4)),
         y = rep(c(3.5,1.5,4.5,2.5), 3)) -> annotate_me


Sims[["sims_interact"]] %>%
  mutate(dv = forcats::fct_recode(dv,
                                  "Strong Leader" = "sld",
                                  "Army Rule" = "ard",
                                  "Oppose Democracy" = "hdd")) %>%
  mutate(Category = NA,
         Category = ifelse(wnneighb == 0 & collegeed == 0, "Not College Educated,\nWhite Social Prejudice = 0", Category),
         Category = ifelse(wnneighb == 1 & collegeed == 0, "Not College Educated,\nWhite Social Prejudice = 1", Category),
         Category = ifelse(wnneighb == 0 & collegeed == 1, "College Educated,\nWhite Social Prejudice = 0", Category),
         Category = ifelse(wnneighb == 1 & collegeed == 1, "College Educated,\nWhite Social Prejudice = 1", Category)) %>%
  ggplot(., aes(x = fit, y = Category, fill=factor(..quantile..))) + 
  theme_steve() +
  facet_wrap(~dv, scales="free") +
  stat_density_ridges(geom = "density_ridges_gradient", 
                      calc_ecdf = TRUE, quantile_lines = TRUE,
                      quantiles = c(0.025, 0.975), alpha=0.4, scale=0.9) +
  scale_fill_manual(name = "Probability", values = c("#A6CEE3", "#1F78B4", "#A6CEE3"),
                    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")) +
  xlab("") + ylab("") + guides(fill=FALSE) +
  scale_x_continuous(breaks = seq(0, .8, by =.1)) +
  geom_text(data=annotate_me, aes(x=x, y=y, label=intervals), 
            colour="black", inherit.aes=FALSE, parse=FALSE, size=3.05) +
  labs(caption = "Note: Simulation means and 95% confidence intervals (in parentheses) annotated in top right of each ridge.",
       title = "b) Ridgeline Plot of Simulated Probabilities of Anti-Democratic Orientations") -> fig_interact_sims

# Random slopes plot -----

Sims[["sims_rs"]] %>%
  mutate(Treatment = ifelse(wnneighb == 0, "No White Social Prejudice", "White Social Prejudice")) %>%
  mutate(dv = forcats::fct_recode(dv,
                                  "Strong Leader" = "sld",
                                  "Army Rule" = "ard",
                                  "Oppose Democracy" = "hdd")) %>%
  mutate(educatrmd = forcats::fct_relevel(educatrmd,
                                          "No Formal Education, Incomplete Primary Education",
                                          "Complete Primary Education",
                                          "Did Not Finish HS",
                                          "HS Grad, No College",
                                          "Some College, No Four-Year Degree",
                                          "Bachelors or Equivalent",
                                          "Advanced University Degree")) %>%
  ggplot(.,aes(color = Treatment, shape=Treatment)) + 
  facet_wrap(~dv) + theme_steve() +
  geom_linerange(aes(x = educatrmd, ymin = lwr,
                     ymax = upr),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = educatrmd, y = fit, ymin = lwr,
                      ymax = upr),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  fill = "WHITE") + coord_flip() + scale_color_brewer(palette="Dark2") +
  xlab("") + ylab("") + theme(legend.title=element_blank()) +
  scale_y_continuous(breaks = seq(0, .6, by = .1)) -> fig_educatrmdrs

# 9) fig:lgbtseps :: Abbreviated dot-whisker plot of the effect of anti-LGBT responses  -----


tidy_neighb_lgbt = as_tibble()

for(i in 4:length(lgbtmodels)){
  Temp = tidy(lgbtmodels[[i]]) %>%
    filter(term %in%  c("neighb_lgbt")) %>%
    mutate(n = nobs(lgbtmodels[[i]]),
           dv = names(lgbtmodels[i]))
  tidy_neighb_lgbt = bind_rows(tidy_neighb_lgbt, Temp)
}

tidy_neighb_lgbt %>%
  separate(dv, c("dv", "iv"), ": ") %>%
  mutate(dv = forcats::fct_recode(dv,
                                  "Strong Leader" = "sld",
                                  "Army Rule" = "ard",
                                  "Oppose Democracy" = "hdd")) %>%
  mutate(n = paste0("n: ", n))  %>%
  mutate(lwr = estimate - abs(qnorm(.025))*std.error,
         upr = estimate + abs(qnorm(.025))*std.error) %>%
  mutate(iv = ifelse(term == "neighb_lgbt", "Would Not Want\na Gay Person\nas Neighbor", "hi mom!")) %>%
  mutate(x = 1.1,
         y = .45) -> tidy_neighb_lgbt

tidy_neighb_lgbt %>%
  ggplot(.,aes(iv, estimate, ymin=lwr, ymax=upr))  +
  theme_steve() +
  scale_colour_brewer(palette = "Set1") +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_pointrange(position = position_dodge(width = .5), size=1) + coord_flip() +
  facet_wrap(~dv) +
  geom_text(data=tidy_neighb_lgbt, aes(x=x, y=y, label=n), 
            colour="black", inherit.aes=FALSE, parse=FALSE, size=3) +
  labs(y = "Coefficient Estimate (with 95% Intervals)",
       x = "",
       shape = "Model",
       color = "Model",
       caption = "Results are abbreviated, faceted dot-whisker plots that communicate just the effect of the particular Social Prejudice coefficient. Full results are available in the replication file.\nNote: each dot and whisker is annotated with the number of observations from the statistical model.") -> fig_lgbtseps


# 10) fig:neighbseps :: Abbreviated dot-whisker plot of the effect of all neighbor responses  -----


tidy_neighb = as_tibble()

for(i in 1:length(neighbmodels)){
  Temp = tidy(neighbmodels[[i]]) %>%
    filter(grepl("neighb", term)) %>%
    mutate(n = nobs(neighbmodels[[i]]),
           dv = names(neighbmodels[i]))
  tidy_neighb = bind_rows(tidy_neighb, Temp)
}

tidy_neighb %>%
  separate(dv, c("dv", "iv"), ": ") %>%
  mutate(dv = forcats::fct_recode(dv,
                                  "Strong Leader" = "sld",
                                  "Army Rule" = "ard",
                                  "Oppose Democracy" = "hdd")) %>%
  mutate(n = paste0("n: ", n),
         plabel = paste0("p: ", round(p.value/2, 3)))  %>%
  mutate(lwr = estimate - abs(qnorm(.05))*std.error,
         upr = estimate + abs(qnorm(.05))*std.error) %>%
 #  group_by(dv) %>%
  arrange(dv, term) %>%
  ungroup() %>%
  mutate(x = rep(seq(1.3, 15.3), 3),
         y = 1.1) -> tidy_neighb # %>%
  # mutate(x = c(1.1,1.1,1.1,2.1,2.1,2.1),
  #        y = 1.1) -> tidy_neighb

tidy_neighb %>%
  mutate(iv = forcats::fct_recode(iv,
                                  "People with AIDS" = "neighb_aids",
                                  "Criminals" = "neighb_crim",
                                  "People Who Speak a Different Language" = "neighb_difflang",
                                  "Members of a Different Race" = "neighb_diffrace",
                                  "Heavy Drinkers" = "neighb_drinkers",
                                  "Drug Addicts" = "neighb_drug",
                                  "Emotionally Unstable People" = "neighb_emotion",
                                  "Political Extremists" = "neighb_extremists",
                                  "People of a Different Religion" = "neighb_diffrelig",
                                  "Immigrants/Foreign Workers" = "neighb_immig",
                                  "Jews" = "neighb_jews",
                                  "Muslims" = "neighb_muslim",
                                  "Homosexuals" = "neighb_lgbt",
                                  "Militant Minorities" = "neighb_mm",
                                  "Unmarried Couples Living Together" = "neighb_unmarried")) -> tidy_neighb

tidy_neighb %>%
  ggplot(.,aes(iv, estimate, ymin=lwr, ymax=upr))  +
  theme_steve() +
  scale_colour_brewer(palette = "Set1") +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_pointrange(position = position_dodge(width = .5), size=1) + coord_flip() +
  facet_wrap(~dv) +
  geom_text(data=tidy_neighb, aes(x=x, y=y, label=plabel), 
            colour="black", inherit.aes=FALSE, parse=FALSE, size=3) +
  labs(y = "Coefficient Estimate (with 95% Intervals)",
       x = "",
       shape = "Model",
       color = "Model",
       caption = "Results are abbreviated, faceted dot-whisker plots that communicate just the effect of the particular neighbor prompt. Full results are available in the replication file.\nNote: each dot and whisker is annotated with the number of observations from the statistical model.") -> fig_neighbseps



# 11) fig:noemanc :: Abbreviated dot-whisker plot of the effect of white social prejudice with (and without) emancipative values.

tidy_neighb_emanc = as_tibble()

for(i in 1:length(noemancmodels)){
  Temp = tidy(noemancmodels[[i]]) %>%
    filter(term %in%  c("wnneighb")) %>%
    mutate(n = nobs(noemancmodels[[i]]),
           dv = names(noemancmodels[i]))
  tidy_neighb_emanc = bind_rows(tidy_neighb_emanc, Temp)
}

Mdf <- rbind(tidy(Models[[4]]) %>% mutate(model = "m1"),
             tidy(Models[[8]]) %>% mutate(model = "m2"),
             tidy(Models[[12]]) %>% mutate(model = "m3")) %>%
  filter(term != "(Intercept)" & term != "sd_(Intercept).censusr:year" & 
           term != "sd_(Intercept).censusr" & term != "sd_(Intercept).year") %>%
  filter(term == "wnneighb") %>%
  bind_rows(., tidy_neighb_emanc) %>%
  separate(dv, c("dv", "iv"), ": ") %>%
  mutate(dv = rep(c("sld","ard","hdd"), 2),
         iv = ifelse(is.na(iv), "emanc", iv)) %>%
  mutate(n = rep(c(3452, 3433, 3421), 2)) %>%
  mutate(dv = forcats::fct_recode(dv,
                                  "Strong Leader" = "sld",
                                  "Army Rule" = "ard",
                                  "Oppose Democracy" = "hdd")) %>%
  mutate(n = paste0("n: ", n)) %>%
  mutate(lwr = estimate - abs(qnorm(.025))*std.error,
         upr = estimate + abs(qnorm(.025))*std.error) %>%
  mutate(term = "White Social Prejudice") %>%
  mutate(x = c(1.1, 1.1, 1.1, 2.1, 2.1, 2.1),
         y = .45) -> tidy_neighb_emanc



tidy_neighb_emanc %>%
  mutate(iv = ifelse(iv == "emanc", "White Social Prejudice\n(With Emancipative Values as Control)",
                     "White Social Prejudice\n(Without Emancipative Values as Control)")) %>%
  ggplot(.,aes(iv, estimate, shape=iv, ymin=lwr, ymax=upr))  +
  theme_steve() +
  scale_colour_brewer(palette = "Set1") +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_pointrange(position = position_dodge(width = .5), size=1) + coord_flip() +
  facet_wrap(~dv)  +
  #geom_text(data=tidy_neighb_emanc, aes(x=x, y=y, label=n), 
  #          colour="black", inherit.aes=FALSE, parse=FALSE, size=3) 
  theme(legend.position = "none") +
  labs(y = "Coefficient Estimate (with 95% Intervals)",
       x = "",
       shape = "",
       color = "Model",
       caption = "Results are abbreviated, faceted dot-whisker plots. Full results are available in the replication file. Note: there was no missing observations on emancipative values.\nThe number of observations are identical across both sets of models and are already communicated in Figure 2 in the mansucript.") -> fig_noemanc

# 11) fig:caimodels :: Abbreviated dot-whisker plot of the effect of white social prejudice with child autonomy index.

tidy_caimodels = as_tibble()

for(i in 1:length(caimodels)){
  Temp = tidy(caimodels[[i]]) %>%
    filter(term %in%  c("wnneighb", "cai")) %>%
    mutate(n = nobs(caimodels[[i]]),
           dv = names(caimodels[i]))
  tidy_caimodels = bind_rows(tidy_caimodels, Temp)
}

tidy_caimodels %>%
  separate(dv, c("dv", "iv"), ": ") %>%
  mutate(dv = rep(c("sld","ard","hdd"), 2),
         iv = ifelse(is.na(iv), "emanc", iv)) %>%
  mutate(dv = forcats::fct_recode(dv,
                                  "Strong Leader" = "sld",
                                  "Army Rule" = "ard",
                                  "Oppose Democracy" = "hdd")) %>%
  mutate(lwr = estimate - abs(qnorm(.025))*std.error,
         upr = estimate + abs(qnorm(.025))*std.error) %>%
  mutate(term = ifelse(term == "cai", "Child Autonomy Index", "White Social Prejudice")) %>%
  ggplot(.,aes(term, estimate, shape=term, ymin=lwr, ymax=upr))  +
  theme_steve() +
  scale_colour_brewer(palette = "Set1") +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_pointrange(position = position_dodge(width = .5), size=1) + coord_flip() +
  facet_wrap(~dv)  +
  theme(legend.position = "none") +
  labs(y = "Coefficient Estimate (with 95% Intervals)",
       x = "",
       shape = "",
       color = "Model",
       caption = "Results are abbreviated, faceted dot-whisker plots. Full results are available in the replication file. Note: there was no missing observations on emancipative values.\nThe number of observations are identical across both sets of models and are already communicated in Figure 2 in the mansucript.")  -> fig_cai
  



aFigures = mget(ls(pattern = "fig_"))

saveRDS(aFigures, file="data/aFigures.rds")
