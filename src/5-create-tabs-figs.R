# Simple bar plot -----

Data %>% select(neighb_crim:neighb_difflang) %>%
  summarise_if(is.numeric, mean, na.rm=T) %>%
  gather(Category, Value, neighb_crim:neighb_difflang) %>%
  arrange(-Value) %>% na.omit() %>%
  mutate(Group = c("Drug Addicts", "Heavy Drinkers", "Criminals",
                   "Emotionally Unstable People", "Political Extremists",
                   "Homosexuals", "People With AIDS", "Militant Minorities",
                   "People Who Speak a Different Language",
                   "Immigrants/Foreign Workers", "Muslims","Jews",
                   "Unmarried Couples Living Together", "Members of a Different Race",
                   "People of a Different Religion"),
         labs = paste0(round(Value*100, 1),"%")) %>%
  ggplot(.,aes(reorder(Group, -Value), Value)) +  theme_steve_web() +
  geom_bar(stat="identity", alpha=0.8, color="black", fill="#619cff") +
  geom_text(aes(label=labs),hjust = -0.1, 
            position = position_dodge(width = .9), size=3.1, family="Open Sans") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by=.1), limits=c(0,1)) +
  coord_flip() +
  ylab("") + xlab("") +
  labs(caption = "Note: no white respondent in the data listed ''people of the same religion'' as an unwelcome potential neighbor.") -> fig_bar

# Dot-whisker -----
library(artyfarty)
library(dotwhisker)
library(ggridges)

Mdf <- rbind(tidy(Models[[4]]) %>% mutate(model = "m1"),
             tidy(Models[[8]]) %>% mutate(model = "m2"),
             tidy(Models[[12]]) %>% mutate(model = "m3")) %>%
  filter(term != "(Intercept)" & term != "sd_(Intercept).censusr:year" & 
           term != "sd_(Intercept).censusr" & term != "sd_(Intercept).year")


fig_dw_cap <- paste0("Number of observations: (Model 1: ", prettyNum(nobs(Models[[4]]), big.mark=","),
                  "; Model 2: ", prettyNum(nobs(Models[[8]]), big.mark=","),
                  "; Model 3: ", prettyNum(nobs(Models[[12]]), big.mark=","),")")

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
                       wnneighb = "White Social Prejudice")) +
  theme_steve_web() +
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
  labs(caption=fig_dw_cap)  -> fig_dw

# Simulated first differences -----

Sims[["sims_fd"]] %>%
  group_by(dv, iv, sim) %>%
  mutate(diff = diff(fit)) %>%
  distinct(sim,dv, iv, diff) %>%
  mutate(neg = ifelse(diff < 0, 1, 0)) %>%
  group_by(dv, iv) %>%
  summarize(n=sum(neg),
            mean = mean(diff),
            lwr = quantile(diff, .025),
            upr = quantile(diff, .975)) %>%
  ungroup() %>%
  mutate(Category = forcats::fct_recode(iv,
                                        "Members\nof a\nDifferent Race" = "neighb_diffrace",
                                        "White\nSocial\nPrejudice" = "wnneighb",
                                        "Immigrants/\nForeign Workers" = "neighb_immig",
                                        "People Who\nSpeak a\nDifferent Language" = "neighb_difflang"
  ),
  dv = forcats::fct_recode(dv,
                           "Strong Leader" = "sld",
                           "Army Rule" = "ard",
                           "Oppose Democracy" = "hdd"),
  perc = round(n/1000, 3),
  label = paste0(sprintf("%.3f", round(mean, 3)),
                 "\n(",sprintf("%.3f", round(lwr, 3)),", ",sprintf("%.3f", round(upr, 3)),")\n[", perc,"]")) -> howmanynegs

howmanynegs %>%
  mutate(x = .25,
         y = rep(c(3.5,2.5,1.5,4.5), 3)) -> annotate_me

# library(ggridges)
Sims[["sims_fd"]] %>%
  group_by(dv, iv, sim) %>%
  summarize(diff = diff(fit)) %>%
  mutate(Category = forcats::fct_recode(iv,
                                        "People Who\nSpeak a\nDifferent Language" = "neighb_difflang",
                                        "Members\nof a\nDifferent Race" = "neighb_diffrace",
                                        "Immigrants/\nForeign Workers" = "neighb_immig",
                                        "White\nSocial\nPrejudice" = "wnneighb"
  )) %>% 
  ungroup() %>%
  mutate(dv = forcats::fct_recode(dv,
                                  "Strong Leader" = "sld",
                                  "Army Rule" = "ard",
                                  "Oppose Democracy" = "hdd")) %>%
  # mutate(Category = forcats::fct_relevel(Category,
  #                                        "White Outgroup Intolerance",
  #                                        "Immigrants/\nForeign Workers",
  #                                        "Members\nof a\nDifferent Race",
  #                                        "People Who\nSpeak a\nDifferent Language")) %>%
  ggplot(., aes(x = diff, y = Category, fill=factor(..quantile..))) + 
  theme_steve_web() +
  facet_wrap(~dv) +
  stat_density_ridges(geom = "density_ridges_gradient",
                      quantile_lines = TRUE,
                      quantiles = c(0.025, 0.975),
                      alpha = 0.4, 
                      calc_ecdf = TRUE) + 
  scale_fill_manual(name = "Probability", values = c("#A6CEE3", "#1F78B4", "#A6CEE3"),
                    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")) +
  geom_vline(xintercept=0, linetype="dashed") + 
  xlab("") + ylab("") + guides(fill=FALSE) +
  scale_x_continuous(breaks = seq(-.2, .4, by=.1)) +
  geom_text(data=annotate_me, aes(x=x, y=y, label=label), 
            colour="black", inherit.aes=FALSE, parse=FALSE, size=2.75, family="Open Sans") + 
  labs(caption = "Note: each ridge annotated with the mean of first differences, 95% intervals around the mean (in parentheses), and the proportions of simulations with negative first differences [in brackets].") -> fig_fd

# Education-interaction plot -----
# Create simple dwplot

Mdf <- rbind(tidy(Models[[13]]) %>% mutate(model = "m4"),
             tidy(Models[[14]]) %>% mutate(model = "m5"),
             tidy(Models[[15]]) %>% mutate(model = "m6")) %>%
  filter(term != "(Intercept)" & term != "sd_(Intercept).censusr:year" & 
           term != "sd_(Intercept).censusr" & term != "sd_(Intercept).year") %>%
  filter(term %in% c("wnneighb", "collegeed", "collegeed:wnneighb"))


plotcap_educinteraction <- paste0("Number of observations: (Strong Leaders: ", prettyNum(nobs(Models[[13]]), big.mark=","),
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
  theme_steve_web() + 
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
  labs(caption=plotcap_educinteraction,
       title = "a) The Effect of Intolerance and College Education on Anti-Democratic Orientations") -> fig_interact_dw


# make interact_sims_plot -----

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
         Category = ifelse(wnneighb == 0 & collegeed == 1, "College Educated,\nWhite Social Prejudicee = 0", Category),
         Category = ifelse(wnneighb == 1 & collegeed == 1, "College Educated,\nWhite Social Prejudice = 1", Category)) %>%
  mutate(x = c(rep(.675, 4), rep(.3, 4), rep(.55, 4)),
         y = rep(seq(1.45, 4.45, by=1), 3)) -> annotate_me


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
  theme_steve_web() +
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
  facet_wrap(~dv) + theme_steve_web() +
  geom_linerange(aes(x = educatrmd, ymin = lwr,
                     ymax = upr),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = educatrmd, y = fit, ymin = lwr,
                      ymax = upr),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  fill = "WHITE") + coord_flip() + scale_color_brewer(palette="Dark2") +
  xlab("") + ylab("") + theme(legend.title=element_blank()) +
  scale_y_continuous(breaks = seq(0, .6, by = .1)) -> fig_rs

# Save all the figures -----

list("fig_bar" = fig_bar,
     "fig_dw" = fig_dw,
     "fig_fd" = fig_fd,
     "fig_interact_dw" = fig_interact_dw,
     "fig_interact_sims" = fig_interact_sims,
     "fig_rs" = fig_rs) -> Figures


saveRDS(Figures, "data/figures.rds")
