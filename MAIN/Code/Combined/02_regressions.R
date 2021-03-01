
library(stargazer); library(ggplot2); library(lfe); library(viridis)

# load data
dat <- readRDS('D:/OneDrive - hawaii.edu/Documents/Projects/Water/Hotel V2/Data/Processed/01_merged_BWS_and_SafeGraph_data.rds')
length(unique(dat$tmk))



# create log variables and other variables
dat$gal_total_monthly_log  <- log(dat$gal_total_monthly+1)
dat$gal_avg_daily_log      <- log(dat$gal_avg_daily+1)
dat$raw_visit_counts_log   <- log(dat$raw_visit_counts+1)
dat$raw_visitor_counts_log <- log(dat$raw_visitor_counts+1)

dat$visit_dwell       <- dat$raw_visit_counts * dat$median_dwell
dat$visit_dwell_log   <- log(dat$visit_dwell+1)
dat$visitor_dwell     <- dat$raw_visitor_counts * dat$median_dwell
dat$visitor_dwell_log <- log(dat$visitor_dwell+1)




#### plots ----




#### regressions ----

# regressions - mean daily water consumption
lm01 <- felm(data = dat, formula = gal_avg_daily_log ~ raw_visitor_counts_log | tmk | 0 | tmk + date)
lm02 <- felm(data = dat, formula = gal_avg_daily_log ~ raw_visit_counts_log   | tmk | 0 | tmk + date)
lm03 <- felm(data = dat, formula = gal_avg_daily_log ~ visitor_dwell_log      | tmk | 0 | tmk + date)
lm04 <- felm(data = dat, formula = gal_avg_daily_log ~ visit_dwell_log        | tmk | 0 | tmk + date)

stargazer(lm01, lm02, lm03, lm04, dep.var.labels = 'Log daily gallons', covariate.labels = c('Log visitor count', 'Log visit count', 'Log (visitors * dwell time)', 'Log (visits * dwell time)'),
          add.lines = list(c('TMK FE', '\\bf{Yes}', '\\bf{Yes}', '\\bf{Yes}', '\\bf{Yes}')))

# regressions - mean daily water consumption, interact with category
lm09 <- felm(data = dat, formula = gal_total_monthly_log ~ raw_visitor_counts_log * premise_type | tmk | 0 | tmk + date)
lm10 <- felm(data = dat, formula = gal_total_monthly_log ~ raw_visit_counts_log * premise_type   | tmk | 0 | tmk + date)
lm11 <- felm(data = dat, formula = gal_total_monthly_log ~ visitor_dwell_log * premise_type      | tmk | 0 | tmk + date)
lm12 <- felm(data = dat, formula = gal_total_monthly_log ~ visit_dwell_log * premise_type        | tmk | 0 | tmk + date)

stargazer(lm09, lm10, lm11, lm12, dep.var.labels = 'Log monthly gallons', covariate.labels = c('Log visitor count', 'Log visit count', 'Log (visitors * dwell time)', 'Log (visits * dwell time)'),
          add.lines = list(c('TMK FE', '\\bf{Yes}', '\\bf{Yes}', '\\bf{Yes}', '\\bf{Yes}')))

# col <- ColorByDensity::get_point_density(x = dat$visitor_dwell_log, y = lm6$residuals, n = 100)
# ggplot(data = NULL, aes(x = dat$visitor_dwell_log, y = lm6$residuals, color = col)) + geom_point(alpha = 0.4) + scale_color_viridis() +
#   theme(legend.position = 'none', text = element_text(size = 15)) + labs(x = 'Log visitors * dwell time', y = 'Redisuals')
# rm(col)

rm(list=setdiff(ls(), "dat"))




##### regressions - parents only ----

# find locations that have no parents (because they themselves are parents)
dat_parents <- dat[dat$parent_safegraph_place_id == '',]
  length(unique(dat_parents$tmk))

# regressions - mean daily water consumption
lm01 <- felm(data = dat_parents, formula = gal_avg_daily_log ~ raw_visitor_counts_log | tmk | 0 | tmk + date)
lm02 <- felm(data = dat_parents, formula = gal_avg_daily_log ~ raw_visit_counts_log   | tmk | 0 | tmk + date)
lm03 <- felm(data = dat_parents, formula = gal_avg_daily_log ~ visitor_dwell_log      | tmk | 0 | tmk + date)
lm04 <- felm(data = dat_parents, formula = gal_avg_daily_log ~ visit_dwell_log        | tmk | 0 | tmk + date)

stargazer(lm01, lm02, lm03, lm04, dep.var.labels = 'Log daily gallons', covariate.labels = c('Log visitor count', 'Log visit count', 'Log (visitors * dwell time)', 'Log (visits * dwell time)'),
          add.lines = list(c('TMK FE', '\\bf{Yes}', '\\bf{Yes}', '\\bf{Yes}', '\\bf{Yes}')))

# regressions - mean daily water consumption, interact with category
lm09 <- felm(data = dat_parents, formula = gal_total_monthly_log ~ raw_visitor_counts_log * premise_type | tmk | 0 | tmk + date)
lm10 <- felm(data = dat_parents, formula = gal_total_monthly_log ~ raw_visit_counts_log * premise_type   | tmk | 0 | tmk + date)
lm11 <- felm(data = dat_parents, formula = gal_total_monthly_log ~ visitor_dwell_log * premise_type      | tmk | 0 | tmk + date)
lm12 <- felm(data = dat_parents, formula = gal_total_monthly_log ~ visit_dwell_log * premise_type        | tmk | 0 | tmk + date)

stargazer(lm09, lm10, lm11, lm12, dep.var.labels = 'Log monthly gallons', covariate.labels = c('Log visitor count', 'Log visit count', 'Log (visitors * dwell time)', 'Log (visits * dwell time)'),
          add.lines = list(c('TMK FE', '\\bf{Yes}', '\\bf{Yes}', '\\bf{Yes}', '\\bf{Yes}')))

rm(list=setdiff(ls(), "dat"))




##### regressions - children only ----

# find locations that have no parents (because they themselves are parents)
dat_children <- dat[dat$parent_safegraph_place_id != '',]
  length(unique(dat_children$tmk))

# regressions - mean daily water consumption
lm01 <- felm(data = dat_children, formula = gal_avg_daily_log ~ raw_visitor_counts_log | tmk | 0 | tmk + date)
lm02 <- felm(data = dat_children, formula = gal_avg_daily_log ~ raw_visit_counts_log   | tmk | 0 | tmk + date)
lm03 <- felm(data = dat_children, formula = gal_avg_daily_log ~ visitor_dwell_log      | tmk | 0 | tmk + date)
lm04 <- felm(data = dat_children, formula = gal_avg_daily_log ~ visit_dwell_log        | tmk | 0 | tmk + date)

stargazer(lm01, lm02, lm03, lm04, dep.var.labels = 'Log daily gallons', covariate.labels = c('Log visitor count', 'Log visit count', 'Log (visitors * dwell time)', 'Log (visits * dwell time)'),
          add.lines = list(c('TMK FE', '\\bf{Yes}', '\\bf{Yes}', '\\bf{Yes}', '\\bf{Yes}')))

# regressions - mean daily water consumption, interact with category
lm09 <- felm(data = dat_children, formula = gal_total_monthly_log ~ raw_visitor_counts_log * premise_type | tmk | 0 | tmk + date)
lm10 <- felm(data = dat_children, formula = gal_total_monthly_log ~ raw_visit_counts_log * premise_type   | tmk | 0 | tmk + date)
lm11 <- felm(data = dat_children, formula = gal_total_monthly_log ~ visitor_dwell_log * premise_type      | tmk | 0 | tmk + date)
lm12 <- felm(data = dat_children, formula = gal_total_monthly_log ~ visit_dwell_log * premise_type        | tmk | 0 | tmk + date)

stargazer(lm09, lm10, lm11, lm12, dep.var.labels = 'Log monthly gallons', covariate.labels = c('Log visitor count', 'Log visit count', 'Log (visitors * dwell time)', 'Log (visits * dwell time)'),
          add.lines = list(c('TMK FE', '\\bf{Yes}', '\\bf{Yes}', '\\bf{Yes}', '\\bf{Yes}')))

rm(list=setdiff(ls(), "dat"))
