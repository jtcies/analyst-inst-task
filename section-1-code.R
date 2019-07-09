library(tidyverse)
library(tidymodels)
library(scales)

theme_set(theme_minimal())

rct <- read_csv("section-1-data.csv")

# functions -------------

calc_effect <- function(rct, outcome) {

    formula <- as.formula(paste0(outcome, "~ treatment_assignment +
                        factor(experimental_block)"))

    lm(formula, data = rct)

}

# checking data ---------------------
# check treatment and control breakdwon
rct %>%
    count(treatment_assignment) %>%
    mutate(pct = n / sum(n))

rct %>%
    group_by(experimental_block, treatment_assignment) %>%
    summarise(n = n()) %>%
    mutate(pct = n / sum(n)) %>%
    filter(treatment_assignment == "Treatment") %>%
    ggplot(aes(pct)) +
        geom_histogram(binwidth = 0.1)

# see which districts were included

rct %>%
    count(state, congressional_district, sort = TRUE) %>%
    as.data.frame()

# interesting as there might be some state effects
# look at number of targets

rct %>%
 ggplot(aes(number_targets)) +
    geom_histogram() +
    facet_wrap(~treatment_assignment) +
    scale_x_log10()

rct %>%
    filter(treatment_assignment == "Treatment", number_targets == 0)

rct %>%
    filter(treatment_assignment == "Control", number_targets > 0)

# are variances equivalent? are the data normal?

rct %>%
    filter(house_total_votes_2018 > 0) %>%
    mutate(
        dem_margin = (house_dem_votes_2018 - house_rep_votes_2018) / house_total_votes_2018
    ) %>%
    group_by(treatment_assignment) %>%
    summarise(s = sd(dem_margin) ^ 2)

# variances are slightly different, but it doesn't seem to affect analysis

rct %>%
    filter(house_total_votes_2018 > 0) %>%
    mutate(
        dem_margin = (house_dem_votes_2018 - house_rep_votes_2018) / house_total_votes_2018
    ) %>%
    ggplot(aes(dem_margin)) +
        geom_density() +
        facet_wrap(~treatment_assignment)

# appears mostly normal

# check randomization

rct %>%
    mutate(dem_win = house_dem_votes_2018 > house_rep_votes_2018) %>%
    group_by(treatment_assignment) %>%
    summarise(pct_dem = mean(dem_win))

# adding variables and cleaning ----------

rct_tidy <- rct %>%
    filter(house_total_votes_2018 != 0) %>%
    mutate(
        # fix some total house votes > num registered voters
        number_registered_voters = if_else(
            house_total_votes_2018 > number_registered_voters,
            house_total_votes_2018, number_registered_voters
        ),
        pct_dem_all = house_dem_votes_2018 / number_registered_voters,
        pct_rep_all = house_rep_votes_2018 / number_registered_voters,
        other_votes = house_total_votes_2018 - house_dem_votes_2018 - house_rep_votes_2018,
        pct_other_all = other_votes / number_registered_voters,
        pct_dem_voters = house_dem_votes_2018 / house_total_votes_2018,
        pct_rep_voters = house_rep_votes_2018 / house_total_votes_2018,
        pct_other_voters = other_votes / house_total_votes_2018,
        dem_margin = (house_dem_votes_2018 - house_rep_votes_2018) /
            (house_dem_votes_2018 + house_rep_votes_2018),
        state_district = paste0(state, congressional_district),
        turnout = house_total_votes_2018 / number_registered_voters,
    )

# treatment effect analysis ------------------- 

outcomes <- c("dem_margin", "pct_dem_all", "pct_rep_all", "pct_other_all", "turnout")

mods <- outcomes %>%
    map(~calc_effect(rct_tidy, .x))

effects <- mods %>%
    map_dfr(~tidy(.x, conf.int = TRUE, conf.level = 0.9)) %>%
    filter(term == "treatment_assignmentTreatment") %>%
    mutate(outcome = outcomes)

effects %>%
    mutate(
        outcome = case_when(
            outcome == "turnout" ~ "Turnout",
            outcome == "pct_dem_all" ~ "Dem votes (of all registered)",
            outcome == "pct_rep_all" ~ "Rep votes (of all registered)",
            outcome == "pct_other_all" ~ "Other votes (of all registered)",
            outcome == "dem_margin" ~ "Dem margin over Rep",
        ),
        outcome = str_wrap(outcome, 14),
        outcome = fct_reorder(outcome, estimate),
    ) %>%
    mutate_at(vars(estimate, conf.low, conf.high), function(x) x * 100) %>%
    ggplot(aes(outcome, estimate, ymin = conf.low, ymax = conf.high,
               color = outcome)) +
        geom_point(size = 2.5, show.legend = FALSE) +
        geom_linerange(show.legend = FALSE) +
        geom_hline(yintercept = 0, linetype = 3) +
        coord_flip() +
        labs(
            y = "Percentage point change",
            x = "effect",
            title = "Greatest impact is on turnout, mostly benefitting Dems",
            subtitle = "None of the impacts are statistically significant at 0.9 level"
       )

ggsave("output/treament_effects.png", width = 6, height = 3.7, units = "in")
write_csv(effects, "output/treatment_effects_table.csv")

effects %>%
    select(outcome, estimate, conf.low, conf.high) %>%
    arrange(-estimate) %>%
    tbl_df()

# experiment-wide democratic gain ---------------
# compare the actuals to the counter-factuals for the treatment group
# to determine total number of votes added

treat <- rct_tidy %>%
    filter(treatment_assignment == "Treatment") %>%
    select(pid, number_registered_voters, pct_dem_all,
           pct_rep_all, pct_other_all) %>%
    gather(outcome, pct, 3:5)

# counterfactual assumes increase from treatment didn't happen
cf <- treat %>%
    left_join(effects, by = "outcome") %>%
    mutate(
        est = pct - estimate,
        est_high = pct - conf.low,
        est_low = pct - conf.high,
        outcome = paste0(outcome, "_cf")
    ) %>%
    select(pid, outcome, est, est_high, est_low) %>%
    gather(var, val, 3:5) %>%
    unite(var, outcome, var) %>%
    spread(var, val)

gain <- rct_tidy %>%
    inner_join(cf, by = "pid") %>%
    mutate(
        cf_dem = number_registered_voters * pct_dem_all_cf_est,
        cf_dem_low = number_registered_voters * pct_dem_all_cf_est_low,
        cf_dem_high = number_registered_voters * pct_dem_all_cf_est_high,
        cf_rep = number_registered_voters * pct_rep_all_cf_est,
        cf_rep_low = number_registered_voters * pct_rep_all_cf_est_low,
        cf_rep_high = number_registered_voters * pct_rep_all_cf_est_high,
        cf_other = number_registered_voters * pct_other_all_cf_est,
        cf_other_high = number_registered_voters * pct_other_all_cf_est_high,
        cf_other_low = number_registered_voters * pct_other_all_cf_est_low,
        dem_rep_dif = house_dem_votes_2018 - house_rep_votes_2018,
        dem_other_dif = house_dem_votes_2018 - other_votes,
        cf_dem_rep_dif = cf_dem - cf_rep,
        cf_dem_other_dif = cf_dem - cf_other,
        cf_dem_rep_dif_high = cf_dem_high - cf_rep_high,
        cf_dem_other_dif_high = cf_dem_high - cf_other_high,
        cf_dem_rep_dif_low = cf_dem_low - cf_rep_low,
        cf_dem_other_dif_low = cf_dem_low - cf_other_low,
        dem_gain = (2 * (dem_rep_dif - cf_dem_rep_dif)) + (dem_other_dif - cf_dem_other_dif),
        dem_gain_high = (2 * (dem_rep_dif - cf_dem_rep_dif_low)) + 
            (dem_other_dif - cf_dem_other_dif_low),
        dem_gain_low = (2 * (dem_rep_dif - cf_dem_rep_dif_high)) + 
            (dem_other_dif - cf_dem_other_dif_high)
    )

gain_summary <- gain %>% 
    summarise(
        dem_gain = sum(dem_gain),
        dem_gain_low = sum(dem_gain_low),
        dem_gain_high = sum(dem_gain_high)
    ) 

write_csv(gain_summary, "output/party_treatment_gain.csv")

# subgroup analysis ---------------------------

subgroup <- rct_tidy %>%
    mutate(
        dem_margin_2016 = (clinton_votes_2016 - trump_votes_2016) /
            (clinton_votes_2016 + trump_votes_2016),
        pct_dem_voters_2016 = clinton_votes_2016 / total_votes_2016,
        dif = dem_margin - dem_margin_2016,
        cut_2016 = cut(
            dem_margin_2016,
            breaks = unname(quantile(dem_margin_2016, seq(0, 1, by = 0.1)))
        ),
        clinton_win = clinton_votes_2016 > trump_votes_2016
    )

subgroup_mod <- lm(
    dem_margin ~ dem_margin_2016 * treatment_assignment + factor(experimental_block),
    data = subgroup
)

subgroup_mod %>%
    tidy() %>%
    filter(!str_detect(term, "factor"))

augmented <- subgroup_mod %>%
    augment()

augmented %>%
    bind_cols(subgroup) %>%
    mutate(cut = cut(dem_margin_2016, 10)) %>%
    ggplot(aes(cut, .fitted, color = treatment_assignment)) +
        geom_boxplot() +
        labs(
            x = "2016 Democratic vote margin",
            y = "predicted 2018 Democratic vote margin",
            title = "Treatment less effective after accounting for 2016 results",
            color = ""
        )

ggsave("output/subgroup_plot.png", width = 10, heigh = 6.18, units = "in")
