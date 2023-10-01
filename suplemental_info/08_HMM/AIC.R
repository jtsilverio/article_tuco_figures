library(momentuHMM)
m1 = readRDS("HMM/m1.rds")
m2 = readRDS("HMM/m2.rds")

mod_df = AIC(m1, m2)
mod_df = mod_df %>% 
    mutate(Formula = c("~season", "~1"), .after = 1)
aic_table = kable(mod_df, caption = "HMM models AIC table.")

save_kable(aic_table, "suplemental_info/08_HMM/AIC.html", )
