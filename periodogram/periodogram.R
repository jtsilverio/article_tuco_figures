# https://academic.oup.com/bioinformatics/article/22/3/310/220284?login=true#e1
library(dplyr)
library(data.table)
library(momentuHMM)
library(ggplot2)
library(spectr)
library(dplR)
library(spectr)
library(lomb)

# Read Data ---------------------------------------------------------------
tuco = readRDS("01_data/activity_processed/tuco_processed.rds")
tuco.metadata = fread("01_data/animals/animal_metadata.csv")


# Read Model --------------------------------------------------------------
m2 = readRDS("03_analysis/hmm/m2.rds") # modelo com 

# Viterbi State Decoding --------------------------------------------------
decoded = viterbi(m2)
tuco$state = factor(decoded, labels = c("Low","Medium","High"))

# Widen State Column
tuco = tuco %>% 
    mutate(Low = ifelse(state == "Low", T, F),
           Medium = ifelse(state == "Medium", T, F),
           High = ifelse(state == "High", T, F))

# Periodogram -------------------------------------------------------------
tuco_split = split(tuco, tuco$ID)

for (state in c("Low","Medium","High","vedba")) {
    
    state_split = lapply(tuco_split, function(x) as.numeric(x[[state]]))
    
    state_split = lapply(state_split, dplR::pass.filt,
                       W = 180,
                       type="low",
                       method="Butterworth")

    state_lomb = lapply(state_split,
                          lomb::lsp,
                          from = 900,
                          to = 1920,
                          type = "period",
                          ofac = 5,
                          #normalize = "press",
                          plot = F)
    
    state_lomb = lapply(state_lomb, function(x) data.frame(scanned = x$scanned, power = x$power, sig_level = x$sig.level))
    state_lomb = data.table::rbindlist(state_lomb, idcol = "ID", fill = T)
    
    if(state == "Low"){
        state_lomb$state = "Low"
        lomb = state_lomb
    }else{
        state_lomb$state = state
        lomb = dplyr::bind_rows(lomb, state_lomb)
    }
}

lomb = left_join(lomb, tuco.metadata)

# Reorder IDs
lomb$ID = factor(lomb$ID, levels = c("MAR01", "MAR02", "JUL15",
                                     "JUL16", "JUL17", "JUL18",
                                     "JUL19", "JUL20", "JUL21", 
                                     "JUL23", "OCT01", "OCT08", 
                                     "OCT09", "OCT10", "OCT13", 
                                     "OCT14", "FEV01", "FEV02",
                                     "FEV03", "FEV05", "FEV06"))

# Reorder states
lomb$state = factor(lomb$state, levels = c("Low", "Medium", "High", "vedba"))

# Find Peaks --------------------------------------------------------------
peaks_period = lomb %>% 
    group_by(ID, state) %>% 
    summarise(peak = scanned[which.max(power)], sig_level = sig_level[which.max(power)], power = max(power), season = season[1], sex = sex[1]) %>% 
    ungroup()

# Visual Classification of autocorrelation plots based on ACF plots
rhythmicity = readr::read_csv("03_analysis/rhythmicity/visual_rhythmicity_analysis.csv", col_types = "ffl")
peaks_period = left_join(peaks_period, rhythmicity)
peaks_period$power = ifelse(peaks_period$rhythmic == F, NA, peaks_period$power)
peaks_period$peak = ifelse(peaks_period$rhythmic == F, NA, peaks_period$peak)

# Periodogram Plots -----------------------------------------------------------

# Reorder IDs
lomb$ID = factor(lomb$ID, levels = c("MAR01", "MAR02", "JUL15",
                                     "JUL16", "JUL17", "JUL18",
                                     "JUL19", "JUL20", "JUL21", 
                                     "JUL23", "OCT01", "OCT08", 
                                     "OCT09", "OCT10", "OCT13", 
                                     "OCT14", "FEV01", "FEV02",
                                     "FEV03", "FEV05", "FEV06"))

periodogram = ggplot(lomb,
       aes(scanned, 
           power, 
           color = season, 
           group = ID)) +
    facet_grid(ID~state) +
    geom_line() +
    geom_hline(aes(yintercept = sig_level),
               color = "blue",
               linetype = "dashed",
               alpha = 0.4) +
    geom_point(data = peaks_period,
               aes(peak, power, group = ID),
               color = "black",
               shape = 4) +
    geom_text(data = peaks_period,
              aes(x = 60 * 32, y = 3600, label = round(peak/60, 2)),
              color = "black",
              hjust = 1,
              vjust = 1,
              size = 2.5)  +
    scale_x_continuous(breaks = c(15, 18, 21, 24, 27, 30) * 60,
                       labels = c(15, 18, 21, 24, 27, 30)) +
    egg::theme_article() +
    theme(legend.position="bottom",
          legend.title = element_blank(),
          text = element_text(size = 9.5)) +
    xlab("Period (h)") +
    ylab("Power")

# Save Data and Plots --------------------------------------------------------
saveRDS(peaks_period %>% ungroup(), "03_analysis/periodogram/peaks_period.rds")

ggsave("04_figures/periodogram/periodogram.png", 
       periodogram, "png",
       bg = "white",
       width = 210,
       height = 290,
       units = "mm",
       dpi = 200)

# Period Distribution --------------------------------------------------------
peaks_period = left_join(peaks_period, tuco.metadata)
peaks_period$season = factor(peaks_period$season, levels = c("March","July","October","February"))

period_distribution = ggplot(peaks_period, aes(x = season, y = peak/60, color = season)) +
    facet_grid(~state) +
    gghalves::geom_half_boxplot() +
    gghalves::geom_half_point() +
    egg::theme_article() +
    scale_x_discrete(label = c("Mar","Jul","Oct","Feb")) +
    ylab("Period (h)") +
    xlab("")

ggsave("04_figures/periodogram/period_distribution.png", 
       period_distribution,
       "png",
       bg = "white",
       width = 210,
       height = 290,
       units = "mm",
       dpi = 200)

