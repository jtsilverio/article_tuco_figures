library(dplyr)
library(data.table)
library(momentuHMM)
library(ggplot2)
library(dplR)
library(egg)
library(patchwork)
library(pracma)

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

# ACFs -------------------------------------------------------------------------
# Calculate ACF for each state and then find the third peak in the timeseries
tuco_split = split(tuco, tuco$ID)

for (state in c("Low","Medium","High","vedba")) {
    
    state_acf = lapply(tuco_split, function(x) as.numeric(x[[state]]))
    state_acf = lapply(state_acf, dplR::pass.filt,
                       W = 180,
                       type="low",
                       method="Butterworth")
    
    state_acf = lapply(state_acf, acf, lag.max = 4000, plot = F)
    state_acf = data.table::rbindlist(state_acf, idcol = "ID", fill = T)
    
    if(state == "Low"){
        names(state_acf)[names(state_acf) == "acf"] = state
        acfs = state_acf
    }else{
        acfs[, state] = state_acf$acf 
    }
}

# Calculate CI for ACF and merge season data
acfs = acfs %>% 
    dplyr::select(ID, lag, n.used, Low, Medium, High, "vedba") %>% 
    group_by(ID) %>% 
    mutate(acp_region = qnorm((1 + 0.9)/2) / sqrt(n.used)) %>% 
    tidyr::pivot_longer(cols = c("Low","Medium", "High", "vedba"), names_to = "state", values_to = "acf") %>% 
    ungroup() %>% 
    left_join(tuco.metadata)


# Reorder IDs
acfs$ID = factor(acfs$ID, levels = c("MAR01", "MAR02", "JUL15",
                                  "JUL16", "JUL17", "JUL18",
                                  "JUL19", "JUL20", "JUL21", 
                                  "JUL23", "OCT01", "OCT08", 
                                  "OCT09", "OCT10", "OCT13", 
                                  "OCT14", "FEV01", "FEV02",
                                  "FEV03", "FEV05", "FEV06"))

# Find ACF Peaks (RI) ----------------------------------------------------------
peaks_acf = acfs %>% 
    group_by(ID, state) %>% 
    summarise(data.frame(pracma::findpeaks(acf, minpeakdistance = 1100, npeaks = 3))) %>%
    rename(acf = X1, lag = X2, begin = X3, end = X4) %>%
    arrange(lag, .by_group = T) %>% 
    slice_tail()

# get CI for peaks
peaks_acf = left_join(peaks_acf,
                      acfs %>% dplyr::select(ID,
                                             season,
                                             state,
                                             lag,
                                             acp_region),
                  by = c("ID", "state", "lag"))

peaks_acf = peaks_acf %>% 
            mutate(acp = ifelse(acf > acp_region, T, F))

# reorder factors
peaks_acf$state = factor(peaks_acf$state, levels = c("Low", "Medium", "High", "vedba"))

# Save data
saveRDS(peaks_acf %>% ungroup(), "03_analysis/rhythmicity/rhythmicity.rds")

# ACF Plots -------------------------------------------------------------------

# This file contains the visual classification of the plot below. We visually classified animals between rhythmic and arrythmic
rhythmicity = readr::read_csv("03_analysis/rhythmicity/visual_rhythmicity_analysis.csv", col_types = "ffl")
peaks_acf = left_join(peaks_acf, rhythmicity)
peaks_acf$acf = ifelse(peaks_acf$rhythmic == F, NA, peaks_acf$acf)
peaks_acf$acf = ifelse(peaks_acf$acp == F, NA, peaks_acf$acf)

saveRDS(peaks_acf %>% ungroup(), "03_analysis/rhythmicity/rhythmicity_classified.rds")


# Plot --------------------------------------------------------

acf_plot = ggplot(data = acfs, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = acp_region), linetype = 2, color = "blue", alpha = 0.4) +
    geom_hline(aes(yintercept = -acp_region), linetype = 2, color = "blue", alpha = 0.4) +
    geom_line(aes(color = season)) +
    scale_x_continuous(breaks = c(0, 1440, 1440*2, 1440*3, 1440*4),
                       labels = c(0, 24, 48, 72, 96)) +
    facet_grid(ID~state) +
    xlab("Lag (h)") +
    ylab("Autocorrelation") +
    geom_point(data = peaks_acf,
               aes(lag, acf), shape = 4) +
    egg::theme_article() +
    theme(legend.position="bottom",
          legend.title = element_blank(),
          text = element_text(size = 9.5))

ggsave("04_figures/rhythmicity/acf_plot.png", 
       acf_plot, "png", bg = "white",
       dpi = 200, width = 210, height = 290, units = "mm")

# Rythmicity Index Distribution --------------------------------------------
peaks_acf$season = factor(peaks_acf$season, levels = c("March","July","October","February"))

bx_plot = ggplot(peaks_acf, aes(x = season, y = acf, color = season)) +
    facet_grid(~state) +
    gghalves::geom_half_boxplot() +
    gghalves::geom_half_point() +
    egg::theme_article() +
    scale_x_discrete(label = c("Mar","Jul","Oct","Feb")) +
    ylab("Rhythmicity Index (RI)") +
    xlab("")


# Individual Plots if Needed -----------------------------------------------
#
# # HIGH STATE
# acf_plot = ggplot(data = acfs[acfs$state == "high",], mapping = aes(x = lag, y = acf)) +
#     #geom_hline(aes(yintercept = 0)) +
#     geom_hline(aes(yintercept = acp_region), linetype = 2, color = "blue", alpha = 0.6) +
#     geom_hline(aes(yintercept = -acp_region), linetype = 2, color = "blue", alpha = 0.6) +
#     geom_line(aes(color = season)) +
#     scale_x_continuous(breaks = c(0, 1440, 1440*2, 1440*3, 1440*4),
#                        labels = c(0, 24, 48, 72, 96)) +
#     facet_grid(ID~state) +
#     xlab("Lag (h)") +
#     ylab("Autocorrelation") +
#     geom_point(data = peaks[acfs$state == "high",],
#                aes(lag, acf), shape = 4) +
#     egg::theme_article()
#
# # MEDIUM STATE
# acf_medium =ggplot(data = acfs[acfs$state == "medium",], mapping = aes(x = lag, y = acf)) +
#     geom_hline(aes(yintercept = 0)) +
#     geom_hline(aes(yintercept = acp_region), linetype = 2) +
#     geom_hline(aes(yintercept = -acp_region), linetype = 2) +
#     geom_line(aes(color = season), size = 0.4) +
#     scale_x_continuous(breaks = c(0, 1440, 1440*2, 1440*3, 1440*4),
#                        labels = c(0, 24, 48, 72, 96)) +
#     facet_wrap(~ID) +
#     xlab("Lag (h)") +
#     ylab("Autocorrelation") +
#     geom_point(data = peaks[peaks$state == "medium",],
#                aes(lag, acf), shape = 4) +
#     ggtitle("ACF Medium State")
# 
# # REST STATE
# acf_rest = ggplot(data = acfs[acfs$state == "rest",], mapping = aes(x = lag, y = acf)) +
#     geom_hline(aes(yintercept = 0)) +
#     geom_hline(aes(yintercept = acp_region), linetype = 2) +
#     geom_hline(aes(yintercept = -acp_region), linetype = 2) +
#     geom_line(aes(color = season), size = 0.4) +
#     scale_x_continuous(breaks = c(0, 1440, 1440*2, 1440*3, 1440*4),
#                        labels = c(0, 24, 48, 72, 96)) +
#     facet_wrap(~ID) +
#     xlab("Lag (h)") +
#     ylab("Autocorrelation") +
#     geom_point(data = peaks[peaks$state == "rest",],
#                aes(lag, acf), shape = 4) +
#     ggtitle("ACF Rest State") +
#     egg::theme_article()
# 
# # VEDBA
# acf_vedba = ggplot(data = acfs[acfs$state == "vedba",], mapping = aes(x = lag, y = acf)) +
#     geom_hline(aes(yintercept = 0)) +
#     geom_hline(aes(yintercept = acp_region), linetype = 2) +
#     geom_hline(aes(yintercept = -acp_region), linetype = 2) +
#     geom_line(aes(color = season), size = 0.4) +
#     scale_x_continuous(breaks = c(0, 1440, 1440*2, 1440*3, 1440*4),
#                        labels = c(0, 24, 48, 72, 96)) +
#     facet_wrap(~ID) +
#     xlab("Lag (h)") +
#     ylab("Autocorrelation") +
#     geom_point(data = peaks[peaks$state == "vedba",],
#                aes(lag, acf), shape = 4) +
#     ggtitle("ACF VeDBA") +
#     egg::theme_article()
