#' In any maximum likelihood estimation, such as the fitHMM used here, the initial
#' parameters are important to try to avoid local minimums in the negative-log-likelihood profile.
#' 
#' The momentuHMM documention has a good discussion on how to find good starting values:
#' [Th́eo Michelot & Roland Langrock (2019) ](https://cran.r-project.org/web/packages/moveHMM/vignettes/moveHMM-starting-values.pdf)
library(momentuHMM)
library(data.table)
library(dplyr)
library(parallel)

start_script = Sys.time()
tuco = readRDS("01_data/rds/tuco_preprocessed.rds")
set.seed(53)
niter = 100
retryfits = 2

# Delete unused columns
tuco$datetime = NULL
tuco$day_number =  NULL
tuco$lux = NULL
tuco$temp =  NULL

# State names
stateNames <- c("rest","medium", "high")

# Prep data
tuco = prepData(tuco, 
                coordNames = NULL, 
                covNames = c("sex","season"))

# Calculate initial means using k-means
tuco_kmeans = kmeans(tuco$vedba, 3, 50)
centers = sort(tuco_kmeans$centers)

# Randomize Initial Par
get_par0 = function(i){
    vedba_mean0 = runif(3, min = centers - centers/2, max = centers + centers/2)
    vedba_sd0 = runif(3, min = c(0.01, 0.05, 0.3), max = c(0.05, 0.3, 0.8))
    prop_zero = length(which(tuco$vedba==0))/nrow(tuco)
    vedba_zeromass0 = runif(3, min = 0, max = prop_zero)
    return(c(vedba_mean0, vedba_sd0, vedba_zeromass0))
}

all_Par0 = lapply(as.list(1:niter), get_par0)

# Prepare parallel processing
ncores = detectCores() - 1
cl = makeCluster(getOption("cl.cores", ncores))
clusterExport(cl,list("tuco","fitHMM","stateNames","retryfits"))

models = parLapply(cl = cl, all_Par0, fun = function(Par0){
    tryCatch(
        {m = fitHMM(data = tuco, 
                    formula = ~1,
                    nbStates = 3, 
                    dist = list(vedba = "gamma"), 
                    Par0 = list(vedba = Par0),
                    stateNames =  stateNames,
                    retryFits = retryfits)},
        error = function(e){return(e)}
    )
})
stopCluster(cl)

# Extract Best model parameters ------------------------------------------------

bm_index = which.min(unlist(lapply(models, function(x){x$mod$minimum})))
best_model = models[[bm_index]]

Sys.time() - start_script

##############################################
# Value of the maximum log-likelihood: 467469.6 
# vedba parameters:
# -----------------
#                  rest       medium         high
# mean     0.0181281110 3.268385e-01 1.163954e-01
# sd       0.0098276827 8.657868e-02 5.098034e-02
# zeromass 0.0008149793 3.159508e-13 1.223916e-05
# 
# Regression coeffs for the transition probabilities:
# ---------------------------------------------------
#     1 -> 2    1 -> 3    2 -> 1   2 -> 3    3 -> 1   3 -> 2
# (Intercept) -5.030154 -3.252298 -3.877485 -2.02341 -2.756717 -2.19553
# 
# Transition probability matrix:
# ------------------------------
#              rest      medium       high
# rest   0.95673364 0.006254937 0.03701142
# medium 0.01795706 0.867372849 0.11467009
# high   0.05405167 0.094739204 0.85120913
# 
# Initial distribution:
# ---------------------
#         rest       medium         high 
# 6.009411e-01 7.441752e-30 3.990589e-01
#############################################

#############################################
# Value of the maximum log-likelihood: 467893.1 
# 
# 
# vedba parameters:
# -----------------
#                 rest       medium         high
# mean     0.018130514 1.163597e-01 3.268437e-01
# sd       0.009828206 5.079876e-02 8.651551e-02
# zeromass 0.000815682 1.066966e-05 5.183554e-11
# 
# Regression coeffs for the transition probabilities:
# ---------------------------------------------------
#                    1 -> 2      1 -> 3     2 -> 1      2 -> 3     3 -> 1     3 -> 2
# (Intercept)   -3.27325740 -5.26693838 -2.8924954 -2.24843680 -3.8381138 -2.1656054
# seasonJuly    -0.18109786 -0.81101351 -0.1115011 -0.27518244 -0.7166057  0.4075636
# seasonMarch    0.09552143  0.03131097  0.1196878 -0.06829224 -0.4381071  0.1581281
# seasonOctober  0.15320053  0.72842963  0.4129760  0.34023053  0.1543347  0.1194529
# 
# Transition probability matrix (based on mean covariate values):
# ---------------------------------------------------------------
#              rest     medium        high
# rest   0.95510674 0.03980877 0.005084495
# medium 0.05381732 0.86126570 0.084916975
# high   0.01210138 0.11698704 0.870911580
# 
# Initial distribution:
# ---------------------
#         rest       medium         high 
# 6.023382e-01 3.976615e-01 2.597046e-07
##########################################





