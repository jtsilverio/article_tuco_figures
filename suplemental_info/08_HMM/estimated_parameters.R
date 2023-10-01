library(momentuHMM)
m1 = readRDS("HMM/m1.rds")
m2 = readRDS("HMM/m2.rds")

#parameters_table = data.frame(Parameter = rep('par',9), Estimate = rep(0,9))
parameters_table = as_tibble(m2$mle$vedba) %>% tidyr::pivot_longer(cols = 1:3, values_to = "Estimate", names_to = "Parameter")

parameters_table$Parameter = rep(c("mean","sd","zero-mass"), 3)
parameters_table$Parameter = parameters_table$Parameter[order(parameters_table$Parameter)]
parameters_table$Estimate = round(parameters_table$Estimate, 5)
parameters_table = parameters_table %>% mutate(State = rep(c("Rest", "Medium", "High"), 3), .after = Parameter)

ci_lower = as_tibble(m2$CIreal$vedba$lower) %>% tidyr::pivot_longer(cols = 1:3, values_to = "Estimate", names_to = "Parameter")
ci_lower$Parameter = rep(c("mean","sd","zero-mass"), 3)
ci_lower$Parameter = ci_lower$Parameter[order(parameters_table$Parameter)]

ci_upper = as_tibble(m2$CIreal$vedba$upper) %>% tidyr::pivot_longer(cols = 1:3, values_to = "Estimate", names_to = "Parameter")
ci_upper$Parameter = rep(c("mean","sd","zero-mass"), 3)
ci_upper$Parameter = ci_upper$Parameter[order(parameters_table$Parameter)]

parameters_table$CI = paste0("[", round(ci_lower$Estimate, 3),", ", round(ci_upper$Estimate, 3), "]")
parameters_table = parameters_table[1:6,]

estimated_parameters = kable(parameters_table, 
      caption = "Gamma State-dependent distribution parameters, mean and standard deviation, estimated by a three-state Hidden Markov Model.")

save_kable(estimated_parameters, "suplemental_info/08_HMM/estimated_parameters.html")
