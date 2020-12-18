library(tidyverse)
library(reshape2)
users <- read_csv("C:/Users/jdisa/OneDrive/Documents/CMU/Sophomore/Fall/80325 Foundations of Causation and Machine Learning/finalproject/netusers.csv")
exports <- read_csv("C:/Users/jdisa/OneDrive/Documents/CMU/Sophomore/Fall/80325 Foundations of Causation and Machine Learning/finalproject/exports.csv")

exports_shaped = exports %>% pivot_longer(!`Country Code`, names_to = "year", values_to = "exports_t") %>% arrange(desc(year), `Country Code`) %>%
  mutate(exports_t1 = lead(exports_t, n = length(unique(`Country Code`))))
view(exports_shaped)
users_shaped = users %>% pivot_longer(!`Country Code`, names_to = "year", values_to = "netusers_t") %>% arrange(desc(year), `Country Code`) %>%
  mutate(netusers_t1 = lead(netusers_t, n = length(unique(`Country Code`))))

combined = merge(exports_shaped, users_shaped, by = c("year", "Country Code")) %>%
  filter(year != 1990) %>%
  mutate(id = paste(`Country Code`, year, sep=";")) %>%
  select(!c(year, `Country Code`))

combined_internal = merge(exports_shaped, users_shaped, by = c("year", "Country Code")) %>%
  filter(year != 1990)
  
combined_missing = combined %>% 
  mutate(rt_exports = ifelse(is.na(exports_t), 1, 0),
         rt1_exports = ifelse(is.na(exports_t1), 1, 0),
         rt_netusers = ifelse(is.na(netusers_t), 1, 0),
         rt1_netusers = ifelse(is.na(netusers_t1), 1, 0))

write_csv(combined_missing, path = "C:/Users/jdisa/OneDrive/Documents/CMU/Sophomore/Fall/80325 Foundations of Causation and Machine Learning/finalproject/data_ts_missing.csv")
write_csv(combined, path = "C:/Users/jdisa/OneDrive/Documents/CMU/Sophomore/Fall/80325 Foundations of Causation and Machine Learning/finalproject/data_ts.csv")
combined %>%
  ggplot(aes(x = exports_t, y = exports_t1)) +
  geom_point()

combined %>%
  ggplot(aes(x = netusers_t, y = exports_t1)) +
  geom_point()

combined_internal %>% filter(year == 2015) %>%
  ggplot(aes(y = netusers_t1, x = exports_t)) +
  geom_point()


