library(plotly)
p <- plot_ly(z = ~yield_curves$US) %>% add_surface()
chart_link = api_create(p, filename="./Plots/us-yield-curve")
Sys.setenv("plotly_username"="stanimir.s.ivanov")
Sys.setenv("plotly_api_key"="AMUk5BtS1jyWylXfHT1f")

lapply(yield_curves, function(x){
  write.csv(x, file = paste0(x, ".csv"))
})

write.csv(yield_curves$US, file = "us.csv")
write.csv(yield_curves$CA, file = "ca.csv")
write.csv(yield_curves$JP, file = "jp.csv")
write.csv(yield_curves$DE, file = "de.csv")
write.csv(yield_curves$UK, file = "uk.csv")
