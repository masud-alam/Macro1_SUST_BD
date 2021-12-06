

### Importing FRED St. Louis data

library(fredr)

fredr_set_key("98af1328342db1af61f2fee2c777b359") # api_key string must be retrieved from https://research.stlouisfed.org/useraccount/apikey


# search database for Real GDP per Worker series

search_ls <- fredr_series_search_text("Real GDP")

# view column names of series search result list
colnames(search_ls)


# loading ggplot2 R-package
library(ggplot2)

# A939RX0Q048SBEA is id for seasonlly adjusted qterly real gdp per capita units
series_ls <-fredr_series_observations(series_id = "A939RX0Q048SBEA") 

# convert series list to dataframe
series_df <- do.call(cbind.data.frame, series_ls)

# plotting data
ggplot(series_df) + geom_line(mapping = aes(x=date,y=value), 
                              color = "red") +
  ggtitle("Quarterly US Real GDP Per Capita, seasonally adjusted [Chained 2012 Dollars]") + 
  xlab("time") + 
  ylab("Quarterly US Real GDP Per Capita")



### Gross Saving (Private)

search_ls <- fredr_series_search_text("Gross Saving")

# view column names of series search result list
colnames(search_ls)


# loading ggplot2 R-package
library(ggplot2)

# GPSAVE is id for seasonally adjusted qterly real gross saving units
series_ls <-fredr_series_observations(series_id = "GPSAVE") 

# convert series list to dataframe
series_df <- do.call(cbind.data.frame, series_ls)

# plotting data
ggplot(series_df) + geom_line(mapping = aes(x=date,y=value), 
                              color = "red") +
  ggtitle("Quarterly US Private Saving, seasonally adjusted") + 
  xlab("time") + 
  ylab("Quarterly US Private Saving")+theme_bw()


### Take values from 2019 to 2021

grossSav_Covid <- ts(series_df$value , start = c(1947, 3), frequency = 4)
gsav <- window(grossSav_Covid, start = 2019, end=c(2021,3), freq = 4)
plot(gsav, ylab="Quarterly US Private Saving",xlab="Quarter", main="Quarterly US Private Saving, seasonally adjusted" )
lines(gsav, col = "blueviolet", lwd=2)

# Using ggplot2

series_df_gsav <- series_df[289:299,]
# plotting data
p1 <- ggplot(series_df_gsav) + geom_line(mapping = aes(x=date,y=value), 
                              color = "blueviolet",lwd=2) +
  ggtitle("Quarterly US Private Saving during COVID period") + 
  xlab("time") + 
  ylab("Quarterly US Private Saving")+theme_bw()

p1+scale_x_date(breaks = scales::breaks_pretty(10))





# plot
ggplot(stk.df,
       aes(x = Date, y = log(AAPL.Close))) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +             
  labs(x = "Year", y = "log(AAPL's closing value)") +
  theme_bw()



##Simulating in a Solow model

s <- 0.25
d <- 0.05
n <- 0.02
a <- 0.4

k <- rep(NA,200)
y <- rep(NA,200)
i <- rep(NA,200)
c <- rep(NA,200)
dp <- rep(NA,200)


k[1] <- 1
y[1] <- k[1]^a
i[1] <- s*y[1]
c[1] <- y[1]-i[1]
dp[1] <- d*k[1]

for (t in 2:200){
  
  y[t] <- k[t-1]^a 
  k[t] <- s*y[t]+(1-d-n)*k[t-1]
  i[t] <- s*y[t]
  c[t] <- y[t]-i[t]
  dp[1] <- d*k[t]
}

plot.ts(dp)
plot.ts(k)
plot.ts(y)
plot.ts(c)
plot.ts(i)





##Replication: A Contribution to the Empirics of Economic Growth by Mankiw, Romer, and Weil (1992)


library(tibble)     # nice dataframes
library(stargazer)  # regression tables
library(ggplot2)    # nice graphs
library(dplyr)      # data manipulation
library(car)        # test linear hypotheses
library(skimr)      # descriptive statistics
library(haven)      # reading stata data

options(warn = -1)
options(scipen = 10000)
options(repr.plot.width = 6, repr.plot.height = 4)
mrw_data <- read_dta("https://github.com/quarcs-lab/data-quarcs/raw/master/mrw1992/mrw2.dta")
write.csv(mrw_data, "mrw_data.csv")
skim(mrw_data)


#Rename variable names, so they are more meaningful.
mrw_data <- mrw_data %>% 
  rename(non_oil = n, 
         oecd = o,
         intermediate = i,
         gdp_60 = rgdpw60,
         gdp_85 = rgdpw85,
         gdp_growth_60_85 = gdpgrowth,
         pop_growth_60_85 = popgrowth,
         inv_gdp = i_y,
         school = school)

#Create the key variables of the Solow model.

delta_gamma <- 0.05

mrw_data <- mrw_data %>% 
  mutate(ln_gdp_85 = log(gdp_85),
         ln_gdp_60 = log(gdp_60),
         ln_gdp_growth = ln_gdp_85 - ln_gdp_60,
         ln_inv_gdp = log(inv_gdp/100),
         non_oil = factor(non_oil),
         intermediate = factor(intermediate),
         oecd = factor(oecd),
         ln_ndg = log(pop_growth_60_85/100 + delta_gamma),
         ln_school = log(school/100)) %>% 
  select(country, region, ln_gdp_85, ln_gdp_60, ln_inv_gdp, 
         non_oil, intermediate, oecd,
         ln_ndg, ln_school, gdp_growth_60_85, ln_gdp_growth)


skim(mrw_data)
non_oil      <- mrw_data %>% 
  filter(non_oil == 1)
intermediate <- mrw_data %>% 
  filter(intermediate == 1)
oecd         <- mrw_data %>% 
  filter(oecd == 1)


africa       <- mrw_data %>% 
  filter(region == "Africa")
asia         <- mrw_data %>% 
  filter(region == "Asia")
europe       <- mrw_data %>% 
  filter(region == "Europe")
latinAmerica <- mrw_data %>% 
  filter(region == "LatinAmerica")
northAmerica <- mrw_data %>% 
  filter(region == "NorthAmerica")
oceania      <- mrw_data %>% 
  filter(region == "Oceania")



solow_oecd <- lm(ln_gdp_85 ~ ln_inv_gdp + ln_ndg, data = oecd)
solow_int  <- lm(ln_gdp_85 ~ ln_inv_gdp + ln_ndg, data = intermediate)
solow_noil <- lm(ln_gdp_85 ~ ln_inv_gdp + ln_ndg, data = non_oil)


stargazer(solow_noil, solow_int, solow_oecd, digits=2, type = "text",
          column.labels = c("Non-Oil", 
                            "Intermediate", 
                            "OECD"),
          covariate.labels = c("log(I / GDP)", 
                               "log(n+delta+g)", 
                               "Constant"), 
          dep.var.labels = "Log(GDP) 1985",
          omit.stat = c("f", 
                        "rsq", 
                        "ser"),
          title = "Table 1 - Unrestricted Models",
          style = "qje")
solow_noil_r <- lm(ln_gdp_85 ~ I(ln_inv_gdp - ln_ndg), data = non_oil)
solow_int_r  <- lm(ln_gdp_85 ~ I(ln_inv_gdp - ln_ndg), data = intermediate)
solow_oecd_r <- lm(ln_gdp_85 ~ I(ln_inv_gdp - ln_ndg), data = oecd)


stargazer(solow_noil_r, solow_int_r, solow_oecd_r, digits=2, type = "text",
          column.labels = c("Non-Oil", 
                            "Intermediate", 
                            "OECD"),
          covariate.labels = c("log(I / GDP)- log(n+delta+g)", 
                               "Constant"), 
          dep.var.labels = "Log(GDP) 1985",
          omit.stat = c("f", 
                        "rsq", 
                        "ser"),
          title = "Table 1 - Restricted Models",
          style = "qje")


linearHypothesis(solow_noil, "ln_inv_gdp = - ln_ndg")
linearHypothesis(solow_int,  "ln_inv_gdp = - ln_ndg")
linearHypothesis(solow_oecd, "ln_inv_gdp = - ln_ndg")
C <- coef(solow_noil_r)[2]
alpha_solow_noil_r <- C/(1+C)
alpha_solow_noil_r <- round(alpha_solow_noil_r, 2)
print(paste("Implied alpha (Non oil):", alpha_solow_noil_r))


C <- coef(solow_int_r)[2]
alpha_solow_int_r <- C/(1+C)
alpha_solow_int_r <- round(alpha_solow_int_r, 2)
print(paste("Implied alpha (Intermediate):", alpha_solow_int_r))

C <- coef(solow_oecd_r)[2]
alpha_solow_oecd_r <- C/(1+C)
alpha_solow_oecd_r <- round(alpha_solow_oecd_r, 2)
print(paste("Implied alpha (OECD):", alpha_solow_oecd_r))


# Augmented Solow model
# Add human capital accumulation to the Solow model.




