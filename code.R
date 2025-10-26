##Main code to create "Deconstructing Monetary Policy" graphs and table.
########################################################################

#required libraries
library(foreign)
library(dplyr)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(zoo)
library(lubridate)
library(svglite)
library(tikzDevice)


setwd("C:/Users/nonso/Dropbox/Research/Deconstructing Monetary Policy")

ct <- "Set1" #Color Theme from RColorBrewer
img_l <- 1600 #image length
img_h <- 800 #image height
img_r <- 150 #image resolution

#Section 1: Inflation graphs and de-trend

#load inflation data
cpi <- read.csv("Data/cpi.csv")

#Add Proper Date
start_date <- as.Date("2009-01-01")
cpi$Date <- seq.Date(from = start_date, by = "month", length.out = nrow(cpi))

#Calculate year-on-year food, core, and all inflation

#All inflation
cpi <- cpi %>%
  arrange(Date) %>% # Ensure data is ordered by date
  mutate(inflation = (cpi / lag(cpi, 12) - 1) * 100)

#Food Inflation
cpi <- cpi %>%
  arrange(Date) %>% # Ensure data is ordered by date
  mutate(food_inf = (food_cpi / lag(food_cpi, 12) - 1) * 100)

#Core Inflation
cpi <- cpi %>%
  arrange(Date) %>% # Ensure data is ordered by date
  mutate(core_inf = (core_cpi / lag(core_cpi, 12) - 1) * 100)

#Plot All, Food and Core Inflation
#create temp long data

temp <- melt(
  cpi[c("Date", "inflation", "food_inf", "core_inf")],
  id.vars = c("Date"),     # Columns to keep as is
  variable.name = "Inflation",
  value.name = "percent"
)

p <- ggplot(
    data = temp,
    aes(x = Date, y = percent, color = Inflation)
    ) +
    geom_line() +
    labs(
        x = "Date", y = "Year on Year Percent Change"
    ) +
    scale_color_manual(
        values = brewer.pal(3, ct),
        labels = c("All", "Food", "Core")
        ) +
    theme_minimal() +
    theme(legend.position = "bottom")

plot(p)

tikz("svgimages/inflation.tex", width = 6, height = 4, pointsize = 12)
print(p)
dev.off()

#Decompose function
#Distinguish between shocks and trends.

#Calculate monthly change in CPI

cpi <- cpi %>%
  arrange(Date) %>% # Ensure data is ordered by date
  mutate(mom = (cpi / lag(cpi, 1) - 1) * 100)

p <- ggplot(
    cpi,
    aes(x = Date, y = mom)
) + geom_line(
    aes(color = "Actual")
) +
geom_smooth(
    linewidth = 2,
    se = FALSE,
    aes(color = "Smooth Loess")
    ) +
labs(
        x = "Date", y = "Month on Month Percent Change"
    ) +
    scale_color_manual(
        name = "",
        values = brewer.pal(3, ct),
        labels = c("Actual", "Smooth Loess")
        ) +
    theme_minimal() +
    theme(legend.position = "bottom")

plot(p)

png("Images/inflation_mom.png", width = 1600, height = 800, res = 150)
plot(p)
dev.off()

tikz("svgimages/inflation_mom.tex", width = 6, height = 4, pointsize = 12)
print(p)
dev.off()


#Detrend data
mom_ts <- ts(cpi$mom, frequency = 12, start = c(2009, 1))
decomposed <- decompose(mom_ts)

# Remove seasonality
sa_mom_ts <- mom_ts - decomposed$seasonal


#test for structural break
library(strucchange)

sb_model <- breakpoints(mom_ts ~ 1) # No predictors, intercept-only model

summary(sb_model)
plot(sb_model)

phl <- sb_model$breakpoints
hl <- as.Date(cpi$Date[phl])

#Visualize
plot(ts_inf)
lines(fitted(sb_model, breaks = 1), col = "red")


#plot with structural break point
p <- ggplot(
    cpi,
    aes(x = Date, y = mom)
) + geom_line(
    aes(color = "Actual")
) +
geom_smooth(
    aes(color = "Smooth Loess"),
    linewidth = 2,
    se = FALSE
    ) +
geom_vline(
    data = data.frame(xintercept = hl),
    linewidth = 2, linetype = "dashed",
    aes(xintercept = xintercept, color = "Break Points")
    ) +
scale_color_manual(
        name = "",
        values = brewer.pal(3, ct)
        ) +
labs(
        x = "Date", y = "Month on Month Percent Change"
    ) +
    theme_minimal()+
    theme(legend.position = "bottom")

plot(p)

png("Images/inflation_mom.png", width = 1600, height = 800, res = 150)
plot(p)
dev.off()

tikz("svgimages/inflation_mom.tex", width = 6, height = 4, pointsize = 12)
print(p)
dev.off()


###############################################
#Evolution of Money Supply growth

ms <- read.csv("Data/moneycredit.csv")
ms$Date <- as.Date(
    paste0(ms$tyear, "-", ms$tmonth, "-01")
)
ms$M3 <- as.numeric(ms$moneySupply_M3)
#plot growth rate of money supply m1, m2, m3

#Cal month on month growth rates
ms <- ms %>%
  arrange(Date) %>% # Ensure data is ordered by date
  mutate(m1_yoy = (baseMoney / lag(baseMoney, 12) - 1) * 100)

ms <- ms %>%
  arrange(Date) %>% # Ensure data is ordered by date
  mutate(m2_yoy = (moneySupply_M2 / lag(moneySupply_M2, 12) - 1) * 100)

ms <- ms %>%
  arrange(Date) %>% # Ensure data is ordered by date
  mutate(m3_yoy = (M3 / lag(M3, 12) - 1) * 100)

#plot all ms growth
temp <- melt(
  ms[c("Date", "m1_yoy", "m2_yoy", "m3_yoy")],
  id.vars = c("Date"),     # Columns to keep as is
  variable.name = "msg",
  value.name = "percent"
)

p <- ggplot(
    data = temp,
    aes(x = Date, y = percent, color = msg)
    ) +
    geom_line() +
    labs(
        x = "Date", y = "Year on Year Percent Change"
    ) +
    scale_color_manual(
        name = "",
        values = brewer.pal(3, ct),
        labels = c("M1", "M2", "M3")
        ) +
    theme_minimal() +
    theme(legend.position = "bottom")
plot(p)

png("Images/ms_growth.png", width = 1600, height = 800, res = 150)
plot(p)
dev.off()

tikz("svgimages/ms_growth.tex", width = 6, height = 4, pointsize = 12)
print(p)
dev.off()

#compare with GDP growth
gdp <- read.csv("Data/gdp.csv")

#add date
gdp$Date <- seq.Date(
    from = as.Date("2010-03-01"),
    by = "quarter",
    length.out = nrow(gdp)
)

#calc growth rates - year on year
gdp <- gdp %>%
  arrange(Date) %>% # Ensure data is ordered by date
  mutate(ngdp_yoy = (nom_gdp / lag(nom_gdp, 4) - 1) * 100)

gdp <- gdp %>%
  arrange(Date) %>% # Ensure data is ordered by date
  mutate(rgdp_yoy = (real_gdp / lag(real_gdp, 4) - 1) * 100)

#Plot M2 growth versus real GDP growth

p <- ggplot() +
    geom_line(
        data = ms[which(year(ms$Date) >= 2010), ],
        aes(x = Date, y = m2_yoy, color = "M2")
    ) +
    geom_smooth(
        data = ms[which(year(ms$Date) >= 2010), ],
        aes(x = Date, y = m2_yoy, color = "M2 - Smooth-Loess"),
        se = FALSE,
        linewidth = 2
    ) +
    geom_line(
        data = gdp,
        aes(x = Date, y = rgdp_yoy, color = "GDP"),
        linewidth = 2
    ) +
    scale_color_manual(
        name = "",
        values = brewer.pal(3, ct),
        labels = c("Real GDP", "M2", "M2 - Smooth-Loess")
        ) +
    labs(
        y = "Year-on-Year percent change", x = "Year"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

plot(p)

#save image
png("Images/ms_v_gdp.png", width = img_l, height = img_h, res = img_r)
plot(p)
dev.off()

tikz("svgimages/ms_v_gdp.tex", width = 6, height = 4, pointsize = 12)
print(p)
dev.off()


#Repeat for M1
p <- ggplot() +
    geom_line(
        data = ms[which(year(ms$Date) >= 2010), ],
        aes(x = Date, y = m1_yoy, color = "M1")
    ) +
    geom_smooth(
        data = ms[which(year(ms$Date) >= 2010), ],
        aes(x = Date, y = m1_yoy, color = "M1 - Smooth-Loess"),
        se = FALSE,
        linewidth = 2
    ) +
    geom_line(
        data = gdp,
        aes(x = Date, y = rgdp_yoy, color = "GDP"),
        linewidth = 2
    ) +
    scale_color_manual(
        name = "",
        values = brewer.pal(3, ct)
        ) +
    labs(
        y = "Year-on-Year percent change", x = "Year"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

plot(p)

#save image
png("Images/m1_v_gdp.png", width = img_l, height = img_h, res = img_r)
plot(p)
dev.off()

tikz("svgimages/m1_v_gdp.tex", width = 6, height = 4, pointsize = 12)
print(p)
dev.off()

#Compare implied price level growth with actual inflation growth

#compute smooth loess data of M2 growth
temp_ms <- ms[which(year(ms$Date) >= 2010), ]
loess_model <- loess(
    m2_yoy ~ as.numeric(Date),
    data = temp_ms
    )

smoothed_data <- data.frame(
  Date = temp_ms$Date,
  y = predict(loess_model),
  ymin = predict(loess_model, se = TRUE)$fit - 1.96 * predict(loess_model, se = TRUE)$se.fit, #nolint
  ymax = predict(loess_model, se = TRUE)$fit + 1.96 * predict(loess_model, se = TRUE)$se.fit #nolint
)


temp <- merge(
    smoothed_data,
    gdp,
    by = "Date",
    All = TRUE
)

#Spread between M2 and GDP growths
temp$spread <- temp$y - temp$rgdp_yoy

#plot spread versus actual inflation

p <- ggplot() +
    geom_line(
        data = temp,
        aes(x = Date, y = spread, color = "Implied"),
        linewidth = 2
    ) +
    geom_line(
        data = cpi,
        aes(x = Date, y = inflation, color = "Actual"),
        linewidth = 2
    ) +
    scale_color_manual(
        name = "",
        values = brewer.pal(3, ct)
        ) +
    theme_minimal() +
    theme(legend.position = "bottom")

plot(p)

#save plot
#save image
png("Images/implied_v_actual.png", width = img_l, height = img_h, res = img_r)
plot(p)
dev.off()

tikz("svgimages/implied_v_actual.tex", width = 6, height = 4, pointsize = 12)
print(p)
dev.off()

#repeat using nominal gdp data to show effect
#is not indirect re-measuring of inflation

#Spread between M2 and GDP growths
temp$spread2 <- temp$y - temp$ngdp_yoy

#plot spread versus actual inflation

p <- ggplot() +
    geom_line(
        data = temp,
        aes(x = Date, y = spread2, color = "Implied - Using nominal GDP"),
        linewidth = 2
    ) +
    geom_line(
        data = cpi,
        aes(x = Date, y = inflation, color = "Actual"),
        linewidth = 2
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

plot(p)

#save plot
#save image
png("Images/implied_nom_v_actual.png", width = img_l, height = img_h, res = img_r) #nolint
plot(p)
dev.off()

tikz("svgimages/implied_nom_v_actual.tex", width = 6, height = 4, pointsize = 12)
print(p)
dev.off()


#Repeat with Comparison with M1 and Real GDP
#compute smooth loess data of M1 growth
temp_ms <- ms[which(year(ms$Date) >= 2010), ]
loess_model <- loess(
    m1_yoy ~ as.numeric(Date),
    data = temp_ms
    )

smoothed_data <- data.frame(
  Date = temp_ms$Date,
  y = predict(loess_model),
  ymin = predict(loess_model, se = TRUE)$fit - 1.96 * predict(loess_model, se = TRUE)$se.fit, #nolint
  ymax = predict(loess_model, se = TRUE)$fit + 1.96 * predict(loess_model, se = TRUE)$se.fit #nolint
)


temp <- merge(
    smoothed_data,
    gdp,
    by = "Date",
    All = TRUE
)

#Spread between M2 and GDP growths
temp$spread <- temp$y - temp$rgdp_yoy

#plot
p <- ggplot() +
    geom_line(
        data = temp,
        aes(x = Date, y = spread, color = "Implied"),
        linewidth = 2
    ) +
    geom_line(
        data = cpi,
        aes(x = Date, y = inflation, color = "Actual"),
        linewidth = 2
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

plot(p)

#save plot
#save image
png("Images/implied_v_actual_m1.png", width = img_l, height = img_h, res = img_r) #no lint.
plot(p)
dev.off()

tikz("svgimages/implied_v_actual_m1.tex", width = 6, height = 4, pointsize = 12)
print(p)
dev.off()

#############################################################
#Section 3: What is growing in M2?

ms$dd_perc <- (ms$demandDeposits / ms$moneySupply_M2) * 100
ms$cob_perc <- (ms$currencyOutsideBanks / ms$moneySupply_M2) * 100
ms$qm_perc <- (ms$quasiMoney / ms$moneySupply_M2) * 100

temp <- melt(
  ms[c("Date", "dd_perc", "cob_perc", "qm_perc")],
  id.vars = c("Date"),     # Columns to keep as is
  variable.name = "M2",
  value.name = "percent"
)

#plot stacked bar graph
p <- ggplot(
    data = temp,
    aes(x = Date, y = percent, fill = M2)
) +
geom_bar(stat = "Identity") +
scale_fill_manual(
    name = "",
    values = brewer.pal(3, ct),
    labels = c(
        "Demand Deposits",
        "Cash Outside Banks",
        "Quasi Money"
        )
) +
labs(
    x = "Year", y = "Percent of Money Supply M2"
) +
theme_minimal() +
theme(legend.position = "bottom")

plot(p)
png("Images/perc_of_m2.png", width = img_l, height = img_h, res = img_r) #nolint.
plot(p)
dev.off()

tikz("svgimages/perc_of_m2.tex", width = 6, height = 4, pointsize = 12)
print(p)
dev.off()

##################################
#Compute and plot growth rates of M2 components
#All inflation
ms <- ms %>%
  arrange(Date) %>% # Ensure data is ordered by date
  mutate(dd_yoy = (demandDeposits / lag(demandDeposits, 12) - 1) * 100)

#Food Inflation
ms <- ms %>%
  arrange(Date) %>% # Ensure data is ordered by date
  mutate(cob_yoy = (currencyOutsideBanks / lag(currencyOutsideBanks, 12) - 1) * 100) #nolint

#Core Inflation
ms <- ms %>%
  arrange(Date) %>% # Ensure data is ordered by date
  mutate(qm_yoy = (quasiMoney / lag(quasiMoney, 12) - 1) * 100)

temp <- melt(
  ms[c("Date", "dd_yoy", "cob_yoy", "qm_yoy")],
  id.vars = c("Date"),     # Columns to keep as is
  variable.name = "M2",
  value.name = "percent"
)


#plot stacked bar graph
p <- ggplot(
    data = temp,
    aes(x = Date, y = percent, color = M2)
) +
geom_line() +
#geom_smooth(se = FALSE, linewidth = 1.5) +
scale_color_manual(
    name = "",
    values = brewer.pal(3, ct),
    labels = c(
        "Demand Deposits",
        "Cash Outside Banks",
        "Quasi Money"
        )
) +
labs(
    x = "Year", y = "Year-on-Year Percent Change"
) +
theme_minimal() +
theme(legend.position = "bottom")

plot(p)

png("Images/yoy_of_m2.png", width = img_l, height = img_h, res = img_r) #nolint.
plot(p)
dev.off()

tikz("svgimages/yoy_of_m2.tex", width = 6, height = 4, pointsize = 12)
print(p)
dev.off()

#zoom in since 2015

#plot stacked bar graph
p <- ggplot(
    data = temp[which(year(temp$Date) > 2014), ],
    aes(x = Date, y = percent, color = M2)
) +
geom_line() +
#geom_smooth(se = FALSE, linewidth = 1.5) +
scale_color_manual(
    name = "",
    values = brewer.pal(3, ct),
    labels = c(
        "Demand Deposits",
        "Cash Outside Banks",
        "Quasi Money"
        )
) +
labs(
    x = "Year", y = "Year-on-Year Percent Change"
) +
theme_minimal() +
theme(legend.position = "bottom")

plot(p)

png("Images/yoy_of_m2_zoom.png", width = img_l, height = img_h, res = img_r) #nolint.
plot(p)
dev.off()

tikz("svgimages/yoy_of_m2_zoom.tex", width = 6, height = 4, pointsize = 12)
print(p)
dev.off()

################################################################
#Decomposing Quasi Money
#potential components
# 1. time deposits
# 2. foreign currency deposits
# 3. money market mutual funds
# 4. Short term government securities
# 5. Repos?

#load cbn Depository Corporations Survey
cbn <- read.csv("Data/cbn.csv")

#gen clean date
cbn$Date <- seq.Date(
    from = as.Date("2007-12-1"),
    by = "month", length.out = nrow(cbn)
    )

#prepare shares for plot
cbn$cbn_ngn <- ((cbn$Central.Bank.8 - cbn$Of.Which..Foreign.Currency.Deposit) / cbn$Other.deposits) * 100 #nolint
cbn$cbn_fx <- (cbn$Of.Which..Foreign.Currency.Deposit / cbn$Other.deposits) * 100 #nolint

cbn$com_ngn <- ((cbn$Commercial...Merchant.Banks.8 - cbn$Of.Which..Foreign.Currency.Deposit.1) / cbn$Other.deposits) * 100 #nolint
cbn$com_fx <- (cbn$Of.Which..Foreign.Currency.Deposit.1 / cbn$Other.deposits) * 100 #nolint

cbn$oth_all <- ((
    cbn$Non.Interest.Banks.8 + cbn$Primary.Mortgage.Banks.8 +
    cbn$Micro.Finance.Banks.8
) / cbn$Other.deposits) * 100

temp <- melt(
  cbn[c("Date", "cbn_ngn", "cbn_fx", "com_ngn", "com_fx", "oth_all")],
  id.vars = c("Date"),     # Columns to keep as is
  variable.name = "component",
  value.name = "percent"
)

#Plot Composition of Quasi Money
p <- ggplot(
    data = temp,
    aes(x = Date, y = percent, fill = component)
) +
geom_bar(stat = "Identity") +
scale_fill_manual(
    name = "",
    values = brewer.pal(5, ct),
    labels = c(
        "CBN - NGN",
        "CBN - FX",
        "Commercial and Merchant Banks - NGN",
        "Commercial and Merchant Banks - FX",
        "Others - NGN and FX"
        )
) +
labs(
    x = "Year", y = "Percent of Quasi Money"
) +
theme_minimal() +
theme(legend.position = "bottom")

plot(p)

png("Images/perc_of_qm.png", width = img_l, height = img_h, res = img_r) #nolint.
plot(p)
dev.off()

tikz("svgimages/perc_of_qm.tex", width = 6, height = 4, pointsize = 12)
print(p)
dev.off()

###################################################################
#Money creation analysis

#1. Private sector credit
priv_cred <- read.csv("Data/private_credit.csv")
priv_cred$Date <- seq.Date(
    from = as.Date("2007-01-1"),
    by = "month", length.out = nrow(priv_cred)
    )

#Calculate growth rate
priv_cred <- priv_cred %>%
  arrange(Date) %>% # Ensure data is ordered by date
  mutate(pc_yoy = (credit_private / lag(credit_private, 12) - 1) * 100)

priv_cred <- priv_cred %>%
  arrange(Date) %>% # Ensure data is ordered by date
  mutate(loans_ngn_yoy = (loans_ngn / lag(loans_ngn, 12) - 1) * 100)

priv_cred <- priv_cred %>%
  arrange(Date) %>% # Ensure data is ordered by date
  mutate(loans_fx_yoy = (loans_fx / lag(loans_fx, 12) - 1) * 100)


#plot vs GDP
p <- ggplot() +
    geom_line(
        data = priv_cred,
        aes(x = Date, y = pc_yoy, color = "Private Sector Credit")
    ) +
    geom_line(
        data = priv_cred,
        aes(x = Date, y = loans_ngn_yoy, color = "Private Sector Credit - NGN")
    ) +
    geom_line(
        data = priv_cred,
        aes(x = Date, y = loans_fx_yoy, color = "Private Sector Credit - FX")
    ) +
    geom_line(
        data =  gdp,
        aes(x = Date, y = ngdp_yoy, color = "Nominal GDP growth")
    ) +
    scale_color_manual(
    name = "",
    values = brewer.pal(4, ct),
    labels = c(
        "Nominal GDP",
        "Private Sector Credit - All",
        "Private Sector Credit - FX",
        "Private Sector Credit - NGN"
        )
) +
    labs(
    x = "Year", y = "Percent Change"
) +
theme_minimal() +
theme(legend.position = "bottom")

plot(p)

png("Images/priv_gr.png", width = img_l, height = img_h, res = img_r) #nolint.
plot(p)
dev.off()

tikz("svgimages/priv_gr.tex", width = 6, height = 4, pointsize = 12)
print(p)
dev.off()

#2 Private credit values
#plot vs GDP
p <- ggplot(
        data = priv_cred[which(!is.na(priv_cred$loans_ngn)), ],
        aes(x = Date)
    ) +
    geom_line(
        aes(y = loans_ngn / 1000000,
        color = "Private Sector Credit - NGN")
    ) +
    geom_line(
        aes(y = loans_fx / 1000000,
        color = "Private Sector Credit - FX")
    ) +
    scale_color_manual(
    name = "",
    values = brewer.pal(3, ct),
) +
    labs(
    x = "Year", y = "NGN (tn)"
) +
theme_minimal() +
theme(legend.position = "bottom")

plot(p)

png("Images/priv_credit_nom.png", width = img_l, height = img_h, res = img_r) #nolint.
plot(p)
dev.off()

tikz("svgimages/priv_credit_nom.tex", width = 6, height = 4, pointsize = 12)
print(p)
dev.off()

###Public sector credit growth
debt <- read.csv("Data/debtstock.csv")
debt$Date <- seq.Date(
    from = as.Date("2010-03-1"),
    by = "quarter", length.out = nrow(debt)
    )
#cal growth rates
debt <- debt %>%
  arrange(Date) %>% # Ensure data is ordered by date
  mutate(all_debt_yoy = (Total.Domestic.Debt / lag(Total.Domestic.Debt, 12) - 1) * 100)

debt <- debt %>%
  arrange(Date) %>% # Ensure data is ordered by date
  mutate(tbills_yoy = (Treasury.Bills / lag(Treasury.Bills, 12) - 1) * 100)

#Plot growth rates vs GDP
#plot vs GDP
p <- ggplot() +
    geom_line(
        data = debt,
        aes(x = Date, y = all_debt_yoy, color = "All Public Debt")
    ) +
    geom_line(
        data = debt,
        aes(x = Date, y = tbills_yoy, color = "Treasury Bills")
    ) +
    geom_line(
        data = priv_cred,
        aes(x = Date, y = pc_yoy, color = "Private Sector Credit")
    ) +
    geom_line(
        data =  gdp,
        aes(x = Date, y = ngdp_yoy, color = "Nominal GDP growth")
    ) +
    scale_color_manual(
    name = "",
    values = brewer.pal(4, ct),
    labels = c(
        "All Public Debt",
        "Nominal GDP growth",
        "Private Sector Credit",
        "Treasury Bills"
        )
) +
    labs(
    x = "Year", y = "Percent Change"
) +
theme_minimal() +
theme(legend.position = "bottom")

plot(p)

png("Images/debt_gr.png", width = img_l, height = img_h, res = img_r) #nolint.
plot(p)
dev.off()

tikz("svgimages/debt_gr.tex", width = 6, height = 4, pointsize = 12)
print(p)
dev.off()

#Add debt to CBN to total public debt
cbn_debt <- read.csv("Data/cbn_credit_fg.csv")
cbn_debt$Date <- seq.Date(
    from = as.Date("2007-12-1"),
    by = "month", length.out = nrow(cbn_debt)
    )

temp <- merge(
    debt,
    cbn_debt,
    by = "Date"
)
temp$debt_plus_cbn <-
    temp$Total.Domestic.Debt +
    temp$Securities.Central.Government.NC +
    temp$Loans.Central.Government.NC

head(temp)

#plot debt stock private vs public
p <- ggplot() +
    geom_line(
        data = priv_cred,
        aes(x = Date, y = credit_private / 1000000, color = "Total Private Credit")
    ) +
    geom_line(
        data = debt,
        aes(x = Date, y = Total.Domestic.Debt / 1000000, color = "Total Domestic Debt")
    ) +
    geom_line(
        data = debt,
        aes(x = Date, y = Treasury.Bills / 1000000, color = "Treasury Bills")
    ) +
    geom_line(
        data = temp,
        aes(x = Date, y = debt_plus_cbn / 1000000, color = "Total Domestic Debt plus Debt to CBN") #nolint
    ) +
    scale_color_manual(
    name = "",
    values = brewer.pal(4, ct)
) +
    labs(
    x = "Year", y = "NGN Billions"
) +
theme_minimal() +
theme(legend.position = "bottom")

plot(p)

png("Images/debt_nom.png", width = img_l, height = img_h, res = img_r) #nolint.
plot(p)
dev.off()

tikz("svgimages/debt_nom.tex", width = 6, height = 4, pointsize = 12)
print(p)
dev.off()

#FX creating money supply
fxcbn <- read.csv("Data/cbnfx.csv")
fxcbn$Date <- seq.Date(
    from = as.Date("2007-12-1"),
    by = "month", length.out = nrow(cbn_debt)
    )

temp <- merge(
    ms,
    fxcbn,
    by = "Date",
    all.x = TRUE
)

head(temp)
temp$cbnfxom2 <- temp$fxloans / temp$moneySupply_M2
temp$cmmfxdepm2 <- temp$commfxliab / temp$moneySupply_M2

cbnsd <- read.csv("Data/cbnsd.csv")
cbnsd$Date <- seq.Date(
    from = as.Date("2001-12-1"),
    by = "month", length.out = nrow(cbnsd)
    )

temp <- merge(
    temp, cbnsd, by = "Date", all.x = TRUE
)

temp$domdepm2 <- temp$Domiciliary.Accounts / temp$moneySupply_M2

p <- ggplot(
    data = temp[which(year(temp$Date) > 2000), ],
    aes(x = Date)
) +
geom_line(
    aes(y = cbnfxom2 * 100, color = "CBN FX liabilities")
) +
geom_line(
    aes(y = cmmfxdepm2 * 100, color = "Commercial Bank FX Deposits")
) +
geom_line(
    aes(y = domdepm2 * 100, color = "Domiciliary Accounts Deposits")
) +
    scale_color_manual(
    name = "",
    values = brewer.pal(3, ct)
) +
    labs(
    x = "Year", y = "Percent of Money Supply - M2"
) +
theme_minimal() +
theme(legend.position = "bottom")


plot(p)

png("Images/cbn_dom_liab.png", width = img_l, height = img_h, res = img_r) #nolint.
plot(p)
dev.off()

tikz("svgimages/cbn_dom_liab.tex", width = 6, height = 4, pointsize = 12)
print(p)
dev.off()

####real interest rates and private sector credit. 

rates <- read.csv("Data/rates.csv")
rates$Date <- seq.Date(
    from = as.Date("2000-1-1"),
    by = "month", length.out = nrow(rates)
    )

rates$r_mpr <- rates$mpr - rates$inf
rates$r_tbill <- as.numeric(rates$tbill) - rates$inf
rates$r_prime <- rates$prime - rates$inf

View(rates)

p <- ggplot(
    data = rates[which(year(rates$Date) > 2009), ],
    aes(x = Date)
) +
geom_line(
    aes(y = mpr, color = "MPR")
) +
geom_line(
    aes(y = prime, color = "Prime Rate")
) +
geom_line(
    aes(y = r_prime, color = "Real Prime Rate")
) +
geom_line(
        data = priv_cred[which(year(priv_cred$Date) > 2009), ],
        aes(x = Date, y = pc_yoy, color = "Private Sector Credit"),
        linewidth = 1.2
    ) +
geom_line(
        data = priv_cred[which(year(priv_cred$Date) > 2009), ],
        aes(x = Date, y = loans_ngn_yoy, color = "Private Sector Credit - NGN"),
        linewidth = 1.2
    ) +
scale_color_manual(
    name = "",
    values = brewer.pal(5, ct)
) +
    labs(
    x = "Year", y = "Percent"
) +
theme_minimal() +
theme(legend.position = "bottom")

plot(p)

png("Images/rates.png", width = img_l, height = img_h, res = img_r) #nolint.
plot(p)
dev.off()

tikz("svgimages/rates.tex", width = 6, height = 4, pointsize = 12)
print(p)
dev.off()


####
deficit <- read.csv("Data/deficit.csv")
df_long <- melt(
    deficit[c("year", "fgdp", "pbalgdp")],
    id.vars = "year", variable.name = "category", value.name = "value"
    )

# Plot
v <- brewer.pal(3, ct)
p <- ggplot(df_long, aes(x = factor(year), y = value, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Value", fill = "Category") +
  scale_fill_manual(
    name = "",
    labels = c("fgdp" = "Fiscal Deficit", "pbalgdp" = "Primary Balance"),
    values = c("fgdp" = v[1], "pbalgdp" = v[2])
    ) +
labs(
    x = "Year", y = "Percent of GDP"
) +
  theme_minimal() +
theme(legend.position = "bottom")

plot(p)

png("Images/deficit.png", width = img_l, height = img_h, res = img_r) #nolint.
plot(p)
dev.off()

tikz("svgimages/deficit.tex", width = 6, height = 4, pointsize = 12)
print(p)
dev.off()

#debt composition
head(debt)
debt$sh_tb <- (debt$Treasury.Bills / debt$Total.Domestic.Debt) * 100

p <- ggplot(
    debt,
    aes(x = Date, y = sh_tb)
) +
geom_bar(
    stat = "identity", fill = v[2]
) +
labs(
    x = "Year", y = "Percent of Total Debts"
) +
  theme_minimal() +
theme(legend.position = "bottom")

plot(p)
png("Images/sh_tb.png", width = img_l, height = img_h, res = img_r) #nolint.
plot(p)
dev.off()
View(debt)

tikz("svgimages/sh_tb.tex", width = 6, height = 4, pointsize = 12)
print(p)
dev.off()
