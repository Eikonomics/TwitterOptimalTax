## ## Boring old set-up
library(tidyverse)
library(scales)
rm(list = ls())

### Fun set-up for calculation 

## Baseline data (assumptions)
M <- 6641000      # Soruce: Hagstofan, gross total income 2018
SD <- 7982000     # Soruce: Hagstofan, SD of Total income 2018

MinW <- 50000 * 12    # Min annual income (used to produce chart)
MaxW <- 5000000 * 12  # Max annual income (used to produce chart)

## Create the data to calculate different take-home pay
Data <-  data.frame(
                seq(from = MinW, to = MaxW, by = 12 * 10000)  # 10k per month intervals (over years)
                )
Data <- setNames(Data, c("Gehalt")) # Incomes

## Calculate tax rates for all possible incomes 
Data$PreictedTaxRate <-  1 / (1 + exp(- (Data$Gehalt - M)/SD))  

## Total post-tax income
Data$PredictedAfterTax <- Data$Gehalt * (1 - Data$PreictedTaxRate)

## Total post-tax income, monthly average
Data$PredictedAfterTaxMonthly <- Data$PredictedAfterTax / 12

# marginal pay (additonal take-home pay, per 10k wage increase)
Data$marginalTax <- Data$PredictedAfterTaxMonthly - lag(Data$PredictedAfterTaxMonthly, n = 1L)

## Draw the plots

## plot common features
GoodLook <- 
  ggplot(Data) +
  geom_line(size = 1.5) + 
  theme_minimal(base_size = 16) + 
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) 

# Total vs take-home pay
  GoodLook + 
    aes(y = PredictedAfterTaxMonthly, x = Gehalt/12) +
    ylab("Mánaðarlaun, eftir skatt") +
    xlab("Mánaðarlaun, fyrir skatt") 
    

# marginal total income vs take-home pay (net income)
GoodLook +
  aes(y = marginalTax, x = Gehalt/12) +
  xlab("Mánaðarlaun, fyrir skatt") +
  ylab("Breyting á nettó-tekjum") 


