trumpilot <- read.csv("merge1618.csv")

## Generate measure of vote shift
trumpilot$votech <- with(trumpilot, log(Rep18*Dem16/(Rep16*Dem18)))

## Generate state variable for state fixed effects
trumpilot$statefips <- as.factor(floor(trumpilot$FIPS/1000))

## All counties, predicted using percent of county GDP from soybeans
summary(lm(data = subset(trumpilot, is.finite(votech)), votech ~ soyval12))
summary(lm(data = subset(trumpilot, is.finite(votech)), votech ~ log(1 + soyval12)))
summary(lm(data = subset(trumpilot, is.finite(votech)), votech ~ statefips + soyval12))
summary(lm(data = subset(trumpilot, is.finite(votech)), votech ~ statefips + log(1 + soyval12)))

## Counties not split between House districts, predicted using percent of county GDP from soybeans
summary(lm(data = subset(trumpilot, is.finite(votech) & Splitflag == 0), votech ~ soyval12))
summary(lm(data = subset(trumpilot, is.finite(votech) & Splitflag == 0), votech ~ log(1 + soyval12)))
summary(lm(data = subset(trumpilot, is.finite(votech) & Splitflag == 0), votech ~ statefips + soyval12))
summary(lm(data = subset(trumpilot, is.finite(votech) & Splitflag == 0), votech ~ statefips + log(1 + soyval12)))

## All counties, predicted using percent of county area used for soybean planting
summary(lm(data = subset(trumpilot, is.finite(votech)), votech ~ soypct))
summary(lm(data = subset(trumpilot, is.finite(votech)), votech ~ log(1 + soypct)))
summary(lm(data = subset(trumpilot, is.finite(votech)), votech ~ statefips + soypct))
summary(lm(data = subset(trumpilot, is.finite(votech)), votech ~ statefips + log(1 + soypct)))

## Counties not split between House districts, predicted using percent of county area used for soybean planting
summary(lm(data = subset(trumpilot, is.finite(votech) & Splitflag == 0), votech ~ soypct))
summary(lm(data = subset(trumpilot, is.finite(votech) & Splitflag == 0), votech ~ log(1 + soypct)))
summary(lm(data = subset(trumpilot, is.finite(votech) & Splitflag == 0), votech ~ statefips + soypct))
summary(lm(data = subset(trumpilot, is.finite(votech) & Splitflag == 0), votech ~ statefips + log(1 + soypct)))