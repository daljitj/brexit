library("dplyr")
library("ggplot2")
library("reshape2")
library("ggthemes")
library("grid")

setwd("C:/Users/User/Documents/R/Brexit personality")
getwd()

#load vote data
votes <- read.csv("resultdata.csv") %>%
  select(Area_Code,Pct_Leave,Votes_Cast)

# load trait data
traits <- read.csv("personalitytraits.csv") %>%
  select(LAD.ID,Extraversion=E,Agreeableness=A,Conscientiousness=C,Neuroticism=N,Openness=O)

#merge dataframes
df <- left_join(votes,traits,by=c("Area_Code"="LAD.ID")) %>%
  melt(id.vars=c("Area_Code","Pct_Leave","Votes_Cast"))


#Build regression models 
model <- df %>% group_by(variable) %>%
  do(mod=lm(Pct_Leave~value,data=.,weights=.$Votes_Cast)) %>%
  mutate(intercept=coef(mod)["(Intercept)"]) %>%
  mutate(slope=coef(mod)["value"]) %>%
  mutate(label = paste0("r^2 == ",round(summary(mod)$r.squared,3))) %>%
  select(-mod) %>%
  as.data.frame()

# Plot data
ggplot(df,aes(x=value,y=Pct_Leave)) +
  facet_grid(~variable) +
  geom_point(color="blue",alpha=0.3,shape=20,aes(size=Votes_Cast)) +
  geom_abline(data=model,aes(intercept=intercept,slope=slope),color="red") + 
  geom_text(data=model,aes(label=label,x=90,y=20),color="red",parse=TRUE,hjust=1) +
  theme_few() +
  theme(panel.spacing = unit(1, "lines")) +
  ylab("Percentage voting leave") + xlab("Trait score") +
  guides(size=FALSE) +
  scale_y_continuous(limits=c(20,75),breaks=seq(0,100,10)) +
  labs(title="2016 UK EU Referendum Vote by Personality Traits",
       subtitle="(weighted by population of voting area)",
       caption="Sources: doi:10.1371/journal.pone.0122245 + Electoral Commission")

