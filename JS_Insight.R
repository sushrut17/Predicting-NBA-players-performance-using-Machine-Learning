

library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(dummies)
library(faraway)
library(Hmisc)
library(MatchIt)
library("psych")
library(ggalt)
library(ggExtra)
library("coefplot")
library(car)
library("jtools")
library("huxtable")
library(MASS)

#reading the dat
dat <- read.csv("~/Documents/NBA_Social_Influence/final_master_data_file.csv")
head(dat)


#############Initial VISUALIZATION#############

#SALARY VS MP
dat_sal <- dat[!(is.na(dat_sal$SALARY)) & (dat_sal$POSITION != 'PF-C'),]
dat_sal['MP_above_avg'] <- ifelse(dat_sal$MP > mean(dat_sal$MP),1,0)
dat_sal['salary_norm'] <- (dat_sal$SALARY - mean(dat_sal$SALARY))/sd(dat_sal$SALARY)
dim(dat_sal)

ggplot(aes(x=as.factor(MP_above_avg),y=SALARY, fill = as.factor(MP_above_avg)),data=dat_sal)+
  geom_violin(trim=FALSE)+
  ggtitle("Salary Range for Below vs Above Average Minute Played")+
  geom_boxplot(width=0.1, fill = "white") 

t.test(SALARY~MP_above_avg, data=dat_sal) ##P value showed significant differences

#effect of MP of salary = 34% R squared
sal <- lm(log(SALARY)~MP,data=dat_sal)
summary(sal)
exp(1)**coef(sal)

#how much higher is salary for players above avg in MP vs Lower avg? ~ 158%
(10656070-4136981)/4136981 

#looking at salary by position
ggplot(aes(x=POSITION,y=SALARY, fill = POSITION),data=dat_sal)+
  geom_violin(trim=FALSE)+
  ggtitle("Salary Range Based on Position")+
  geom_boxplot(width=0.1, fill = "white")

##Omitting the NA's
dat.full <- na.omit(dat_sal)

#looking at the dist
dat.full %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_density(fill="lightskyblue3")

##Adding the log var for social media

dat.full$TWITTER_FAVORITE_COUNT_log <- log(dat.full$TWITTER_FAVORITE_COUNT+1)
dat.full$TWITTER_RETWEET_COUNT_log <-log(dat.full$TWITTER_RETWEET_COUNT+1)
dat.full$pageview_mean_log <- log(dat.full$pageview_mean)

options("scipen"=100, "digits"=4)

###########OLS MODELS : DO RESIDUAL PLOTS FOR EACH MODELS
##Social Media Model
soc_med <- lm(WINS~TWITTER_FAVORITE_COUNT_log+
                TWITTER_RETWEET_COUNT_log+
                pageview_mean_log,data=dat.full)
summary(soc_med)
vif(soc_med)
ggplot(aes(x=.fitted,y=.resid),data=soc_med) + 
  geom_point(col="steelblue")+ geom_hline(yintercept = 0) +
  ggtitle("Residual Plot Social Media Model") +
  labs(x="Fitted Values",y="Residuals")

##Salary
Salary <- lm(WINS~SALARY,data=dat_sal)
summary(Salary)
ggplot(aes(x=.fitted,y=.resid),data=Salary) + 
  geom_point(col="steelblue")+ geom_hline(yintercept = 0) +
  ggtitle("Residual Plot Salary Model") +
  labs(x="Fitted Values",y="Residuals")


Salary_norm <- lm(WINS~salary_norm,data=dat_sal)
summary(Salary_norm)
ggplot(aes(x=.fitted,y=.resid),data=Salary_norm) + 
  geom_point(col="steelblue")+ geom_hline(yintercept = 0) +
  ggtitle("Residual Plot Salary Model") +
  labs(x="Fitted Values",y="Residuals")


theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(dat_sal, aes(salary_norm, WINS)) + 
  geom_point() + 
  geom_smooth(method="lm")+
  ggtitle("Salary vs Wins")
ggMarginal(g, type = "density", fill="steelblue")
  

##Time Model
Minute <- lm(WINS~MP,data=dat_sal)
summary(Minute)
ggplot(aes(x=.fitted,y=.resid),data=Minute) + 
  geom_point(col="steelblue")+ geom_hline(yintercept = 0) +
  ggtitle("Residual Plot Minute Model") +
  labs(x="Fitted Values",y="Residuals")

g <- ggplot(dat_sal, aes(MP, WINS)) + 
  geom_point() + 
  geom_smooth(method="lm")+
  ggtitle("Minutes Played vs Wins")
ggMarginal(g, type = "density", fill="steelblue")

#test for heteroskedasticity
lmtest::bptest(Minute)

#weighted OLS for Time MOdel
min.weights <- 1 / lm(abs(Minute$residuals) ~ Minute$fitted.values)$fitted.values^2
Minute.lmw <- rlm(WINS ~ MP, 
              data = dat_sal, 
              weights = min.weights)
summary(Minute.lmw)
ggplot(aes(x=.fitted,y=.resid),data=Minute.lmw) + 
  geom_point(col="steelblue")+ geom_hline(yintercept = 0) +
  ggtitle("Residual Plot Minute Weighted Model") +
  labs(x="Fitted Values",y="Residuals")


######Do the MP vs WINS by Position plot--> use encircle
selected <- dat_sal[dat_sal$MP > 30 & 
                            dat_sal$WINS > 15,]

ggplot(dat_sal, aes(x=MP, y=WINS)) + 
  geom_point(aes(col=POSITION, size=SALARY)) +   # draw points
  scale_color_brewer(type='qual')+
  geom_smooth() + 
  ylim(0,25)+
geom_encircle(aes(x=MP, y=WINS), 
              data=selected, 
              color="red", 
              size=2, 
              expand=0.05)+
  labs(subtitle="by Salary and Position", 
       y="WINS", 
       x="MP", 
       title="Wins vs Minutes Played Comparison") + 
  theme_gray() 
  

##Defense Model
defense_df <- dat_sal[,c("DRB","STL","BLK","WINS")]
defense <- lm(WINS~.,data=defense_df)
summary(defense)
coefplot(defense)
ggplot(aes(x=.fitted,y=.resid),data=defense) + 
  geom_point(col="steelblue")+ geom_hline(yintercept = 0) +
  ggtitle("Residual Plot Defense Model") +
  labs(x="Fitted Values",y="Residuals")
crPlots(defense)



##Offense Model
offense_df <- dat_sal[,c("eFG.","FT.","ORB","AST","PS.G","WINS")]
offense <- lm(WINS~.,data=offense_df)
summary(offense)
coefplot(offense)
ggplot(aes(x=.fitted,y=.resid),data=offense) + 
          geom_point(col="steelblue")+ geom_hline(yintercept = 0) +
          ggtitle("Residual Plot Offense Model") +
          labs(x="Fitted Values",y="Residuals")
crPlots(offense)


###Interaction Terms
##Putting it all together = Estimating Wins based on Salary and Minute Play
summary(dat_sal$MP)
cor(dat_sal$MP, dat_sal$SALARY)
table(dat_sal$MP_above_avg)

salary_MP <- lm(WINS~salary_norm*MP_above_avg,data=dat_sal)
summary(salary_MP)


####MODELS SUMMARY

export_summs(soc_med, Salary_norm, Minute, salary_MP, defense, offense
             ,scale = TRUE)


plot_summs(soc_med, Salary_norm, Minute, salary_MP, defense,offense
           , scale = TRUE)

plot_summs(soc_med
           , scale = FALSE, plot.distributions = TRUE, inner_ci_level = .9)
plot_summs(Salary_norm
           , scale = FALSE, plot.distributions = TRUE, inner_ci_level = .9)
plot_summs(Minute
           , scale = FALSE, plot.distributions = TRUE, inner_ci_level = .9)
plot_summs(salary_MP
           , scale = FALSE, plot.distributions = TRUE, inner_ci_level = .9)
plot_summs(defense
           , scale = FALSE, plot.distributions = TRUE, inner_ci_level = .9)
plot_summs(offense
           , scale = FALSE, plot.distributions = TRUE, inner_ci_level = .9)


#######PROPENSITY SCORE BASED ON MINUTE PLAY
# is there differences in salary based on minute play?
df <- select(dat_sal, -c(pageview_mean,TWITTER_FAVORITE_COUNT,TWITTER_RETWEET_COUNT))
df <- na.omit(df)
num_df <- df[,sapply(df, is.numeric)]
num_df <- select(num_df, -c(Rk,salary_norm))
mean_comp_before <- as.data.frame(
                      num_df %>%
                      keep(is.numeric) %>% 
                      group_by(MP_above_avg) %>%
                      summarise_all(funs(mean(., na.rm = T)))
                    )

test <- c(colnames(mean_comp_before[,2:35]))
p <- sapply(test, function(v) {
  t.test(num_df[, v] ~ num_df$MP_above_avg)$p.value
})


describeBy(df[,sapply(df, is.numeric)],
           df[,sapply(df, is.numeric)]$MP_above_avg)


m_ps <- glm(MP_above_avg ~ PS.G+DRB+STL+PIE+AGE, ##Best combination to prod lowest aic
            family = binomial(), data = df)
summary(m_ps) 

prs_df <- data.frame(pr_score = predict(m_ps, type = "response"),
                     MP_above_avg = m_ps$model$MP_above_avg)
head(prs_df)

labs <- paste("Minutes Played", c("Above Avg", "Below Avg"))
prs_df %>%
  mutate(MP_above_avg = ifelse(MP_above_avg == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~MP_above_avg) +
  xlab("Probability of going to Have Minutes Played Above Average") +
  theme_bw()
#checking for NA
sapply(df, function(x) sum(is.na(x)))

mod_match <- matchit(MP_above_avg ~ PS.G+DRB+STL+PIE+AGE,
                     method = "nearest", data = df)

# We can get some information about how successful the matching was using summary(mod_match) and plot(mod_match)
summary(mod_match)
plot(mod_match)

dta_m <- match.data(mod_match)
dim(dta_m)

t.test(SALARY~MP_above_avg, data=dta_m) ##P value showed significant differences
ggplot(aes(x=as.factor(MP_above_avg),y=SALARY, fill = as.factor(MP_above_avg)),data=dta_m)+
  geom_violin(trim=FALSE)+
  ggtitle("Salary Range for Below vs Above Average Minute Played")+
  geom_boxplot(width=0.1, fill = "white") 

(12141449-4068610)/4068610

