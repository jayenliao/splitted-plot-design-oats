# title: "統計諮詢：Mid-term Project 2"
# subtitle: "國立成功大學統計學系暨數據科學研究所"
# author: "陳溫茹（R26091040）、廖傑恩（RE6094028)、戴庭筠（R26091032）"

library(HH)
library(car)
library(dplyr)
library(ggplot2)
data(oats)
global_mean <- mean(oats$yield)
I <- length(unique(oats$blocks))
J <- length(unique(oats$plots))
K <- length(unique(oats$subplots))

nrow(oats)
ncol(oats)-1

qplot(y = yield, x = factor(blocks), geom = 'boxplot', data = oats) +
  xlab('blocks') + theme_bw()

oats %>% mutate(blocks = paste0('Block ', blocks)) %>% 
  qplot(y = yield, x = factor(variety), geom = 'boxplot', data = .) +
  facet_wrap(. ~ blocks) +
  xlab('variety') + theme_bw()

oats_ <- oats %>% mutate(blocks = paste0('Block ', blocks),
                         variety = paste0('Variety ', variety),
                         nitrogen = factor(nitrogen))
levels(oats_$nitrogen) <- c(0, 0.2, 0.4, 0.6)
ggplot(aes(y = yield, x = factor(nitrogen)), data = oats_) +
  geom_point() +
  geom_segment(aes(y = 0, x = factor(nitrogen), yend = yield, xend = factor(nitrogen))) +
  xlab('Nitrogen Level') +
  facet_wrap(variety ~ blocks, ncol = 6) + theme_bw()

# 資料分析

## 檢查前提假設是否滿足

sw_test <- shapiro.test(oats$yield)
round(sw_test$statistic, 4)
round(sw_test$p.value, 4)

bartlett_ <- oats %>% mutate(group = paste0(variety, nitrogen)) %>% 
  bartlett.test(yield ~ group, data = .)
k2 <- round(bartlett_$statistic, 4)
p_k2 <- round(bartlett_$p.value, 4)

leveneTest_ <- oats %>% mutate(group = paste0(variety, nitrogen)) %>%
  leveneTest(yield ~ group, data = .)
l <- round(leveneTest_$`Pr(>F)`[1], 4)
p_l <- round(leveneTest_$`Pr(>F)`[1], 4)


k2
p_k2 
l
p_l

### 檢查ANOVA table SS

# total sum of sq
S <- (oats$yield - mean(oats$yield))^2
S <- sum(S)

# block SS
S <- tapply(oats$yield, oats$blocks, mean) - global_mean

# Error: blocks
S <- tapply(oats$yield, oats$blocks, mean) - global_mean
a1 <-round(sum(S^2)*3*4,0)

# Error: blocks:plots
mtx <- tapply(oats$yield, oats[, c('blocks', 'plots')], mean)
S <- mtx - rowMeans(mtx)
a2 <- sum(S^2)
a2 <- round(a2*4, digits = 0)

S <- tapply(oats$yield, oats$variety, mean) - global_mean
a3 <- sum(S^2)
a3 <-  round(a3*6*4, digits = 0)

Error1 <- round(a2-a3, digits = 0)

# Error: blocks:plots:subplots
S <- tapply(oats$yield, oats$variety, mean)
S <- (S - global_mean)^2
a4 <- round(sum(S)*I*J, digits = 0)

S <- 15875 + 1786 + 6013 + 20021 + 322 + 7969   

## 分析結果

oats_new <- oats %>% mutate(
  blocks = as.factor(blocks),
  plots = as.factor(plots),
  subplots = as.factor(subplots),
  variety = as.factor(variety),
  nitrogen = as.factor(nitrogen)
)

### 主要結果

yatesppl.aov <-
  aov(yield ~ variety*nitrogen + Error(blocks/plots/subplots), data=oats_new)
library(rstatix)
df <- anova_summary(yatesppl.aov, detailed = TRUE)
df_ <- data.frame(
  變異來源 = c('殘差：blocks', '作物種類（V）', '殘差：blocks:plots',
               '氮肥濃度（N）', 'V與N交互作用', '殘差：blocks:plots:subplots'),
  自由度 = c(5, df$DFn[1], df$DFd[1], df$DFn[-1], df$DFd[2]),
  平方和 = round(c(15875, df$SSn[1], df$SSd[1], df$SSn[-1], df$SSd[2]))
)
df_$均方 = round(df_$平方和 / df_$自由度)
df_$`F值` = c('', round(df$`F`[1], 2), '', round(df$`F`[-1], 2), '')
df_$`p值` = c('', round(df$p[1], 2), '', round(df$p[-1], 2), '')
knitr::kable(df_)

oats_new_2 <- group_by(oats,nitrogen,variety)%>%
  summarise(yield.mean = mean(yield)) %>% as.data.frame()
oats_new_2$nitrogen <- oats_new_2$nitrogen %>% as.character()
oats_new_2$variety <- oats_new_2$variety %>% as.character()

interaction1 <- 
ggplot(oats_new_2)+
  geom_line(aes(x = variety , y = yield.mean, group = nitrogen, color = nitrogen), size = 1) +
  labs(title = "Interaction Plot of\nNitrogen and Varity",
       x = 'Variety', y = 'Yield', colour = 'Nitrogen') + theme_bw() +
  theme(legend.position = 'top')

interaction2 <- 
ggplot(oats_new_2)+
  geom_line(aes(x = nitrogen , y = yield.mean, group = variety, color = variety), size = 1) +
  labs(title = "Interaction Plot of\nNitrogen and Varity", 
       x = 'Nitrogen', y = 'Yield',
       colour = 'Variety')+ theme_bw() +
   theme(legend.position = 'top')

gridExtra::grid.arrange(interaction1, interaction2, ncol = 2)

### 模型殘差診斷

#### 診斷1：針對$\eta_{ij}$的診斷

par(mfcol = c(2, 2))
plot(aov, which = 1)
plot(aov, which = 3)
plot(aov, which = 2)
plot(aov, which = 5)
ggplot2::autoplot(object = aov_, which=c(1:3, 5))

oats_new_y_hio <- oats_new %>%
  group_by(blocks, variety) %>% summarise(y_hio = 2*mean(yield))
oats_new_y_hio <- data.frame(
  y_hio = c(286.0, 266.5, 259.5, 183.0, 216.0, 190.0,
            174.5, 226.5, 242.5, 184.0, 191.0, 170.5,
            165.0, 173.5, 237.0, 179.0, 180.5, 218.0),
  blocks = factor(rep(1:6, each = 3)),
  variety = factor(rep(1:3, 6))
)

aov_ <- aov(y_hio ~ blocks + variety, data = oats_new_y_hio)
sw_test_res1 <- shapiro.test(aov_$residuals)
p.1 <- round(sw_test_res1$p.value, 4)
W.1 <- round(sw_test_res1$statistic, 4)

fit <- lm(y_hio ~ blocks+ variety, oats_new_y_hio)
ncv <- car::ncvTest(fit)
C <- round(ncv$ChiSquare, 4)
p <- round(ncv$p, 4)

W.1
p.1
C
p

#### 診斷2：針對$\epsilon_{ijk}$的診斷

yatesppl.aov.1 <- aov(yield ~ variety * nitrogen + blocks:variety, data = oats_new)
autoplot(yatesppl.aov.1)

res1 <- residuals(yatesppl.aov.1)
sw_test_res1 <- shapiro.test(res1)
p.1 <- round(sw_test_res1$p.value, 4)
W.1 <- round(sw_test_res1$statistic, 4)

fit <- lm(yield ~ variety * nitrogen + blocks:variety, data = oats_new)
ncv <- car::ncvTest(fit)
C <- round(ncv$ChiSquare, 4)
P <- round(ncv$p, 4)

W.1
p.1
C
p

### 線性效果之檢驗

contrasts(oats_new$nitrogen) <- contr.poly(4)
oats.aov <- aov(yield~ variety*nitrogen+Error(blocks/plots/subplots) ,data=oats_new)
lst <- summary(oats.aov, expand.split=FALSE,
              split=list(nitrogen=list(linear=1, quad=2, cub=3)))
tb <- lst$`Error: blocks:plots:subplots`
tb

### 事後比較

contrasts(oats_new$nitrogen) <- contr.treatment(4)
oats.wrong <- aov(terms(yield ~ (blocks*variety) + (nitrogen*variety), keep.order = TRUE),
                  data = oats_new)
oats.mmc <- mmc(oats.wrong, focus="nitrogen")
nitrogen.lmat <- contr.poly(4)
rownames(nitrogen.lmat) <- levels(oats_new$nitrogen)
oats.mmc <- mmc(
  oats.wrong, focus="nitrogen",
  focus.lmat=nitrogen.lmat, alternative = "greater", order.contrasts = TRUE)
df <- oats.mmc$mca$table %>% as.data.frame()
colnames(df) <- c('組間差異平均估計值', '標準誤', 'CI下界', 'CI上界')
knitr::kable(round(df, 4)[,-2])
mmcplot(oats.mmc)
