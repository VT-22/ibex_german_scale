require(plyr)
require(ggplot2)
require(scales)
require(lme4)
library(lmerTest)
library(reshape2) # For colsplit

# dsubset refactorize the subset
dsubset <- function (...) { return(droplevels(subset(...))) }

# Reading the table
whole_table <- read.delim(file.choose(), sep=",", comment.char="#", header=F, quote="")
names(whole_table) <-
  c("Time","IP","Controller","Id","Element",
    "Type","Gr","Question","Answer","Correct","RT")



# Getting rid of the final form
whole_table <- dsubset(whole_table, Controller=="DynamicQuestion")


# Enriching the table
whole_table <- cbind(whole_table, colsplit(whole_table$Question, "[+]",
              c( "Item","type2", "Condition", "Group", "Sentence")))

# Getting rid of test trials
# whole_table <- dsubset(whole_table, nchar(as.character(Prolific)) > 10)

# # Remove duplicated lines

whole_table$code <- paste(whole_table$IP, whole_table$Item,sep="")

whole_table <- whole_table[duplicated(whole_table$code),]

nrow(whole_table)

#toDelete <- seq(1, nrow(whole_table), 2)
#whole_table <- whole_table[ toDelete ,]

nrow(whole_table)

ddply(whole_table, .(Group), summarize, length(Item))
# Group ..1
# 1     1 570
# 2     2 494
# 3     3 532
# 4     4 494
# 5     5 456
# 6     6 570
# 7     7 532
# 8     8 532
# 9     9 456

ddply(whole_table, .(Group), summarize, length(Item)/38)
# Group ..1
# 1     1  15
# 2     2  13
# 3     3  14
# 4     4  13
# 5     5  12
# 6     6  15
# 7     7  14
# 8     8  14
# 9     9  12

boxplot(whole_table$RT)

whole_table <- subset(whole_table, RT < 2300 & RT > 200)

boxplot(whole_table$RT)

length(unique(whole_table$IP))
# 122

nrow(whole_table)

nrow(whole_table)/38
# 122

length(unique(paste(whole_table$IP,whole_table$Time)))
# 122


whole_table$Det <- "CT"
whole_table$Det[whole_table$Group%in%c(2,5,8)] <- "F"
whole_table$Det[whole_table$Group%in%c(3,6,9)] <- "Cleft"



ddply(subset(whole_table, Type=="picture"), .(Det, Condition), summarize, TruthRate=mean(Answer=="Validate"), FalseRate=mean(Answer=="Reject"), RT=mean(RT), length(Item))

# Det Condition TruthRate   FalseRate       RT ..4
# 1 Cleft         A 0.9619565 0.038043478 680.1685 184
# 2 Cleft         B 0.6706587 0.329341317 717.1437 167
# 3 Cleft         C 0.6208791 0.379120879 758.7033 182
# 4    CT         A 0.9798995 0.020100503 773.5628 199
# 5    CT         B 0.9906542 0.009345794 724.4533 214
# 6    CT         C 0.9615385 0.038461538 730.3269 208
# 7     F         A 0.9893617 0.010638298 777.2340 188
# 8     F         B 0.9786096 0.021390374 740.2086 187
# 9     F         C 0.9735450 0.026455026 735.9630 189

means <- ddply(subset(whole_table, Type=="picture"), .(Det, Condition), summarize, TruthRate=mean(Answer=="Validate"), FalseRate=mean(Answer=="Reject"), RT=mean(RT), length(Item))

means 
barplot(means[,3], width = 1, space = NULL,
        names.arg = c("ClA", "ClB", "ClC", "CTA", "CTB", "CTC", "FA", "FB", "FC"), legend.text = NULL, beside = FALSE,
        horiz = FALSE, density = NULL, angle = 45,
        col = c("deepskyblue", "deepskyblue2", "deepskyblue4", "aquamarine","aquamarine3","aquamarine4","deeppink","deeppink3","deeppink4"), border = par("fg"),
        main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
        xlim = NULL, ylim = c(0,1), xpd = TRUE, log = "",
        axes = TRUE, axisnames = TRUE,
        cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
        plot = TRUE, axis.lty = 0, offset = 0,
        add = FALSE, args.legend = NULL)

summary(glmer(Answer=="Validate"~ Condition+(1|IP)+(1|Item), family=binomial, data=whole_table))

# Random effects:
#   Groups Name        Variance Std.Dev.
# IP     (Intercept)  1.995   1.413   
# Item   (Intercept) 15.022   3.876   
# Number of obs: 3542, groups:  IP, 122; Item, 38
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   2.3144     0.6877   3.365 0.000765 ***
#   ConditionB   -1.7634     0.3083  -5.720 1.06e-08 ***
#   ConditionC   -2.1425     0.2990  -7.166 7.75e-13 ***

### Condition had significant effect on percentage of "true" answers

summary(glmer(Answer=="Validate"~ Det+(1|IP)+(1|Item), family=binomial, data=whole_table))

# Random effects:
#   Groups Name        Variance Std.Dev.
# IP     (Intercept)  1.166   1.080   
# Item   (Intercept) 12.502   3.536   
# Number of obs: 3542, groups:  IP, 122; Item, 38
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -0.2954     0.6118  -0.483    0.629    
# DetCT         1.6520     0.2972   5.560 2.70e-08 ***
#   DetF          1.7552     0.3047   5.761 8.35e-09 ***

summary(glmer(Answer=="Validate"~ Det+(1|IP)+(1|Item), family=binomial, data=subset(whole_table, Det == "F" | Det == "CT" )))

# none



summary(glmer(Answer=="Validate"~ Det*Condition+(1|IP)+(1|Item), family=binomial, data=subset(whole_table, Det == "F" | Det == "Cleft" )))

# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)      2.29033    0.81313   2.817  0.00485 ** 
#   DetF            -0.06765    0.54012  -0.125  0.90032    
# ConditionB      -3.33235    0.44741  -7.448 9.47e-14 ***
#   ConditionC      -3.44692    0.43354  -7.951 1.86e-15 ***
#   DetF:ConditionB  3.15861    0.64813   4.873 1.10e-06 ***
#   DetF:ConditionC  2.47369    0.55682   4.443 8.89e-06 ***

summary(glmer(Answer=="Validate"~ Det*Condition+(1|IP)+(1|Item), family=binomial, data=subset(whole_table, Det == "Cleft" | Det == "CT" )))


# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        1.7322     0.7111   2.436  0.01486 *  
#   DetCT              0.3424     0.4490   0.763  0.44576    
# ConditionB        -2.3515     0.3906  -6.020 1.75e-09 ***
#   ConditionC        -2.9422     0.3814  -7.715 1.21e-14 ***
#   DetCT:ConditionB   1.3170     0.4856   2.712  0.00669 ** 
#   DetCT:ConditionC   1.9282     0.4838   3.985 6.74e-05 ***

summary(glmer(Answer=="Validate"~ Condition+(1|IP)+(1|Item), family=binomial, data=subset(whole_table, Det =="Cleft")))

# Random effects:
#   Groups Name        Variance Std.Dev.
# IP     (Intercept)  3.044   1.745   
# Item   (Intercept) 16.691   4.085   
# Number of obs: 1141, groups:  IP, 41; Item, 37
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   2.2014     0.8125   2.709  0.00674 ** 
#   ConditionB   -3.0689     0.4667  -6.576 4.84e-11 ***
#   ConditionC   -3.3013     0.4514  -7.313 2.60e-13 ***

### this is true for the cleft

summary(glmer(Answer=="Validate"~ Condition+(1|IP)+(1|Item), family=binomial, data=subset(whole_table, Det =="Cleft" & (Condition == "C" | Condition == "B"))))

# Random effects:
#   Groups Name        Variance Std.Dev.
# IP     (Intercept) 3.718    1.928   
# Item   (Intercept) 7.656    2.767   
# Number of obs: 1066, groups:  IP, 41; Item, 32
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)   0.6543     0.5996   1.091    0.275
# ConditionC   -0.3530     0.2482  -1.422    0.155


summary(glmer(Answer=="Validate"~ Condition+(1|Item), family=binomial, data=subset(whole_table, Det =="F")))

# Random effects:
#   Groups Name        Variance Std.Dev.
# Item   (Intercept) 28.98    5.383   
# Number of obs: 1154, groups:  Item, 38
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)  
# (Intercept)   2.6434     1.1743   2.251   0.0244 *
#   ConditionB    0.2020     0.8022   0.252   0.8012  
# ConditionC   -0.7124     0.6658  -1.070   0.2847 

summary(glmer(Answer=="Validate"~ Condition+(1|IP)+(1|Item), family=binomial, data=subset(whole_table, Det =="F" & (Condition == "C" | Condition == "B"))))


# Random effects:
#   Groups Name        Variance Std.Dev.
# IP     (Intercept)  1.402   1.184   
# Item   (Intercept) 26.447   5.143   
# Number of obs: 768, groups:  IP, 39; Item, 32
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   5.3741     1.5231   3.528 0.000418 ***
#   ConditionC   -1.2001     0.7813  -1.536 0.124542 


summary(glmer(Answer=="Validate"~ Condition+(1|IP)+(1|Item), family=binomial, data=subset(whole_table, Det =="CT")))

# Random effects:
#   Groups Name        Variance  Std.Dev. 
# IP     (Intercept) 1.146e-07 0.0003385
# Item   (Intercept) 1.529e+01 3.9097366
# Number of obs: 1247, groups:  IP, 42; Item, 38
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)  
# (Intercept)   1.5941     0.7946   2.006   0.0448 *
#   ConditionB    1.2523     0.7896   1.586   0.1127  
# ConditionC   -0.4406     0.5319  -0.828   0.4074

summary(glmer(Answer=="Validate"~ Condition+(1|IP)+(1|Item), family=binomial, data=subset(whole_table, Det =="CT" & (Condition == "C" | Condition == "B"))))

# Random effects:
#   Groups Name        Variance Std.Dev.
# IP     (Intercept)  0.4185  0.6469  
# Item   (Intercept) 14.1516  3.7619  
# Number of obs: 841, groups:  IP, 42; Item, 32
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   4.0066     0.9488   4.223 2.42e-05 ***
#   ConditionC   -1.5915     0.7075  -2.250   0.0245 *

## for CT only between B and C


ddply(subset(whole_table, Type=="picture"), .(Condition), summarize, RT=mean(RT), length(Item))

# Condition       RT ..2
# 1         A 744.6760 571
# 2         B 727.4912 568
# 3         C 741.0864 579

ddply(subset(whole_table, Type=="picture"), .(Det), summarize, RT=mean(RT), length(Item))


# Det       RT ..2
# 1 Cleft 718.5704 533
# 2    CT 742.1578 621
# 3     F 751.1277 564


ddply(subset(whole_table, Type=="picture"), .(Answer), summarize, RT=mean(RT), length(Item))

# Answer       RT  ..2
# 1   Reject 868.3782  156
# 2 Validate 724.7420 1562

ddply(subset(whole_table, Type=="picture"), .(Det, Answer), summarize, RT=mean(RT), length(Item))

# Det   Answer        RT ..2
# 1 Cleft   Reject  800.0000 131
# 2 Cleft Validate  692.0348 402
# 3    CT   Reject 1339.0714  14
# 4    CT Validate  728.3904 607
# 5     F   Reject 1083.6364  11
# 6     F Validate  744.5136 553

RT_det <- ddply(subset(whole_table, Type=="picture"), .(Det, Answer), summarize, RT=mean(RT), length(Item))


barplot(RT_det[,3], width = 1, space = NULL,
        names.arg = c("Cleft_no", "Cleft_yes", "CT_no", "CT_yes", "F_no", "F_yes"), legend.text = NULL, beside = FALSE,
        horiz = FALSE, density = NULL, angle = 45,
        col = c("deepskyblue", "deepskyblue2", "aquamarine","aquamarine3","deeppink3","deeppink4"), border = par("fg"),
        main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
        xlim = NULL, ylim = c(0,1500), xpd = TRUE, log = "",
        axes = TRUE, axisnames = TRUE,
        cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
        plot = TRUE, axis.lty = 0, offset = 0,
        add = FALSE, args.legend = NULL)

ddply(subset(whole_table, Type=="picture" & RT < 8000 & RT > 200), .(Det, Condition, Answer), summarize, RT=mean(RT), length(Item))

# Det Condition   Answer        RT ..2
# 1  Cleft         A   Reject  783.0000   7
# 2  Cleft         A Validate  676.1017 177
# 3  Cleft         B   Reject  752.3091  55
# 4  Cleft         B Validate  699.8750 112
# 5  Cleft         C   Reject  839.7391  69
# 6  Cleft         C Validate  709.2212 113
# 7     CT         A   Reject 1297.5000   4
# 8     CT         A Validate  762.8154 195
# 9     CT         B   Reject  643.5000   2
# 10    CT         B Validate  725.2170 212
# 11    CT         C   Reject 1533.7500   8
# 12    CT         C Validate  698.1900 200
# 13     F         A   Reject  823.5000   2
# 14     F         A Validate  776.7366 186
# 15     F         B   Reject 1268.0000   4
# 16     F         B Validate  728.6721 183
# 17     F         C   Reject 1040.2000   5
# 18     F         C Validate  727.6957 184


ddply(subset(whole_table, Type=="picture" & RT < 8000 & RT > 200 & Det == "F"), .(Det, Condition, Answer), summarize, RT=mean(RT), length(Item))


# Det Condition   Answer        RT ..2
# 1   F         A   Reject  823.5000   2
# 2   F         A Validate  776.7366 186
# 3   F         B   Reject 1268.0000   4
# 4   F         B Validate  728.6721 183
# 5   F         C   Reject 1040.2000   5
# 6   F         C Validate  727.6957 184

RT <- ddply(subset(whole_table, Type=="picture" & RT < 8000 & RT > 200 & Det == "F"), .(Det, Condition, Answer), summarize, RT=mean(RT), length(Item))


RT 
barplot(RT[,4], width = 1, space = NULL,
        names.arg = c("FA_no", "FA_yes", "FB_no", "FB_yes", "FC_no", "FC_yes"), legend.text = NULL, beside = FALSE,
        horiz = FALSE, density = NULL, angle = 45,
        col = c("deepskyblue", "deepskyblue2", "aquamarine","aquamarine3","deeppink3","deeppink4"), border = par("fg"),
        main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
        xlim = NULL, ylim = c(0,2000), xpd = TRUE, log = "",
        axes = TRUE, axisnames = TRUE,
        cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
        plot = TRUE, axis.lty = 0, offset = 0,
        add = FALSE, args.legend = NULL)


ddply(subset(whole_table, Type=="picture" & RT < 8000 & RT > 200 & Det == "Cleft"), .(Det, Condition, Answer), summarize, RT=mean(RT), length(Item))

# Det Condition   Answer       RT ..2
# 1 Cleft         A   Reject 783.0000   7
# 2 Cleft         A Validate 676.1017 177
# 3 Cleft         B   Reject 752.3091  55
# 4 Cleft         B Validate 699.8750 112
# 5 Cleft         C   Reject 839.7391  69
# 6 Cleft         C Validate 709.2212 113



RT_cleft <- ddply(subset(whole_table, Type=="picture" & RT < 8000 & RT > 200 & Det == "Cleft"), .(Det, Condition, Answer), summarize, RT=mean(RT), length(Item))


 
barplot(RT_cleft[,4], width = 1, space = NULL,
        names.arg = c("cleftA_no", "CleftA_yes", "CleftB_no", "CleftB_yes", "CleftC_no", "CleftC_yes"), legend.text = NULL, beside = FALSE,
        horiz = FALSE, density = NULL, angle = 45,
        col = c("deepskyblue", "deepskyblue2", "aquamarine","aquamarine3","deeppink3","deeppink4"), border = par("fg"),
        main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
        xlim = NULL, ylim = c(0,1000), xpd = TRUE, log = "",
        axes = TRUE, axisnames = TRUE,
        cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
        plot = TRUE, axis.lty = 0, offset = 0,
        add = FALSE, args.legend = NULL)


ddply(subset(whole_table, Type=="picture" & RT < 8000 & RT > 200 & Det == "CT"), .(Det, Condition, Answer), summarize, RT=mean(RT), length(Item))

# Det Condition   Answer        RT ..2
# 1  CT         A   Reject 1297.5000   4
# 2  CT         A Validate  762.8154 195
# 3  CT         B   Reject  643.5000   2
# 4  CT         B Validate  725.2170 212
# 5  CT         C   Reject 1533.7500   8
# 6  CT         C Validate  698.1900 200



RT_CT <- ddply(subset(whole_table, Type=="picture" & RT < 8000 & RT > 200 & Det == "CT"), .(Det, Condition, Answer), summarize, RT=mean(RT), length(Item))



barplot(RT_CT[,4], width = 1, space = NULL,
        names.arg = c("CTA_no", "CTA_yes", "CTB_no", "CTB_yes", "CTC_no", "CTC_yes"), legend.text = NULL, beside = FALSE,
        horiz = FALSE, density = NULL, angle = 45,
        col = c("deepskyblue", "deepskyblue2", "aquamarine","aquamarine3","deeppink3","deeppink4"), border = par("fg"),
        main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
        xlim = NULL, ylim = c(0,1500), xpd = TRUE, log = "",
        axes = TRUE, axisnames = TRUE,
        cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
        plot = TRUE, axis.lty = 0, offset = 0,
        add = FALSE, args.legend = NULL)

RT_Det_cond <- ddply(subset(whole_table, Type=="picture" & RT < 8000 & RT > 200), .(Det, Condition), summarize, RT=mean(RT), length(Item))



barplot(RT_Det_cond[,3], width = 1, space = NULL,
        names.arg = c("Cleft_A", "Cleft_B", "Cleft_C", "CT_A", "CT_B", "CT_C", "F_A", "F_B", "F_C"), legend.text = NULL, beside = FALSE,
        horiz = FALSE, density = NULL, angle = 45,
        col = c("deepskyblue", "deepskyblue2", "deepskyblue3", "aquamarine","aquamarine3","aquamarine4", "deeppink", "deeppink3","deeppink4"), border = par("fg"),
        main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
        xlim = NULL, ylim = c(0,1000), xpd = TRUE, log = "",
        axes = TRUE, axisnames = TRUE,
        cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
        plot = TRUE, axis.lty = 0, offset = 0,
        add = FALSE, args.legend = NULL)

summary(lmer(RT~Det + (1|IP)+(1+Answer|Item), data=whole_table))


#none 




summary(lmer(RT~Det + (1|IP)+(1+Answer|Item), data=subset(whole_table, Det == "CT" | Det == "F")))

#none

###no effect of determiner on reaction time CT versus F

summary(lmer(RT~Det + (1|IP)+(1+Answer|Item), data=subset(whole_table, Det == "CT" | Det == "Cleft")))

summary(lmer(RT~Det + (1|IP)+(1+Answer|Item), data=subset(whole_table, Det == "F" | Det == "Cleft")))

## nothing and nothing


summary(lmer(RT~Det + (1|IP)+(1|Item), data=subset(whole_table, Answer == "Validate")))

summary(lmer(RT~Det + (1|IP)+(1|Item), data=subset(whole_table, Answer == "Reject")))

##nothing and nothing


summary(lmer(RT~Condition + (1|IP)+(1+Answer|Item), data=whole_table))

# nothing

summary(lmer(RT~Condition + (1|IP)+(1+Answer|Item), data=subset(whole_table, Condition == "C"|  Condition == "B" )))

### no sigfnicant difference between B and C for RT


summary(lmer(RT~Condition + (1|IP)+(1+Answer|Item), data=subset(whole_table, Det == "F" & (Condition == "C"|  Condition == "B" ))))

# nothing

summary(lmer(RT~Condition + (1|IP)+(1+Answer|Item), data=subset(whole_table, Det == "F" & (Condition == "C"|  Condition == "A" ))))

summary(lmer(RT~Condition + (1|IP)+(1+Answer|Item), data=subset(whole_table, Det == "F" & (Condition == "B"|  Condition == "A" ))))

###both nothing


summary(lmer(RT~Condition + (1+Answer|IP)+(1+Answer|Item), data=subset(whole_table, Det == "CT" & (Condition == "C"|  Condition == "B" ))))



summary(lmer(RT~Condition + (1+Answer|IP)+(1+Answer|Item), data=subset(whole_table, Det == "CT" & (Condition == "A"|  Condition == "B" ))))

summary(lmer(RT~Condition + (1+Answer|IP)+(1+Answer|Item), data=subset(whole_table, Det == "CT" & (Condition == "A"|  Condition == "C" ))))

###all nothing

summary(lmer(RT~Condition + (1|IP)+(1+Answer|Item), data=subset(whole_table, Det == "Cleft" & (Condition == "C"|  Condition == "B" ))))

# nothing

summary(lmer(RT~Condition + (1|IP)+(1|Item), data=subset(whole_table, Det == "Cleft" & Answer == "Validate" & (Condition == "C"|  Condition == "B" ))))

summary(lmer(RT~Condition + (1|IP)+(1|Item), data=subset(whole_table, Det == "Cleft" & Answer == "Reject" & (Condition == "C"|  Condition == "B" ))))

# nothing




summary(lmer(RT~Answer + (1|IP)+(1+Answer|Item), data=whole_table))

# Random effects:
#   Groups   Name           Variance Std.Dev. Corr 
# IP       (Intercept)     55725   236.1         
# Item     (Intercept)     40657   201.6         
# AnswerValidate  28659   169.3    -0.95
# Residual                216256   465.0         
# Number of obs: 3542, groups:  IP, 122; Item, 38
# 
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)      896.12      45.72   56.48  19.602  < 2e-16 ***
#   AnswerValidate  -123.28      39.56   36.75  -3.116  0.00355 ** 

### overall validating easier, i.e. significantly faster


summary(lmer(RT~Answer + (1|IP)+(1+Answer|Item), data = subset(whole_table, Det == "Cleft")))

# Random effects:
#   Groups   Name           Variance Std.Dev. Corr 
# IP       (Intercept)     53700   231.7         
# Item     (Intercept)     33949   184.3         
# AnswerValidate  21665   147.2    -0.91
# Residual                215849   464.6         
# Number of obs: 1141, groups:  IP, 41; Item, 37
# 
# Fixed effects:
#   Estimate Std. Error     df t value Pr(>|t|)    
# (Intercept)      855.35      54.30  53.18  15.751   <2e-16 ***
#   AnswerValidate   -87.84      44.02  38.40  -1.995   0.0531 . 

summary(lmer(RT~Answer + (1|IP)+(1+Answer|Item), data = subset(whole_table, Det == "F")))

# Random effects:
#   Groups   Name           Variance Std.Dev. Corr 
# IP       (Intercept)     30275   174.0         
# Item     (Intercept)     67917   260.6         
# AnswerValidate  56203   237.1    -1.00
# Residual                214797   463.5         
# Number of obs: 1154, groups:  IP, 39; Item, 38
# 
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)      952.38      73.43   15.75  12.970 8.11e-10 ***
#   AnswerValidate  -202.34      69.48   13.35  -2.912   0.0118 * 


summary(lmer(RT~Answer + (1|IP)+(1+Answer|Item), data = subset(whole_table, Det == "CT")))

# Random effects:
#   Groups   Name           Variance Std.Dev. Corr 
# IP       (Intercept)     84170   290.1         
# Item     (Intercept)     83128   288.3         
# AnswerValidate 115139   339.3    -1.00
# Residual                212664   461.2         
# Number of obs: 1247, groups:  IP, 42; Item, 38
# 
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)     1051.41      81.58   38.60  12.889 1.33e-15 ***
#   AnswerValidate  -271.74      73.66   23.08  -3.689  0.00121 **


summary(lmer(RT~Answer*Det + (1|IP)+(1+Answer|Item), data = subset(whole_table, (Det == "CT" | Det  == "Cleft"))))

##nothing


ddply(subset(whole_table, Type=="picture" & RT < 8000 & RT > 200 & (Condition == "B" | Condition == "C") & Answer == "Validate"), .(Det, Condition), summarize, RT=mean(RT), length(Item))

RT_IA <- ddply(subset(whole_table, Type=="picture" & RT < 8000 & RT > 200 & (Condition == "B" | Condition == "C") & Answer == "Validate"), .(Det, Condition), summarize, RT=mean(RT), length(Item))


barplot(RT_IA[,3], width = 1, space = NULL,
        names.arg = c("Cleft_B", "Cleft_C", "CT_B", "CT_C", "F_B", "F_C"), legend.text = NULL, beside = FALSE,
        horiz = FALSE, density = NULL, angle = 45,
        col = c("deepskyblue", "deepskyblue2", "aquamarine","aquamarine3","deeppink3","deeppink4"), border = par("fg"),
        main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
        xlim = NULL, ylim = c(0,850), xpd = TRUE, log = "",
        axes = TRUE, axisnames = TRUE,
        cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
        plot = TRUE, axis.lty = 0, offset = 0,
        add = FALSE, args.legend = NULL)


summary(lmer(RT~Det*Condition + (1|IP)+(1|Item), data = subset(whole_table, Answer == "Validate" & (Condition == "B" | Condition == "C") & (Det == "Cleft" | Det == "F"))))
        
# Fixed effects:
#   Estimate Std. Error     df t value Pr(>|t|)    
# (Intercept)       1666.8      268.9   63.9   6.199 4.64e-08 ***
#   DetF              -460.3      217.7  105.8  -2.114   0.0369 *  
#   ConditionC        -237.6      237.5  465.8  -1.000   0.3177    
# DetF:ConditionC    228.0      290.3  453.9   0.785   0.4327

summary(lmer(RT~Det*Condition + (1|IP)+(1|Item), data = subset(whole_table, Answer == "Validate" & (Condition == "B" | Condition == "C") & (Det == "Cleft" | Det == "CT"))))

# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)       1527.59     341.98   65.00   4.467 3.24e-05 ***
#   DetCT              -84.35     276.30  138.00  -0.305    0.761    
# ConditionC         -86.25     339.91 3212.00  -0.254    0.800    
# DetCT:ConditionC   -92.28     409.88 3369.00  -0.225    0.822

summary(lmer(RT~Det*Condition + (1|IP)+(1|Item), data = subset(whole_table, Answer == "Validate" & (Condition == "B" | Condition == "C") & (Det == "CT" | Det == "F"))))


# Fixed effects:
#   Estimate Std. Error     df t value Pr(>|t|)    
# (Intercept)       1382.9      269.6   53.7   5.129 4.09e-06 ***
#   DetF              -339.5      207.3  128.2  -1.638    0.104    
# ConditionC        -139.7      216.2 1241.4  -0.646    0.518    
# DetF:ConditionC    320.6      290.4 1217.9   1.104    0.270 


ddply(subset(whole_table, Type=="picture" & RT < 8000 & RT > 200 & (Condition == "B" | Condition == "C")), .(Det, Condition), summarize, RT=mean(RT), length(Item))

RT_IA_all <- ddply(subset(whole_table, Type=="picture" & RT < 8000 & RT > 200 & (Condition == "B" | Condition == "C")), .(Det, Condition), summarize, RT=mean(RT), length(Item))


barplot(RT_IA_all[,3], width = 1, space = NULL,
        names.arg = c("Cleft_B", "Cleft_C", "CT_B", "CT_C", "F_B", "F_C"), legend.text = NULL, beside = FALSE,
        horiz = FALSE, density = NULL, angle = 45,
        col = c("deepskyblue", "deepskyblue2", "aquamarine","aquamarine3","deeppink3","deeppink4"), border = par("fg"),
        main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
        xlim = NULL, ylim = c(0,1000), xpd = TRUE, log = "",
        axes = TRUE, axisnames = TRUE,
        cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
        plot = TRUE, axis.lty = 0, offset = 0,
        add = FALSE, args.legend = NULL)


summary(lmer(RT~Det*Condition + (1|IP)+(1|Item), data = subset(whole_table, (Condition == "B" | Condition == "C") & (Det == "Cleft" | Det == "F"))))

# Random effects:
#   Groups   Name        Variance Std.Dev.
# IP       (Intercept)  42703   206.6   
# Item     (Intercept)  26359   162.4   
# Residual             216729   465.5   
# Number of obs: 1525, groups:  IP, 80; Item, 32
# 
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)      810.644     51.830  100.300  15.640   <2e-16 ***
#   DetF             -12.244     58.273  111.600  -0.210    0.834    
# ConditionC        39.118     40.426 1263.300   0.968    0.333    
# DetF:ConditionC    8.212     48.233 1409.600   0.170    0.865



summary(lmer(RT~Det*Condition + (1|IP)+(1|Item), data = subset(whole_table, (Condition == "B" | Condition == "C") & (Det == "Cleft" | Det == "CT"))))

# Random effects:
#   Groups   Name        Variance Std.Dev.
# IP       (Intercept)  70301   265.1   
# Item     (Intercept)  33200   182.2   
# Residual             217921   466.8   
# Number of obs: 1598, groups:  IP, 83; Item, 32
# 
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)        802.82      59.92  111.80  13.399   <2e-16 ***
#   DetCT               23.56      67.99  100.60   0.346    0.730    
# ConditionC          53.81      40.70 1413.70   1.322    0.186    
# DetCT:ConditionC   -21.06      47.41 1481.20  -0.444    0.657 

summary(lmer(RT~Det*Condition + (1|IP)+(1|Item), data = subset(whole_table, (Condition == "B" | Condition == "C") & (Det == "CT" | Det == "F"))))
# none


ddply(subset(whole_table, Type=="picture" & RT < 8000 & RT > 200 & (Condition == "B" | Condition == "C")& Answer == "Reject"), .(Det, Condition), summarize, RT=mean(RT), length(Item))

RT_IA_reject <- ddply(subset(whole_table, Type=="picture" & RT < 8000 & RT > 200 & (Condition == "B" | Condition == "C") & Answer == "Reject"), .(Det, Condition), summarize, RT=mean(RT), length(Item))


barplot(RT_IA_reject[,3], width = 1, space = NULL,
        names.arg = c("Cleft_B", "Cleft_C", "CT_B", "CT_C", "F_B", "F_C"), legend.text = NULL, beside = FALSE,
        horiz = FALSE, density = NULL, angle = 45,
        col = c("deepskyblue", "deepskyblue2", "aquamarine","aquamarine3","deeppink3","deeppink4"), border = par("fg"),
        main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
        xlim = NULL, ylim = c(0,1500), xpd = TRUE, log = "",
        axes = TRUE, axisnames = TRUE,
        cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
        plot = TRUE, axis.lty = 0, offset = 0,
        add = FALSE, args.legend = NULL)


summary(lmer(RT~Det*Condition + (1|IP)+(1|Item), data = subset(whole_table, (Condition == "B" | Condition == "C") & (Det == "Cleft" | Det == "F") & Answer == "Reject")))

summary(lmer(RT~Det*Condition + (1|IP)+(1|Item), data = subset(whole_table, (Condition == "B" | Condition == "C") & (Det == "CT" | Det == "F") & Answer == "Reject")))

summary(lmer(RT~Det*Condition + (1|IP)+(1|Item), data = subset(whole_table, (Condition == "B" | Condition == "C") & (Det == "CT" | Det == "Cleft") & Answer == "Reject")))


summary(lmer(RT~Det + (1|IP)+(1|Item), data = subset(whole_table, (Condition == "B" | Condition == "C") & (Det == "Cleft" | Det == "F") & Answer == "Reject")))

summary(lmer(RT~Det + (1|IP)+(1|Item), data = subset(whole_table, (Condition == "B" | Condition == "C") & (Det == "CT" | Det == "F") & Answer == "Reject")))

summary(lmer(RT~Det + (1|IP)+(1|Item), data = subset(whole_table, (Condition == "B" | Condition == "C") & (Det == "CT" | Det == "Cleft") & Answer == "Reject")))

summary(lmer(RT~Condition + (1|IP)+(1|Item), data = subset(whole_table, (Condition == "B" | Condition == "C") & (Det == "Cleft" | Det == "F") & Answer == "Reject")))

summary(lmer(RT~Condition + (1|IP)+(1|Item), data = subset(whole_table, (Condition == "B" | Condition == "C") & (Det == "CT" | Det == "F") & Answer == "Reject")))

summary(lmer(RT~Condition + (1|IP)+(1|Item), data = subset(whole_table, (Condition == "B" | Condition == "C") & (Det == "CT" | Det == "Cleft") & Answer == "Reject")))

# Random effects:
#   Groups   Name        Variance Std.Dev.
# IP       (Intercept)  84804   291.2   
# Item     (Intercept)  51300   226.5   
# Residual             219096   468.1   
# Number of obs: 572, groups:  IP, 82; Item, 30
# 
# Fixed effects:
#   Estimate Std. Error     df t value Pr(>|t|)    
# (Intercept)   821.18      79.84 131.44  10.286   <2e-16 ***
#   ConditionC    156.16      76.82 307.94   2.033   0.0429 *  

summary(lmer(RT~Condition + (1|IP)+(1|Item), data = subset(whole_table, (Condition == "B" | Condition == "C") & Det == "Cleft" & Answer == "Reject")))

summary(lmer(RT~Condition + (1|IP)+(1|Item), data = subset(whole_table, (Condition == "B" | Condition == "C") & Det == "CT" & Answer == "Reject")))


ddply(subset(whole_table, Type=="picture" & RT < 8000 & RT > 200 & (Condition == "A" | Condition == "C")), .(Det, Condition), summarize, RT=mean(RT), length(Item))

RT_IA_AC <- ddply(subset(whole_table, Type=="picture" & RT < 8000 & RT > 200 & (Condition == "A" | Condition == "C")), .(Det, Condition), summarize, RT=mean(RT), length(Item))


barplot(RT_IA_AC[,3], width = 1, space = NULL,
        names.arg = c("Cleft_A", "Cleft_C", "CT_A", "CT_C", "F_A", "F_C"), legend.text = NULL, beside = FALSE,
        horiz = FALSE, density = NULL, angle = 45,
        col = c("deepskyblue", "deepskyblue2", "aquamarine","aquamarine3","deeppink3","deeppink4"), border = par("fg"),
        main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
        xlim = NULL, ylim = c(0,1000), xpd = TRUE, log = "",
        axes = TRUE, axisnames = TRUE,
        cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
        plot = TRUE, axis.lty = 0, offset = 0,
        add = FALSE, args.legend = NULL)

summary(lmer(RT~Det*Condition + (1|IP)+(1|Item), data = subset(whole_table, (Condition == "A" | Condition == "C") & (Det == "Cleft" | Det == "F"))))

# no

summary(lmer(RT~Det*Condition + (1|IP)+(1|Item), data = subset(whole_table, (Condition == "A" | Condition == "C") & (Det == "CT" | Det == "F"))))

# nope


summary(lmer(RT~Det*Condition + (1|IP)+(1|Item), data = subset(whole_table, (Condition == "A" | Condition == "B") & (Det == "Cleft" | Det == "CT"))))


# no

RT_IA_AB <- ddply(subset(whole_table, Type=="picture" & RT < 8000 & RT > 200 & (Condition == "A" | Condition == "B")), .(Det, Condition), summarize, RT=mean(RT), length(Item))


barplot(RT_IA_AB[,3], width = 1, space = NULL,
        names.arg = c("Cleft_A", "Cleft_B", "CT_A", "CT_B", "F_A", "F_B"), legend.text = NULL, beside = FALSE,
        horiz = FALSE, density = NULL, angle = 45,
        col = c("deepskyblue", "deepskyblue2", "aquamarine","aquamarine3","deeppink3","deeppink4"), border = par("fg"),
        main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
        xlim = NULL, ylim = c(0,1000), xpd = TRUE, log = "",
        axes = TRUE, axisnames = TRUE,
        cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
        plot = TRUE, axis.lty = 0, offset = 0,
        add = FALSE, args.legend = NULL)

summary(lmer(RT~Det*Condition + (1|IP)+(1|Item), data = subset(whole_table, (Condition == "A" | Condition == "B") & (Det == "Cleft" | Det == "F"))))

# none
summary(lmer(RT~Det*Condition + (1|IP)+(1+Answer|Item), data = subset(whole_table, (Condition == "A" | Condition == "B") & (Det == "CT" | Det == "F"))))

# nö

summary(lmer(RT~Det*Condition + (1|IP)+(1|Item), data = subset(whole_table, (Condition == "A" | Condition == "B") & (Det == "Cleft" | Det == "CT"))))


###nothing


summary(lmer(RT~Det*Condition + (1|IP)+(1|Item), data = subset(whole_table, Answer == "Validate" & (Condition == "A" | Condition == "B") & (Det == "Cleft" | Det == "F"))))

summary(lmer(RT~Det*Condition + (1|IP)+(1|Item), data = subset(whole_table, Answer == "Validate" & (Condition == "A" | Condition == "B") & (Det == "Cleft" | Det == "CT"))))

##nothing

