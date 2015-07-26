library(ggplot2)
library(dplyr)

tg <- data.frame(ToothGrowth)

#1. Provide basic summary
summary(filter(tg, supp=="OJ"))
summary(filter(tg, supp=="VC"))

#tg_supp <- group_by(tg,supp)
tg_oj <- filter(tg, supp=="OJ")
tg_vc <- filter(tg, supp=="VC")

#Comparison of two methods for equal doses:
tg_equal_dose_1 <- rbind(filter(tg_oj,dose==0.5),filter(tg_vc,dose==0.5))
tg_equal_dose_2 <- rbind(filter(tg_oj,dose==1.0),filter(tg_vc,dose==1.0))
tg_equal_dose_3 <- rbind(filter(tg_oj,dose==2.0),filter(tg_vc,dose==2.0))

ggplot(tg_oj, aes(len,fill=as.factor(dose))) + geom_density(alpha = 0.1) +ggtitle("Supp = OJ")
ggplot(tg_vc, aes(len,fill=as.factor(dose))) + geom_density(alpha = 0.1)+ggtitle("Supp = VC")

ggplot(tg_equal_dose_1, aes(len,fill=supp)) + geom_density(alpha = 0.1)+ggtitle("Supp = OJ") +ggtitle("Dose = 0.5")
ggplot(tg_equal_dose_2, aes(len,fill=supp)) + geom_density(alpha = 0.1)+ggtitle("Supp = OJ") +ggtitle("Dose = 1.0")
ggplot(tg_equal_dose_3, aes(len,fill=supp)) + geom_density(alpha = 0.1)+ggtitle("Supp = OJ") +ggtitle("Dose = 2.0")
#ggplot(variance_mr, aes(var, fill = n)) + geom_density(alpha = 0.1)

#2. Analyze cmonfidence level
conf_1 <- t.test(len ~ supp, paired=FALSE, var.equal = TRUE, data=tg_equal_dose_1)$conf
conf_2 <- t.test(len ~ supp, paired=FALSE, var.equal = TRUE, data=tg_equal_dose_2)$conf
conf_3 <- t.test(len ~ supp, paired=FALSE, var.equal = TRUE, data=tg_equal_dose_3)$conf

