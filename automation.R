data<-read.csv(choose.files())
View(data)

a<-data$Sales_Channel
b<-data$Unit_Cost

n<-"Sales_Channel"
m<-"Unit_Cost"


c<-length(unique(a))
c
d<-length(unique(b))
d

if((is.character(a) | is.factor(a)) & (is.character(b) | is.factor(b))){
  chi<-chisq.test(a,b)
  chi_p<-chi$p.value
  chi
  chi_p
  
  if(chi_p<0.05){
    cat("there is significant difference between",n,"and",m)
  }else{
    cat("there is no significant difference between",n,"and",m)
  }
  print(chi)
  chi_p
}else if((is.numeric(a) | is.integer(a)) & (is.numeric(b) | is.integer(b))){
  cr= cor(a,b)
  cr
  
  if(cr>0){
    cat("There is positive correlation between",n,"and",m)
  }else{
    cat("There is nagative correlation between",n,"and",m)
  }
  print(cr)
}else if((is.character(a) | is.factor(a)) & (is.numeric(b) | is.integer(b))){
   if(c>2){
     anova_var <- aov(b~a,data)
     summary(anova_var)
     
     anova_p<-anova(anova_var)$'Pr(>F)'[1]
     anova_p
     
     if(anova_p<0.05){
       cat("there is significant difference between",n,"and",m)
     }else{
       cat("there is no significant difference between",n,"and",m)
     }
     print(anova_p)
     summary(anova_var)
     
   }else{
     ttest_var<-t.test(b~a)
     ttest_var
     
     ttest_pvalue<-ttest_var$p.value
     ttest_pvalue
     
     if(ttest_pvalue<0.05){
       cat("there is significant difference between data",n,"and",m)
     }else{
       cat("there is no significant difference between data",n,"and",m)
     }
     print(ttest_var)
     print(ttest_pvalue)
   }
}else if((is.numeric(a) | is.integer(a)) & (is.character(b) | is.factor(b))){
   if(d>2){
     anova_var <- aov(a~b,data)
     summary(anova_var)
     
     anova_p<-anova(anova_var)$'Pr(>F)'[1]
     anova_p
     
     if(anova_p<0.05){
       cat("there is significant difference between data",n,"and",m)
     }else{
       cat("there is no significant difference between data",n,"and",m)
     }
     print(anova_p)
     summary(anova_var)
   }else{
     ttest_var<-t.test(a ~ b)
     ttest_var
     
     ttest_pvalue<-ttest_var$p.value
     ttest_pvalue
     
     if(ttest_pvalue<0.05){
       cat("there is significant difference between data",n,"and",m)
     }else{
       cat("there is no significant difference between data",n,"and",m)
     }
     print(ttest_var)
     print(ttest_pvalue)
   }
 }

