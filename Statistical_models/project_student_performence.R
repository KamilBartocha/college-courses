do_anova <- function(result, variable, var_name, res_name="Final Grade"){
  print("==================================================")
  print("==================================================")
  print("==================================================")
  
  print("VARIABLE NAME:")
  print(var_name)
  
  print("SUMMARY:")
  print(summary(variable))
  
  print("BOX PLOTS:")
  plot(factor(variable), result, xlab=var_name, ylab=res_name)
  try({
    print("HISTOGRAM FOR")
    print(var_name)
    hist(variable, main=var_name)}, silent = TRUE)
  
  try({
    print("BARPLOT FOR")
    print(var_name)
    barplot(prop.table(table(variable)), main=var_name)}, silent = TRUE)
  
  
  print("Start ANOVA for:")
  print(res_name)
  print("explained by:")
  print(var_name)
  
  anova <- aov(result~variable, data.frame(result, variable))
  
  print("SUMMARY OF ANOVA")
  print(summary(anova))
  
  lr <-lm(result~variable, data.frame(result, variable))
  
  print("SUMMARY OF LINEAR MODEL")
  print(summary(lr))
  print("RESIDUAL PLOTS")
  plot(lr$residuals, main=var_name)
  print("Shapiro normality test")
  print(shapiro.test(lr$residuals))
}

do_anova_for_each_column <- function(result, no_zero_res,
                                     data, no_zero_data,
                                     res_name="Final Grade",
                                     no_zero_res_name="Final Grade with out 0"){
  for (name in colnames(data)){
    do_anova(result, factor(data[[name]]), name, res_name)
    print("Now FOR ANALISYS FOR RESULTS WITH OUT 0")
    do_anova(no_zero_res, factor(no_zero_data[[name]]), name, no_zero_res_name)
    sys.sleep(180)
  }
}



# Load data
data = read.table("student-mat.csv",sep=";",header=TRUE)
no_zero_data <- data[data$G3 != 0,]
fg <- data$G3
no_zero_fg <- no_zero_data$G3
print("SAMMARY OF FINAL GRADE")
summary(fg)
hist(fg, main = "HISTOGRAM FOR FINAL GRADE")
no_res = subset(data, select = -c(G1, G2, G3))
no_zero_res = subset(no_zero_data, select = -c(G1, G2, G3))
do_anova_for_each_column(fg, no_zero_fg, no_res, no_zero_res)








# TYLA CI WYSTARCZY :)