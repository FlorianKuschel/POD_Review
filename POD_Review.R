## fehlende p-Werte Geschlecht Male
POD_Hospital_sex_male$se <- sqrt(1/POD_Hospital_sex_male$tpos + 1/POD_Hospital_sex_male$tneg + 1/POD_Hospital_sex_male$cpos + 1/POD_Hospital_sex_male$cneg)

z_sex_kumar <- as.numeric(log(POD_Hospital_sex_male[4,9]) / POD_Hospital_sex_male$se[4])
p_sex_kumar <- 2 * (1 - pnorm(abs(z_sex_kumar)))
POD_Hospital_sex_male[4,12] <- p_sex_kumar

z_sex_moens <- as.numeric(log(POD_Hospital_sex_male[5,9]) / POD_Hospital_sex_male$se[5])
p_sex_moens <- 2 * (1 - pnorm(abs(z_sex_moens)))
POD_Hospital_sex_male[5,12] <- p_sex_moens

## Test Analyse Geschlecht Male
POD_Hospital_sex_male$vi <- conv.wald(out = OR, ci.lb = ci.lb, ci.ub = ci.ub, pval = pval, n = n_ges, data = POD_Hospital_sex_male, transf = log)$vi ## Berechnung der Varianz der Effektgröße einer Studie
POD_Hospital_sex_male$yi <- conv.wald(out = OR, ci.lb = ci.lb, ci.ub = ci.ub, pval = pval, n = n_ges, data = POD_Hospital_sex_male, transf = log)$yi ## berechnung der Effektgröße einer Studie
MA_POD_Hospital_sex_male <- rma(yi, vi, weights = 1/n_ges, data=POD_Hospital_sex_male)

study_labels_sex <- paste(POD_Hospital_sex_male$Autor, POD_Hospital_sex_male$Jahr, sep = " - ")
forest(MA_POD_Hospital_sex_male, slab = study_labels_sex, xlab = "Studie", mlab = "Effektgröße", cex=0.8, main = "Metaanalyse Geschlecht")

predict(MA_POD_Hospital_sex_male, transf=exp, digits=2) ##Ausgabe der OR und CI

funnel(MA_POD_Hospital_sex_male) 



  ## fehlende p-Werte Ethnie Black
POD_Hospital_ethnicity_black$se <- sqrt(1/POD_Hospital_ethnicity_black$tpos + 1/POD_Hospital_ethnicity_black$tneg + 1/POD_Hospital_ethnicity_black$cpos + 1/POD_Hospital_ethnicity_black$cneg)

z_ethnicityb_aamodt <- as.numeric(log(POD_Hospital_ethnicity_black[1,9]) / POD_Hospital_ethnicity_black$se[1])
p_ethnicityb_aamodt <- 2 * (1 - pnorm(abs(z_ethnicityb_aamodt)))
POD_Hospital_ethnicity_black[1,12] <- p_ethnicityb_aamodt

z_ethnicityb_kumar <- as.numeric(log(POD_Hospital_ethnicity_black[2,9]) / POD_Hospital_ethnicity_black$se[2])
p_ethnicityb_kumar <- 2 * (1 - pnorm(abs(z_ethnicityb_kumar)))
POD_Hospital_ethnicity_black[2,12] <- p_ethnicityb_kumar

## Test Analyse Ethnie Black
POD_Hospital_ethnicity_black$vi <- conv.wald(out = OR, ci.lb = ci.lb, ci.ub = ci.ub, pval = pval, n = n_ges, data = POD_Hospital_ethnicity_black, transf = log)$vi
POD_Hospital_ethnicity_black$yi <- conv.wald(out = OR, ci.lb = ci.lb, ci.ub = ci.ub, pval = pval, n = n_ges, data = POD_Hospital_ethnicity_black, transf = log)$yi
MA_POD_Hospital_ethnicity_black <- rma(yi, vi, weights = 1/n_ges, data=POD_Hospital_ethnicity_black)

study_labels_black <- paste(POD_Hospital_ethnicity_black$Autor, POD_Hospital_ethnicity_black$Jahr, sep = " - ")
forest(MA_POD_Hospital_ethnicity_black, slab = study_labels_black, xlab = "Studie", mlab = "Effektgröße", cex=0.8, main = "Metaanalyse Ethnie - Black")

predict(MA_POD_Hospital_ethnicity_black, transf=exp, digits=2) ##Ausgabe der OR und CI



  ## fehlende p-Werte Ethnie Asian
POD_Hospital_ethnicity_asian$se <- sqrt(1/POD_Hospital_ethnicity_asian$tpos + 1/POD_Hospital_ethnicity_asian$tneg + 1/POD_Hospital_ethnicity_asian$cpos + 1/POD_Hospital_ethnicity_asian$cneg)

z_ethnicitya_aamodt <- as.numeric(log(POD_Hospital_ethnicity_asian[1,9]) / POD_Hospital_ethnicity_asian$se[1])
p_ethnicitya_aamodt <- 2 * (1 - pnorm(abs(z_ethnicitya_aamodt)))
POD_Hospital_ethnicity_asian[1,12] <- p_ethnicitya_aamodt

z_ethnicitya_kumar <- as.numeric(log(POD_Hospital_ethnicity_asian[2,9]) / POD_Hospital_ethnicity_asian$se[2])
p_ethnicitya_kumar <- 2 * (1 - pnorm(abs(z_ethnicitya_kumar)))
POD_Hospital_ethnicity_asian[2,12] <- p_ethnicitya_kumar

## Test Analyse Ethnie Asian
POD_Hospital_ethnicity_asian$vi <- conv.wald(out = OR, ci.lb = ci.lb, ci.ub = ci.ub, pval = pval, n = n_ges, data = POD_Hospital_ethnicity_asian, transf = log)$vi
POD_Hospital_ethnicity_asian$yi <- conv.wald(out = OR, ci.lb = ci.lb, ci.ub = ci.ub, pval = pval, n = n_ges, data = POD_Hospital_ethnicity_asian, transf = log)$yi
MA_POD_Hospital_ethnicity_asian <- rma(yi, vi, weights = 1/n_ges, data = POD_Hospital_ethnicity_asian)

study_labels_asian <- paste(POD_Hospital_ethnicity_asian$Autor, POD_Hospital_ethnicity_asian$Jahr, sep = " - ")
forest(MA_POD_Hospital_ethnicity_asian, slab = study_labels_asian, xlab = "Studie", mlab = "Effektgröße", cex = 0.8, main = "Metaanalyse Ethnie Asian")

predict(MA_POD_Hospital_ethnicity_asian, transf=exp, digits=2) ##Ausgabe der OR und CI



  ## fehlende p-Werte Ethnie Hispanic
POD_Hospital_ethnicity_hispanic$se <- sqrt(1/POD_Hospital_ethnicity_hispanic$tpos + 1/POD_Hospital_ethnicity_hispanic$tneg + 1/POD_Hospital_ethnicity_hispanic$cpos + 1/POD_Hospital_ethnicity_hispanic$cneg)

z_ethnicityh_aamodt <- as.numeric(log(POD_Hospital_ethnicity_hispanic[1,9]) / POD_Hospital_ethnicity_hispanic$se[1])
p_ethnicityh_aamodt <- 2 * (1 - pnorm(abs(z_ethnicityh_aamodt)))
POD_Hospital_ethnicity_hispanic[1,12] <- p_ethnicityh_aamodt

z_ethnicityh_kumar <- as.numeric(log(POD_Hospital_ethnicity_hispanic[2,9]) / POD_Hospital_ethnicity_hispanic$se[2])
p_ethnicityh_kumar <- 2 * (1 - pnorm(abs(z_ethnicityh_kumar)))
POD_Hospital_ethnicity_hispanic[2,12] <- p_ethnicityh_kumar

## Test Analyse Ethnie Hispanic
POD_Hospital_ethnicity_hispanic$vi <- conv.wald(out = OR, ci.lb = ci.lb, ci.ub = ci.ub, pval = pval, n = n_ges, data = POD_Hospital_ethnicity_hispanic, transf = log)$vi
POD_Hospital_ethnicity_hispanic$yi <- conv.wald(out = OR, ci.lb = ci.lb, ci.ub = ci.ub, pval = pval, n = n_ges, data = POD_Hospital_ethnicity_hispanic, transf = log)$yi
MA_POD_Hospital_ethnicity_hispanic <- rma(yi, vi, weights = 1/n_ges, data = POD_Hospital_ethnicity_hispanic)

study_labels_hispanic <- paste(POD_Hospital_ethnicity_hispanic$Autor, POD_Hospital_ethnicity_hispanic$Jahr, sep = " - ")
forest(MA_POD_Hospital_ethnicity_hispanic, slab = study_labels_hispanic, xlab = "Studie", mlab = "Effektgröße", cex = 0.8, main = "Metaanalyse Ethnie Hispanic")

predict(MA_POD_Hospital_ethnicity_hispanic, transf=exp, digits=2) ##Ausgabe der OR und CI



  ## fehlende p-Wert Ethnie Native
POD_Hospital_ethnicity_native$se <- sqrt(1/POD_Hospital_ethnicity_native$tpos + 1/POD_Hospital_ethnicity_native$tneg + 1/POD_Hospital_ethnicity_native$cpos + 1/POD_Hospital_ethnicity_native$cneg)

z_ethnicityn_aamodt <- as.numeric(log(POD_Hospital_ethnicity_native[1,9]) / POD_Hospital_ethnicity_native$se[1])
p_ethnicityn_aamodt <- 2 * (1 - pnorm(abs(z_ethnicityn_aamodt)))
POD_Hospital_ethnicity_native[1,12] <- p_ethnicityn_aamodt

z_ethnicityn_kumar <- as.numeric(log(POD_Hospital_ethnicity_native[2,9]) / POD_Hospital_ethnicity_native$se[2])
p_ethnicityn_kumar <- 2 * (1 - pnorm(abs(z_ethnicityn_kumar)))
POD_Hospital_ethnicity_native[2,12] <- p_ethnicityn_kumar

## Test Analyse Ethnie Native
POD_Hospital_ethnicity_native$vi <- conv.wald(out = OR, ci.lb = ci.lb, ci.ub = ci.ub, pval = pval, n = n_ges, data = POD_Hospital_ethnicity_native, transf = log)$vi
POD_Hospital_ethnicity_native$yi <- conv.wald(out = OR, ci.lb = ci.lb, ci.ub = ci.ub, pval = pval, n = n_ges, data = POD_Hospital_ethnicity_native, transf = log)$yi
MA_POD_Hospital_ethnicity_native <- rma(yi, vi, weights = 1/n_ges, data = POD_Hospital_ethnicity_native)

study_labels_native <- paste(POD_Hospital_ethnicity_native$Autor, POD_Hospital_ethnicity_native$Jahr, sep = " - ")
forest(MA_POD_Hospital_ethnicity_native, slab = study_labels_native, xlab = "Studie", mlab = "Effektgröße", cex = 0.8, main = "Metaanalyse Ethnie Native")

predict(MA_POD_Hospital_ethnicity_native, transf=exp, digits=2) ##Ausgabe der OR und CI



  ## fehlende p-Werte Wohnort Urban
POD_Hospital_residency_urban$se <- sqrt(1/POD_Hospital_residency_urban$tpos + 1/POD_Hospital_residency_urban$tneg + 1/POD_Hospital_residency_urban$cpos + 1/POD_Hospital_residency_urban$cneg)

z_residency_kumar <- as.numeric(log(POD_Hospital_residency_urban[2,9]) / POD_Hospital_residency_urban$se[2])
p_residency_kumar <- 2 * (1 - pnorm(abs(z_residency_kumar)))
POD_Hospital_residency_urban[2,12] <- p_residency_kumar

## Test Analyse Wohnort Urban
POD_Hospital_residency_urban$vi <- conv.wald(out = OR, ci.lb = ci.lb, ci.ub = ci.ub, pval = pval, n = n_ges, data = POD_Hospital_residency_urban, transf = log)$vi
POD_Hospital_residency_urban$yi <- conv.wald(out = OR, ci.lb = ci.lb, ci.ub = ci.ub, pval = pval, n = n_ges, data = POD_Hospital_residency_urban, transf = log)$yi
MA_POD_Hospital_residency_urban <- rma(yi, vi, weights = 1/n_ges, data = POD_Hospital_residency_urban)

study_labels_residency_urban <- paste(POD_Hospital_residency_urban$Autor, POD_Hospital_residency_urban$Jahr, sep = " - ")
forest(MA_POD_Hospital_residency_urban, slab = study_labels_residency_urban, xlab = "Studie", mlab = "Effektgröße", cex = 0.8, main = "Metaanalyse Wohnort städtisch")

predict(MA_POD_Hospital_residency_urban, transf=exp, digits=2) ##Ausgabe der OR und CI



  ## fehlende p-Werte Familienstand Married
POD_Hospital_family_married$se <- sqrt(1/POD_Hospital_family_married$tpos + 1/POD_Hospital_family_married$tneg + 1/POD_Hospital_family_married$cpos + 1/POD_Hospital_family_married$cneg)

z_family_moens <- as.numeric(log(POD_Hospital_family_married[2,9]) / POD_Hospital_family_married$se[2])
p_family_moens <- 2 * (1 - pnorm(abs(z_family_moens)))
POD_Hospital_family_married[2,12] <- p_family_moens

## Test Analyse Familienstand Married
POD_Hospital_family_married$vi <- conv.wald(out = OR, ci.lb = ci.lb, ci.ub = ci.ub, pval = pval, n = n_ges, data = POD_Hospital_family_married, transf = log)$vi
POD_Hospital_family_married$yi <- conv.wald(out = OR, ci.lb = ci.lb, ci.ub = ci.ub, pval = pval, n = n_ges, data = POD_Hospital_family_married, transf = log)$yi
MA_POD_Hospital_family_married <- rma(yi, vi, weights = 1/n_ges, data = POD_Hospital_family_married)

study_labels_family_married <- paste(POD_Hospital_family_married$Autor, POD_Hospital_family_married$Jahr, sep = " - ")
forest(MA_POD_Hospital_family_married, slab = study_labels_family_married, xlab = "Studie", mlab = "Effektgröße", cex = 0.8, main = "Metaanalyse Familienstand verheiratet")

predict(MA_POD_Hospital_family_married, transf=exp, digits=2) ##Ausgabe der OR und CI



  ## fehlende p-Werte Bildung college/higher
POD_Hospital_education_college$se <- sqrt(1/POD_Hospital_education_college$tpos + 1/POD_Hospital_education_college$tneg + 1/POD_Hospital_education_college$cpos + 1/POD_Hospital_education_college$cneg)

z_education_moens <- as.numeric(log(POD_Hospital_education_college[2,9]) / POD_Hospital_education_college$se[2])
p_education_moens <- 2 * (1 - pnorm(abs(z_education_moens)))
POD_Hospital_education_college[2,12] <- p_education_moens

## Test Analyse Bildung college/higher
POD_Hospital_education_college$vi <- conv.wald(out = OR, ci.lb = ci.lb, ci.ub = ci.ub, pval = pval, n = n_ges, data = POD_Hospital_education_college, transf = log)$vi
POD_Hospital_education_college$yi <- conv.wald(out = OR, ci.lb = ci.lb, ci.ub = ci.ub, pval = pval, n = n_ges, data = POD_Hospital_education_college, transf = log)$yi
MA_POD_Hospital_education_college <- rma(yi, vi, weights = 1/n_ges, data = POD_Hospital_education_college)

study_labels_education_college <- paste(POD_Hospital_education_college$Autor, POD_Hospital_education_college$Jahr, sep = " - ")
forest(MA_POD_Hospital_education_college, slab = study_labels_education_college, xlab = "Studie", mlab = "Effektgröße", cex = 0.8, main = "Metaanalyse Bildung college")

predict(MA_POD_Hospital_education_college, transf=exp, digits=2) ##Ausgabe der OR und CI



  ## Test Analyse palliative homecare
POD_Hospital_palliative_homecare$vi <- conv.wald(out = OR, ci.lb = ci.lb, ci.ub = ci.ub, pval = pval, n = n_ges, data = POD_Hospital_palliative_homecare, transf = log)$vi
POD_Hospital_palliative_homecare$yi <- conv.wald(out = OR, ci.lb = ci.lb, ci.ub = ci.ub, pval = pval, n = n_ges, data = POD_Hospital_palliative_homecare, transf = log)$yi
MA_POD_Hospital_palliative_homecare <- rma(yi, vi, weights = 1/n_ges, data = POD_Hospital_palliative_homecare)

study_labels_palliative_homecare <- paste(POD_Hospital_palliative_homecare$Autor, POD_Hospital_palliative_homecare$Jahr, sep = " - ")
forest(MA_POD_Hospital_palliative_homecare, slab = study_labels_palliative_homecare, xlab = "Studie", mlab = "Effektgröße", cex = 0.8, main = "Metaanalyse häusliche Palliativepflege")

predict(MA_POD_Hospital_palliative_homecare, transf=exp, digits=2) 





