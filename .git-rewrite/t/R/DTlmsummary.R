DTsummary.biglm <- function(big.object){

  n <- big.object$n
  k <- big.object$n - big.object$df.resid
  beta <- coef(big.object)
  VCV <- vcov(big.object)
  se <- sqrt(diag(VCV))
  tstat <- beta/se

  # BIGLM ASSUME NORMAL DISTRIBUTION: WE FAVOR NON-ASYMPTOTIC STUDENT DISTRIBUTION
  p_value <- 2 * pt(abs(beta/se), df = n - k, lower.tail = FALSE)

  # COMPUTE EVERYTHING IN A DATAFRAME
  resultlm <- tibble::tibble(coeff = names(beta),
                     estimate = beta,
                     se, tstat, p_value)
  resultlm <- resultlm[!is.na(resultlm$estimate),]
  significance <- 3 - as.numeric(resultlm$p_value > 0.001) -
    as.numeric(resultlm$p_value > 0.05) - as.numeric(resultlm$p_value > 0.01)
  resultlm$significance <- sapply(1:length(significance), function(i) paste(rep("*",
                                                                                times = significance[[i]]), collapse = ""))

  # GLOBAL STATISTICS
  adjr2 <- 1 - (1-summary(big.object)$rsq)*(n-1)/(big.object$df.resid-1)
  Fstat <- (n-k)/(k-1)*summary(big.object)$rsq/(1-summary(big.object)$rsq)

  # DATA-TABLE
  dt1 <- DT::datatable(resultlm, colnames = c("Coefficient",
                                              "Estimate", "Std. Error", "Student t-stat", "Pr(>|t|)",
                                              ""), rownames = FALSE, class = "hover", options = list(pageLength = 20)) %>%
    formatRound(2, digits = 4) %>% formatRound(3, digits = 2) %>%
    formatRound(4, digits = 1) %>% formatRound(5, digits = 3)
  sumstat <- tibble::tibble(stat = c("Nombre Observations", "Nombre Paramètres",
                             "R2", "R2 ajusté", "F-stat", "Degré Liberté"),
                    value = c(n, k, summary(big.object)$rsq, adjr2,
                              Fstat, big.object$df.resid))
  dt2 <- DT::datatable(sumstat, rownames = F,
                       colnames = c("Statistique", "Valeur"), class = "hover", options = list(pageLength = 20)) %>%
    formatRound(2, digits = 3)
  return(list(coefftab = dt1, modeltab = dt2))

}
