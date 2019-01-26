# How to compute RRS CI via Kalinowski and Taper 2005 
# Starting with their example in the text to figure out the mechanics

ml <- (15 * log((0.5 * (15/10)) / 1.25)) + (10 * log((0.5 * (10/10)) / 1.25))
chi5 <- qchisq(p = 0.95, df = 1)

ml5 <- (15 * log((0.5 * (.68)) / .84)) + (10 * log((0.5 * (10/10)) / .84))  # this is Wa

ml - chi5 / 2  # this equals the log likelihood of Wa

t(sapply(seq(0.5, 1, by = 0.01), function(i) {
  avg_rrs = mean(c(i, 1))
  data.frame("alpha" = i, "logl" = (15 * log((0.5 * (i)) / avg_rrs)) + (10 * log((0.5 * (10/10)) / avg_rrs)) - (ml - chi5 / 2))
}))

ml95 <- (15 * log((0.5 * (3.45)) / 2.225)) + (10 * log((0.5 * (10/10)) / 2.225))  # this is Wa

t(sapply(seq(1, 5, by = 0.05), function(i) {
  avg_rrs = mean(c(i, 1))
  data.frame("alpha" = i, "logl" = (15 * log((0.5 * (i)) / avg_rrs)) + (10 * log((0.5 * (10/10)) / avg_rrs)) - (ml - chi5 / 2))
}))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~
n_h_off <- 15
n_w_off <- 10
n_h_par <- 20
n_w_par <- 20
# p_h_par <- 0.5
# p_w_par <- 0.5
alpha <- 0.05
#~~~~~~~~~~~~~~~~~~
rrs_ci_kalinowski <- function(n_h_off, n_w_off, n_h_par, n_w_par, alpha){
  chi_alpha <- qchisq(p = (1 - alpha), df = 1)
  n_off <- sum(c(n_h_off, n_w_off))
  n_par <- sum(c(n_h_par, n_w_par))
  
  rs_h <- n_h_off / n_h_par
  rs_w <- n_w_off / n_w_par
  
  p_h_par <- n_h_par / n_par
  p_w_par <- n_w_par / n_par
  
  rrs_h <- rs_h / rs_w
  rrs_w <- rs_w / rs_w
  rrs_avg <- (rrs_h * p_h_par) + (rrs_w * p_w_par)
  
  rrs_ml <- (n_h_off * log(p_h_par * rrs_h / rrs_avg)) + (n_w_off * log(p_w_par * rrs_w / rrs_avg))
  
  xi_dist <- bind_rows(
    lapply(seq(0.01, 5, by = 0.01), function(rrs_h_xi) {
      rrs_avg_xi <- (rrs_h_xi * p_h_par) + (rrs_w * p_w_par)
      tibble(rrs_crit = rrs_h_xi,
             logl = (n_h_off * log(p_h_par * rrs_h_xi / rrs_avg_xi)) + (n_w_off * log(p_w_par * rrs_w / rrs_avg_xi)) - (rrs_ml - chi_alpha / 2)
      )
    } )
  )
  
  rrs_min <- xi_dist %>% 
    mutate(abs_logl = abs(logl)) %>% 
    filter(rrs_crit < rrs_h) %>% 
    top_n(-1, abs_logl) %>% 
    pull(rrs_crit)
  
  rrs_max <- xi_dist %>% 
    mutate(abs_logl = abs(logl)) %>% 
    filter(rrs_crit > rrs_h) %>% 
    top_n(-1, abs_logl) %>% 
    pull(rrs_crit)
  
  xi_plot <- xi_dist %>% 
    ggplot(aes(x = rrs_crit, y = logl)) +
    geom_line() +
    geom_hline(yintercept = 0, colour = "red", lwd = 2) +
    geom_vline(xintercept = c(rrs_h, rrs_min, rrs_max), colour = "blue") +
    ylim(c(-5, 5)) +
    xlim(c(0, 2)) +
    annotate("text", x = rrs_h + 0.1, y = xi_dist %>% filter(rrs_crit == xi_dist$rrs_crit[which.min(abs(xi_dist$rrs_crit  - rrs_h))]) %>% pull(logl) + 0.4, label = round(rrs_h, 2)) +
    annotate("text", x = rrs_min - 0.1, y = xi_dist %>% filter(rrs_crit == rrs_min) %>% pull(logl) + 0.4, label = rrs_min) +
    annotate("text", x = rrs_max + 0.1, y = xi_dist %>% filter(rrs_crit == rrs_max) %>% pull(logl) + 0.4, label = rrs_max)
  
  print(xi_plot)
  return(c(rrs_min, rrs_h, rrs_max))
}

# Test with example in Kalinowski and Taper 2005
rrs_ci_kalinowski(n_h_off = 15, n_w_off = 10, n_h_par = 20, n_w_par = 20, alpha = 0.05)
# This checked out as did other tests with examples in Table 1

# Try to double the number of offspring sampled to verify that CI's tighten
rrs_ci_kalinowski(n_h_off = 18, n_w_off = 32, n_h_par = 20, n_w_par = 20, alpha = 0.05)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pull in Hogan Even data
dat <- read_csv("paired_14_16_filter_parents.csv")

# Get parent sample sizes
dat %>% count(SEX, origin) %>% spread(origin, nn)

# Get offspring sample sizes
dat %>% group_by(SEX, origin) %>% summarise(n_off = sum(n)) %>% spread(origin, n_off)

# Females
rrs_ci_kalinowski(n_h_off = 125, n_w_off = 102, n_h_par = 271, n_w_par = 105, alpha = 0.05)

# Males
rrs_ci_kalinowski(n_h_off = 140, n_w_off = 106, n_h_par = 166, n_w_par = 109, alpha = 0.05)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pull in Hogan Odd data
dat <- read_csv("paired_13_15_filter_parents.csv")

# Get parent sample sizes
dat %>% count(SEX, origin) %>% spread(origin, nn)

# Get offspring sample sizes
dat %>% group_by(SEX, origin) %>% summarise(n_off = sum(n)) %>% spread(origin, n_off)

# Females
rrs_ci_kalinowski(n_h_off = 1, n_w_off = 25, n_h_par = 281, n_w_par = 131, alpha = 0.05)

# Males
rrs_ci_kalinowski(n_h_off = 2, n_w_off = 20, n_h_par = 162, n_w_par = 191, alpha = 0.05)
