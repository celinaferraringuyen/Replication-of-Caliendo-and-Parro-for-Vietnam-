rm(list = ls())
library(tidyverse)
library(readxl)

ADB_PATH <- '/Users/celinaferrari/Downloads/ADB-MRIO-2018_September 2024.xlsx'
MFN_DIR  <- '/Users/celinaferrari/Downloads/2017 countries '
PREF_DIR <- '/Users/celinaferrari/Downloads/2022 coutries '
dir.create("data/processed", recursive=TRUE, showWarnings=FALSE)

COUNTRIES <- c("VNM","AUS","BRN","CAN","JPN","MYS","MEX","NZL","SGP","ROW")
SECTORS   <- c("AGR","MNG","FOD","TEX","WOD","PPR","CHM","RBP","NMM",
               "MET","MAC","ELE","TRQ","MFG","EGW","CON","SRV")
N <- length(COUNTRIES); J <- length(SECTORS); VNM_IDX <- 1

adb_to_iso <- c("VIE"="VNM","AUS"="AUS","BRU"="BRN","CAN"="CAN",
                "JPN"="JPN","MAL"="MYS","MEX"="MEX","NZL"="NZL","SIN"="SGP")

adb_sector_map <- c(
  "Agriculture, hunting, forestry, and fishing"                                          = "AGR",
  "Mining and quarrying"                                                                 = "MNG",
  "Food, beverages, and tobacco"                                                         = "FOD",
  "Textiles and textile products"                                                        = "TEX",
  "Leather, leather products, and footwear"                                              = "TEX",
  "Wood and products of wood and cork"                                                   = "WOD",
  "Pulp, paper, paper products, printing, and publishing"                                = "PPR",
  "Coke, refined petroleum, and nuclear fuel"                                            = "CHM",
  "Chemicals and chemical products"                                                      = "CHM",
  "Rubber and plastics"                                                                  = "RBP",
  "Other nonmetallic minerals"                                                           = "NMM",
  "Basic metals and fabricated metal"                                                    = "MET",
  "Machinery, nec"                                                                       = "MAC",
  "Electrical and optical equipment"                                                     = "ELE",
  "Transport equipment"                                                                  = "TRQ",
  "Manufacturing, nec; recycling"                                                        = "MFG",
  "Electricity, gas, and water supply"                                                   = "EGW",
  "Construction"                                                                         = "CON",
  "Sale, maintenance, and repair of motor vehicles and motorcycles; retail sale of fuel" = "SRV",
  "Wholesale trade and commission trade, except of motor vehicles and motorcycles"       = "SRV",
  "Retail trade, except of motor vehicles and motorcycles; repair of household goods"    = "SRV",
  "Hotels and restaurants"                                                               = "SRV",
  "Inland transport"                                                                     = "SRV",
  "Water transport"                                                                      = "SRV",
  "Air transport"                                                                        = "SRV",
  "Other supporting and auxiliary transport activities; activities of travel agencies"   = "SRV",
  "Post and telecommunications"                                                          = "SRV",
  "Financial intermediation"                                                             = "SRV",
  "Real estate activities"                                                               = "SRV",
  "Renting of M&Eq and other business activities"                                        = "SRV",
  "Public admin and defence; compulsory social security"                                 = "SRV",
  "Public administration and defense; compulsory social security"                        = "SRV",
  "Education"                                                                            = "SRV",
  "Health and social work"                                                               = "SRV",
  "Other community, social, and personal services"                                       = "SRV",
  "Private households with employed persons"                                             = "SRV"
)

# ================================================================
# BLOCK 1: PARSE ADB MRIO
# ================================================================
cat("[1] Reading ADB MRIO...\n")
raw <- read_excel(ADB_PATH, sheet="ADB MRIO 2018", col_names=FALSE)

sec_names_hdr  <- unlist(raw[5,])
ctry_codes_hdr <- unlist(raw[6,])
col_codes_hdr  <- unlist(raw[7,])
row_sec_labels  <- unlist(raw[8:nrow(raw), 2])
row_ctry_labels <- unlist(raw[8:nrow(raw), 3])
row_col_codes   <- unlist(raw[8:nrow(raw), 4])

data_mat <- matrix(
  suppressWarnings(as.numeric(unlist(raw[8:nrow(raw), 5:ncol(raw)]))),
  nrow=nrow(raw)-7, ncol=ncol(raw)-4)
data_mat[is.na(data_mat)] <- 0

data_ctry_hdr <- ctry_codes_hdr[5:length(ctry_codes_hdr)]
data_sec_hdr  <- sec_names_hdr[5:length(sec_names_hdr)]
data_col_hdr  <- col_codes_hdr[5:length(col_codes_hdr)]

is_interm_col <- grepl("^c[0-9]+$", data_col_hdr) & !is.na(data_col_hdr)
is_fd_col     <- !is_interm_col & data_ctry_hdr %in% names(adb_to_iso) & !is.na(data_ctry_hdr)
is_interm_row <- grepl("^c[0-9]+$", row_col_codes) & !is.na(row_ctry_labels)

col_lookup <- data.frame(pos=which(is_interm_col),
                         adb_ctry=data_ctry_hdr[is_interm_col],
                         sec_name=data_sec_hdr[is_interm_col],
                         stringsAsFactors=FALSE) %>%
  mutate(iso=adb_to_iso[adb_ctry],
         iso=ifelse(is.na(iso), "ROW", iso),
         cp_sec=adb_sector_map[sec_name],
         n_idx=match(iso,COUNTRIES), j_idx=match(cp_sec,SECTORS)) %>%
  filter(!is.na(n_idx), !is.na(j_idx))

row_lookup <- data.frame(pos=which(is_interm_row),
                         adb_ctry=row_ctry_labels[is_interm_row],
                         sec_name=row_sec_labels[is_interm_row],
                         stringsAsFactors=FALSE) %>%
  mutate(iso=adb_to_iso[adb_ctry],
         iso=ifelse(is.na(iso), "ROW", iso),   # ← non-CPTPP countries → ROW
         cp_sec=adb_sector_map[sec_name],
         n_idx=match(iso,COUNTRIES), j_idx=match(cp_sec,SECTORS)) %>%
  filter(!is.na(n_idx), !is.na(j_idx))

col_by_nj <- split(col_lookup, list(col_lookup$n_idx, col_lookup$j_idx))
row_by_nj <- split(row_lookup, list(row_lookup$n_idx, row_lookup$j_idx))

NJ <- N*J
Z  <- matrix(0, nrow=NJ, ncol=NJ)
cat("    Building Z...\n")
for (n_dest in seq_len(N)) {
  if (n_dest%%3==1) cat(sprintf("      %s...\n", COUNTRIES[n_dest]))
  for (j_dest in seq_len(J)) {
    col_j  <- (n_dest-1)*J+j_dest
    cols_d <- col_by_nj[[paste(n_dest,j_dest,sep=".")]]
    if (is.null(cols_d)||nrow(cols_d)==0) next
    for (n_src in seq_len(N)) for (k_src in seq_len(J)) {
      rows_s <- row_by_nj[[paste(n_src,k_src,sep=".")]]
      if (is.null(rows_s)||nrow(rows_s)==0) next
      Z[(n_src-1)*J+k_src, col_j] <- Z[(n_src-1)*J+k_src, col_j] +
        sum(data_mat[rows_s$pos, cols_d$pos], na.rm=TRUE)
    }
  }
}

# VA — single total row at raw row 2568
va_data_row <- 2568 - 7
VA <- numeric(NJ)
for (i in seq_len(nrow(col_lookup))) {
  col_j <- (col_lookup$n_idx[i]-1)*J + col_lookup$j_idx[i]
  VA[col_j] <- VA[col_j] + data_mat[va_data_row, col_lookup$pos[i]]
}

# FD matrix
fd_lookup <- data.frame(pos=which(is_fd_col),
                        adb_ctry=data_ctry_hdr[is_fd_col],
                        stringsAsFactors=FALSE) %>%
  mutate(n_idx=match(adb_to_iso[adb_ctry],COUNTRIES)) %>%
  filter(!is.na(n_idx))

FD <- matrix(0, nrow=NJ, ncol=N)
for (n_dest in seq_len(N-1)) {
  fd_cols_n <- fd_lookup$pos[fd_lookup$n_idx==n_dest]
  if (length(fd_cols_n)==0) next
  for (n_src in seq_len(N)) for (k in seq_len(J)) {   # ← N not N-1
    rows_s <- row_by_nj[[paste(n_src,k,sep=".")]]
    if (is.null(rows_s)||nrow(rows_s)==0) next
    FD[(n_src-1)*J+k, n_dest] <- FD[(n_src-1)*J+k, n_dest] +
      sum(data_mat[rows_s$pos, fd_cols_n], na.rm=TRUE)
  }
}

vnm_cols <- ((VNM_IDX-1)*J+1):(VNM_IDX*J)
aus_rows <- ((which(COUNTRIES=="AUS")-1)*J+1):(which(COUNTRIES=="AUS")*J)
cat(sprintf("    Z:%.0f bn | VA:%.0f bn | VNM VA:%.0f mn | AUS->VNM:%.0f mn\n",
            sum(Z)/1e6, sum(VA)/1e6, sum(VA[vnm_cols]), sum(Z[aus_rows,vnm_cols])))

# ================================================================
# BLOCK 2: IO SHARES
# ================================================================
cat("[2] IO shares...\n")
gross_output <- colSums(Z) + VA
gross_output[gross_output<=0] <- NA
alpha <- matrix(VA/gross_output, nrow=N, ncol=J, byrow=FALSE)
alpha[is.na(alpha)] <- 0
alpha <- pmin(pmax(alpha,0),1)

gamma <- array(0, dim=c(N,J,J))
for (n in seq_len(N)) for (j in seq_len(J)) {
  col_j <- (n-1)*J+j
  ct <- sum(Z[,col_j])
  if (!is.na(ct)&&ct>0)
    for (k in seq_len(J))
      gamma[n,k,j] <- sum(Z[(seq_len(N)-1)*J+k, col_j]) / ct
}
for (n in seq_len(N)) for (j in seq_len(J)) {
  s <- alpha[n,j]+sum(gamma[n,,j])
  if (is.finite(s)&&s>0) { gamma[n,,j]<-gamma[n,,j]/s; alpha[n,j]<-alpha[n,j]/s }
}
cat(sprintf("    alpha mean=%.3f\n", mean(alpha)))

# ================================================================
# BLOCK 3: TRADE FLOWS
# ================================================================
cat("[3] Trade flows...\n")
trade <- array(0, dim=c(N,N,J))

# All sources (including ROW) → CPTPP destinations
"for (n_src in seq_len(N)) for (k in seq_len(J)) {
  row_k <- (n_src-1)*J + k
  for (n_dest in seq_len(N-1))
    trade[n_dest, n_src, k] <- sum(Z[row_k, ((n_dest-1)*J+1):(n_dest*J)]) +
      FD[row_k, n_dest]
}"

for (n_src in seq_len(N)) for (k in seq_len(J)) {
  row_k <- (n_src-1)*J + k
  for (n_dest in seq_len(N)) {
    z_spend <- sum(Z[row_k, ((n_dest-1)*J+1):(n_dest*J)])
    fd_spend <- if (n_dest < N) FD[row_k, n_dest] else 0
    trade[n_dest, n_src, k] <- z_spend + fd_spend
  }
}

# ROW domestic trade (ROW buying from ROW)
for (k in seq_len(J)) {
  row_exports_k <- sum(trade[1:(N-1), N, k])
  trade[N, N, k] <- row_exports_k * (0.85 / 0.15)
}

pi_mat <- array(0, dim=c(N,N,J))
for (n in seq_len(N)) for (j in seq_len(J)) {
  tot <- sum(trade[n,,j])
  if (tot>0) pi_mat[n,,j] <- trade[n,,j]/tot
}

expenditure <- matrix(0, nrow=N, ncol=J)
for (n in seq_len(N)) for (j in seq_len(J))
  expenditure[n,j] <- sum(trade[n,,j])

# CORRECT income calculation
income <- numeric(N)
for (n in seq_len(N)) income[n] <- sum(VA[((n-1)*J+1):(n*J)])
income[N] <- sum(trade[N, N, ]) + sum(trade[1:(N-1), N, ])

cat(sprintf("    VNM imports:%.1f bn | income:%.1f bn\n",
            sum(trade[VNM_IDX,-VNM_IDX,])/1e3, income[VNM_IDX]/1e3))

# ================================================================
# BLOCK 4: TARIFFS
# ================================================================
cat("[4] Loading tariffs...\n")

hs_to_cp <- function(ch) case_when(
  ch%in%1:15~"AGR", ch%in%25:27~"MNG", ch%in%16:24~"FOD",
  ch%in%c(41:43,50:67)~"TEX", ch%in%44:46~"WOD", ch%in%47:49~"PPR",
  ch%in%28:38~"CHM", ch%in%39:40~"RBP", ch%in%68:70~"NMM",
  ch%in%72:83~"MET", ch==84~"MAC", ch==85~"ELE",
  ch%in%86:89~"TRQ", ch%in%c(71,90:97)~"MFG", TRUE~"SRV")

get_iso <- function(fname) {
  str_replace_all(str_extract(fname,"_(VNM|AUS|BRN|CAN|CHL|JPN|MYS|MEX|NZL|PER|SGP)_"),"_","")
}

load_wits <- function(path, iso, col_name) {
  df <- read_csv(path, show_col_types=FALSE)
  if (!"SimpleAverage"%in%names(df)) return(NULL)
  df %>% select(ProductCode,SimpleAverage) %>%
    mutate(reporter=iso,
           chapter=as.integer(substr(sprintf("%06d",as.integer(ProductCode)),1,2)),
           cp_sec=hs_to_cp(chapter), rate=as.numeric(SimpleAverage)/100) %>%
    filter(!is.na(rate)) %>%
    group_by(reporter,cp_sec) %>%
    summarise(avg_rate=mean(rate,na.rm=TRUE),.groups="drop") %>%
    rename(!!col_name:=avg_rate)
}

mfn_files  <- list.files(MFN_DIR,  pattern="\\.CSV$|\\.csv$", recursive=TRUE, full.names=TRUE)
pref_files <- list.files(PREF_DIR, pattern="\\.CSV$|\\.csv$", recursive=TRUE, full.names=TRUE)
mfn_files_2017 <- mfn_files[grepl("2017",mfn_files)]
cat(sprintf("    MFN 2017:%d | CPTPP:%d\n", length(mfn_files_2017), length(pref_files)))

mfn_list <- list()
for (f in mfn_files_2017) {
  iso <- get_iso(basename(f))
  if (is.na(iso)||!is.null(mfn_list[[iso]])) next
  df <- tryCatch(load_wits(f,iso,"avg_mfn"),error=function(e)NULL)
  if (!is.null(df)) { mfn_list[[iso]]<-df; cat(sprintf("  MFN:%s\n",iso)) }
}
mfn_all <- bind_rows(mfn_list)

pref_list <- list()
for (f in pref_files) {
  iso <- get_iso(basename(f))
  if (is.na(iso)||!is.null(pref_list[[iso]])) next
  df <- tryCatch(load_wits(f,iso,"avg_pref"),error=function(e)NULL)
  if (!is.null(df)) { pref_list[[iso]]<-df; cat(sprintf("  CPTPP:%s\n",iso)) }
}
pref_all <- if(length(pref_list)>0) bind_rows(pref_list) else
  tibble(reporter=character(),cp_sec=character(),avg_pref=numeric())

tau_pre <- array(1,dim=c(N,N,J)); tau_post <- array(1,dim=c(N,N,J))
cptpp <- c("VNM","AUS","BRN","CAN","CHL","JPN","MYS","MEX","NZL","PER","SGP")
for (n in seq_len(N)) {
  iso_n <- COUNTRIES[n]
  mn <- mfn_all  %>% filter(reporter==iso_n)
  pn <- pref_all %>% filter(reporter==iso_n)
  for (j in seq_len(J)) {
    sec <- SECTORS[j]
    mr <- mn$avg_mfn[mn$cp_sec==sec];  mr <- if(length(mr)>0) mr[1] else 0
    pr <- pn$avg_pref[pn$cp_sec==sec]; pr <- if(length(pr)>0) pr[1] else 0
    for (i in seq_len(N)) {
      if (i==n) next
      tau_pre[n,i,j]  <- 1+mr
      tau_post[n,i,j] <- if(COUNTRIES[i]%in%cptpp&&iso_n%in%cptpp) 1+pr else 1+mr
    }
  }
}
tau_hat <- tau_post/tau_pre
tau_hat[!is.finite(tau_hat)] <- 1
for (n in seq_len(N)) tau_hat[n,n,] <- 1
cat(sprintf("    VNM MFN:%.2f%% | CPTPP:%.2f%% | tau_hat:%.4f\n",
            mean(tau_pre[VNM_IDX,-VNM_IDX,]-1)*100,
            mean(tau_post[VNM_IDX,-VNM_IDX,]-1)*100,
            mean(tau_hat[VNM_IDX,-VNM_IDX,])))

# ================================================================
# BLOCK 5: SOLVER
# ================================================================
# ── Block 5 setup ──
theta <- c(AGR=8.11,MNG=12.76,FOD=3.15,TEX=6.09,WOD=8.24,PPR=10.56,
           CHM=6.77,RBP=3.67,NMM=6.63,MET=6.67,MAC=5.15,ELE=5.74,
           TRQ=7.67,MFG=6.52,EGW=1.00,CON=1.00,SRV=1.00)[SECTORS]

expend_share <- matrix(0, N, J)
for (n in seq_len(N)) {
  tot <- sum(expenditure[n,])
  if (tot > 0) expend_share[n,] <- expenditure[n,] / tot
}

VA_base <- numeric(N)
for (n in seq_len(N)) VA_base[n] <- sum(VA[((n-1)*J+1):(n*J)])
VA_base[N] <- income[N]

solve_cp <- function(pi_mat, alpha, es, gamma, theta, tau_hat, tau_post,
                     X_base, VA_base, N, J, VNM_IDX,
                     tol=1e-7, max_iter=2000, damp_w=0.5) {
  
  w_hat <- rep(1, N)
  P_hat <- matrix(1, N, J)   # ← persist prices across outer iterations
  
  # Compute income scale factor at baseline to ensure identity holds
  inc_scale <- numeric(N)
  for (n in seq_len(N)) {
    lab0 <- sum(sapply(seq_len(J), function(j)
      alpha[n,j] * sum(pi_mat[,n,j] * X_base[,j])))
    inc_scale[n] <- if (lab0 > 0 && VA_base[n] > 0) VA_base[n] / lab0 else 1
  }
  cat("Income scale factors:", round(inc_scale, 3), "\n")
  
  for (iter in seq_len(max_iter)) {
    w_hat_old <- w_hat
    
    # ── Prices: iterate to convergence, carrying forward P_hat ──
    c_hat <- matrix(1, N, J)
    for (price_iter in seq_len(300)) {
      c_hat_old <- c_hat
      for (n in seq_len(N)) for (j in seq_len(J))
        c_hat[n,j] <- w_hat[n]^alpha[n,j] *
          prod(pmax(P_hat[n,], 1e-300)^gamma[n,,j])
      P_hat_new <- P_hat
      for (n in seq_len(N)) for (j in seq_len(J)) {
        cv  <- c_hat[,j] * tau_hat[n,,j]
        ac  <- pi_mat[n,,j] > 1e-10 & is.finite(cv) & cv > 0
        if (!any(ac)) next
        num <- ifelse(ac, pi_mat[n,,j] * cv^(-theta[j]), 0)
        den <- sum(num)
        if (is.finite(den) && den > 0) P_hat_new[n,j] <- den^(-1/theta[j])
      }
      P_hat <- P_hat_new
      if (max(abs(c_hat - c_hat_old)) < 1e-12) break
    }
    
    # ── Trade shares ──
    pi_new <- array(0, c(N, N, J))
    for (n in seq_len(N)) for (j in seq_len(J)) {
      cv  <- c_hat[,j] * tau_hat[n,,j]
      ac  <- pi_mat[n,,j] > 1e-10 & is.finite(cv) & cv > 0
      num <- ifelse(ac, pi_mat[n,,j] * cv^(-theta[j]), 0)
      den <- sum(num)
      if (is.finite(den) && den > 0) pi_new[n,,j] <- num / den
    }
    
    # ── Income with scale correction ──
    inc_new <- numeric(N)
    for (n in seq_len(N)) {
      lab <- sum(sapply(seq_len(J), function(j)
        alpha[n,j] * sum(pi_new[,n,j] * X_base[,j])))
      tar <- sum(sapply(seq_len(J), function(j)
        sum(sapply(seq_len(N), function(i) {
          if (i == n || tau_post[n,i,j] <= 1) return(0)
          (1 - 1/tau_post[n,i,j]) * pi_new[n,i,j] * X_base[n,j]
        }))))
      inc_new[n] <- (lab + tar) * inc_scale[n]
    }
    
    # ── Wage update ──
    w_raw <- inc_new / pmax(VA_base, 1e-10)
    w_raw[!is.finite(w_raw) | w_raw <= 0] <- 1
    w_raw <- w_raw / w_raw[VNM_IDX]
    w_hat <- damp_w * w_hat + (1 - damp_w) * w_raw
    w_hat <- pmax(pmin(w_hat, 10), 0.1)
    
    err <- max(abs(w_hat - w_hat_old))
    if (!is.finite(err)) { cat("Non-finite at iter", iter, "\n"); break }
    if (iter %% 100 == 0)
      cat(sprintf("  Iter%4d | err=%.2e | w_JPN=%.4f w_AUS=%.4f w_ROW=%.4f\n",
                  iter, err, w_hat[5], w_hat[2], w_hat[N]))
    if (err < tol) { cat(sprintf("  Converged at iter %d\n", iter)); break }
    if (iter == max_iter) warning("Did not converge!")
  }
  
  wf <- numeric(N)
  for (n in seq_len(N))
    wf[n] <- w_hat[n] / prod(pmax(P_hat[n,], 1e-300)^es[n,])
  
  list(w_hat=w_hat, welfare_hat=wf, P_hat=P_hat, pi_new=pi_new)
}

results <- solve_cp(pi_mat, alpha, expend_share, gamma, theta, tau_hat, tau_post,
                    expenditure, VA_base, N, J, VNM_IDX)

results <- solve_cp(pi_mat, alpha, expend_share, gamma, theta, tau_hat, tau_post,
                    expenditure, VA_base, N, J, VNM_IDX)
# ================================================================
# BLOCK 6: RESULTS
# ================================================================
cat("\n══ CPTPP Welfare Effects ══\n")
out <- tibble(Country=COUNTRIES,
              `Wage Δ(%)`   =round((results$w_hat      -1)*100,3),
              `Welfare Δ(%)`=round((results$welfare_hat-1)*100,3)) %>%
  arrange(desc(`Welfare Δ(%)`))
print(out)
write_csv(out,"data/processed/cptpp_welfare_results.csv")

cat("\n── Vietnam sectoral prices ──\n")
print(tibble(Sector=SECTORS,
             `Price Δ(%)`=round((results$P_hat[VNM_IDX,]-1)*100,3)) %>%
        arrange(`Price Δ(%)`))
cat("\nDone.\n")



