###########################
### Feature Engineering ###
###########################
library('data.table')
library('plyr')

load('clean.Rdata')

feat   = data.table(full_d[,c('customer_no', 'c', 'Bad_label')], key = 'customer_no') # sort data according to customer_no
x_a    = data.table(full_a[,c('customer_no', 'c')], key = 'customer_no')
x_e    = data.table(full_d[,c('customer_no', 'c')], key = 'customer_no')
full_e = data.table(full_e, key = 'customer_no')
x_d    = data.table(full_d[,c('customer_no', 'feature_3', 'feature_7',
                              'feature_52')], key = 'customer_no')

# Process payment history features for to numeric
pay_text = function(x){
  if (is.na(x)){
    return('XXX')
  }else{
    text = substring(x, seq(1,nchar(x),3), seq(3,nchar(x),3)) # Divide record in paymenthistory ("STDSTDSTDXXXXXXXXXXXXXXXSTDXXXXXXXXXXXXXXXSTD") into  "STD" "STD" "STD" "XXX" "XXX"...
    text = gsub('STD', '000', text) # Replace "STD" by "000"
    return(as.numeric(text))
  }
}

hist1  = simplify2array(sapply(full_a[,'paymenthistory1'], pay_text))
hist2  = simplify2array(sapply(full_a[,'paymenthistory2'], pay_text))

# payment_history_avg_dpd_0_29_bucket
hist1_29 = sapply(hist1, function(x)  sum(x<30, na.rm = TRUE)) # Count of accounts that is in 0-29 days past due bucket throughout the payment history, , with less than 30 days past due being a ‘punctual’ payment and therefore lower risk.
hist2_29 = sapply(hist2, function(x)  sum(x<30, na.rm = TRUE))
x_a$pay_dpd_0_29_bucket = apply(cbind(hist1_29, hist2_29), 1, sum)

# payment_history_avg_dpd_30_bucket
hist1_30 = sapply(hist1, function(x)  sum(x>30, na.rm = TRUE)) # Count of each payment falling into the 30-or-more-days-past-due bag, with more than 30 days past due being a ‘late’ payment and therefore higher risk.
hist2_30 = sapply(hist2, function(x)  sum(x>30, na.rm = TRUE))
x_a$pay_dpd_30_bucket = apply(cbind(hist1_30, hist2_30), 1, sum)


# payment_history_mean_length
hist1_total = sapply(hist1, function(x)  length(x)) # Length of the payment history
hist2_total = sapply(hist2, function(x)  length(x))
x_a$payment_history_mean_length = apply(cbind(hist1_total, hist2_total), 1, sum)

# pay_dpd_ratio
# Ratio of the average number of payments falling into the 30-or-more-days-past-due bag against average number of payments. Indicating duration of non-payment throughout payment history.                     
x_a$pay_dpd_ratio = ifelse(x_a[,payment_history_mean_length] == 0, NA, x_a[,pay_dpd_0_29_bucket]/x_a[,payment_history_mean_length])
                     
# Average and merge
x_a = x_a[, lapply(.SD,mean), by = 'customer_no', .SDcols = names(x_a)[3:ncol(x_a)]]

# total_diff_lastpaymt_opened_dt 
# The total duration between last payment date and account opened date of all accounts. Shows recency of payment.                     
full_a$total_diff_lastpaymt_opened_dt = as.numeric(full_a[,'last_paymt_dt'] - full_a[,'opened_dt'])
temp = data.table(full_a[,c('customer_no', 'total_diff_lastpaymt_opened_dt')])
temp = temp[, lapply(.SD, sum), by = 'customer_no', .SDcols = c('total_diff_lastpaymt_opened_dt')]# Apply a function on a subset of columns.
x_a  = merge(x_a, temp, 'customer_no')

# min_months_last_30_plus
# Minimum number of months that passed before first 30-or-more-days-past-due bag appeared, showing punctuality                     
months_dpd_30_1 = mapply(function(x,y){if (y>0){return(x)} else{return(NA)}}, hist1_29, hist1_30)
months_dpd_30_2 = mapply(function(x,y){if (y>0){return(x)} else{return(NA)}}, hist2_29, hist2_30)
temp = data.frame('customer_no' = full_a$customer_no)
temp$min_months_last_30_plus = apply(cbind(months_dpd_30_1, months_dpd_30_2), 1, min)
temp = data.table(temp, key = 'customer_no')
temp = temp[, lapply(.SD, min), by = 'customer_no', .SDcols = c('min_months_last_30_plus')]
x_a  = merge(x_a, temp)

# utilisation_trend
# [total_cur_balance_amt / total_credit_limit] / [mean_cur_balance_amt / (mean_credit_limit+ mean_cashlimit)]                     
temp = full_a[,c('customer_no','cur_balance_amt','creditlimit','cashlimit')]
temp$utilisation_trend = ((ave(temp$cur_balance_amt, temp$customer_no, FUN = sum)/
                             ave(temp$creditlimit, temp$customer_no, FUN = sum))/
                            (ave(temp$cur_balance_amt, temp$customer_no, FUN = mean)/
                               (ave(temp$creditlimit, temp$customer_no, FUN = mean) +
                                  ave(temp$cashlimit, temp$customer_no, FUN = mean))))
temp[which(is.infinite(temp[,'utilisation_trend'])), 'utilisation_trend'] = NA
temp = data.table(unique(temp[,c(1,ncol(temp))]), key = 'customer_no')
x_a  = merge(x_a, temp)

# ratio_currbalance_creditlimit
# Ratio of current balance to credit limit, showing how much applicant is dependent on credit.
temp = full_a[,c('customer_no','cur_balance_amt','creditlimit')]
temp$ratio_currbalance_creditlimit = ave(full_a$cur_balance_amt, full_a$customer_no, FUN = sum) / 
  ave(full_a$creditlimit, full_a$customer_no, FUN = sum)
temp[which(is.infinite(temp[,'ratio_currbalance_creditlimit'])), 'ratio_currbalance_creditlimit'] = NA
temp = data.table(unique(temp[,c(1,ncol(temp))]), key = 'customer_no')
x_a  = merge(x_a, temp)

# count_enquiry_recency_365
# Number of enquiries made in the last 365 days. Shows frequency of applications.                     
temp   = full_e[, lapply(.SD, max), by = 'customer_no', .SDcols = 'enquiry_dt']
temp   = rename(temp, c('enquiry_dt' = 'max_dt'))
full_e = merge(full_e, temp)
full_e$recent_flag = as.numeric((full_e$max_dt - full_e$enquiry_dt) < 366)
temp = full_e[,c('customer_no','recent_flag')]
temp = temp[, lapply(.SD, sum), by = 'customer_no', .SDcols = 'recent_flag']
temp = rename(temp, c('recent_flag' = 'count_enquiry_recency_365'))
x_e  = merge(x_e, temp)
x_e  = x_e[,c(1,3)]

# count_enquiry_recency_90
# Number of enquiries made in the last 90 days. Shows frequency of applications.                     
full_e$recent_flag = as.numeric((full_e$max_dt - full_e$enquiry_dt) < 91)
temp = full_e[,c('customer_no','recent_flag')]
temp = temp[, lapply(.SD, sum), by = 'customer_no', .SDcols = 'recent_flag']
temp = rename(temp, c('recent_flag' = 'count_enquiry_recency_90'))
x_e  = merge(x_e, temp)

# mean_diff_open_enquiry_dt
# Average difference between enquiry dt_opened date and enquiry date. Shows recency of the previous applications.
full_e$diff_oe = as.numeric(full_e$dt_open - full_e$enquiry_dt)
x_e = merge(x_e, full_e[, lapply(.SD, mean), by = 'customer_no', .SDcols = 'diff_oe'])
x_e = rename(x_e, c('diff_oe' = 'mean_diff_open_enquiry_dt'))

# max_freq_enquiry
# Most frequent enquiry purpose.                     
Mode = function(x) {
  y = unique(x)
  y[which.max(tabulate(match(x, y)))]
}
x_e = merge(x_e, (full_e[, lapply(.SD, Mode), by = 'customer_no', .SDcols = 'enq_purpose']))

# feature_3
x_d$feature_3 = ifelse(x_d$feature_3>741, 'high', ifelse(x_d$feature_3>723, 'medium', ifelse(x_d$feature_3>701,  'low', '-1')))
x_d$feature_3 = factor(x_d$feature_3)

# Deal with NA
to_0 = c('pay_dpd_0_29_bucket', 'pay_dpd_30_bucket')
to_mean = c('pay_dpd_ratio', 'payment_history_mean_length', 'total_diff_lastpaymt_opened_dt', 'utilisation_trend',
            'ratio_currbalance_creditlimit','count_enquiry_recency_365','count_enquiry_recency_90',
            'mean_diff_open_enquiry_dt', 'enquiry_freq')
to_max = c('min_months_last_30_plus')
x_a = data.frame(x_a)
for (x in names(x_a)){
  if (x %in% to_0){
    x_a[which(is.na(x_a[, x])), x] = 0
  }
}
for (x in names(x_a)){
  if (x %in% to_mean){
    x_a[which(is.na(x_a[,x])), x] = mean(x_a[,x], na.rm = TRUE)
  }
}
for (x in names(x_a)){
  if (x %in% to_max){
    x_a[which(is.na(x_a[,x])), x] = max(x_a[,x], na.rm = TRUE)
  }
}
x_e = data.frame(x_e)
for (x in names(x_e)){
  if (x %in% to_mean){
    x_e[which(is.na(x_e[,x])), x] = mean(x_e[,x], na.rm = TRUE)
  }
}

feat = merge(feat, data.table(x_a), key = 'customer_no')
feat = merge(feat, data.table(x_e), key = 'customer_no')
feat = merge(feat, x_d,allow.cartesian=TRUE)
feat = feat[,-'customer_no']

fac_feats  = which(sapply(feat, is.factor))

# Set NA to mode
for (x in fac_feats){
  temp = table(as.vector(feat[,x, with = FALSE]))
  feat[which(is.na(feat[, x, with = FALSE])), x] = names(temp)[temp == max(temp)]
}

feat = data.frame(feat)

train   = feat[which(feat$c == 'train'), 2:ncol(feat)]
test    = feat[which(feat$c == 'test'), 2:ncol(feat)]

# Save changes
save(train, test, file = 'feat.Rdata')

