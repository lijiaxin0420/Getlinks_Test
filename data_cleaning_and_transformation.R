rm(list = ls(all = TRUE))

#########################################
### Data Cleaning and Transformation ###
#########################################
load('full.Rdata')

##################################
### Clean Account Segment data ###
##################################

# Delete columns with all NA
full_a = full_a[, colSums(is.na(full_a)) != nrow(full_a)]

# Remove extraneous features
extra = c('dt_opened', 'upload_dt')
full_a  = full_a[, !(colnames(full_a) %in% extra)]

# Set Numeric features
num = c('high_credit_amt', 'cur_balance_amt', 'amt_past_due', 'creditlimit', 'cashlimit', 'rateofinterest', 'actualpaymentamount')
full_a[num] = lapply(full_a[num], function(x) as.numeric(as.character(x))) # also converts non-num values to NA
for (x in num){
  full_a[which(is.na(full_a[,x])), x] = 0
  
}
full_a[which(is.na(full_a[,'creditlimit'])), x] = mean(full_a[,x], na.rm = TRUE)

# Set categorical features
cat = c('owner_indic', 'acct_type', 'paymentfrequency')
full_a[cat] = lapply(full_a[cat], factor)

# Set char. features
char = c('paymenthistory1', 'paymenthistory2')
full_a[char] = lapply(full_a[char], function(x) gsub('\"\"\"', '', x))

# Set date-time features
dt = c('last_paymt_dt', 'opened_dt', 'closed_dt', 'reporting_dt', 'paymt_str_dt', 'paymt_end_dt')
full_a[dt] = lapply(full_a[dt], function(x) as.Date(x, '%d-%B-%y'))


##################################
### Clean Enquiry Segment data ###
##################################

# Delete columns with all NA
full_e = full_e[, colSums(is.na(full_e)) != nrow(full_e)]

# Remove extraneous features
extra = c('id','externalid', 'upload_dt', 'member_short_name')
full_e  = full_e[, !(colnames(full_e) %in% extra)]

# Set Numeric features
num = c('enq_amt')
full_e[num] = lapply(full_e[num], function(x) as.numeric(as.character(x))) # also converts non-num values to NA
for (x in num){
  full_e[which(is.na(full_e[,x])), x] = mean(full_e[,x], na.rm = TRUE)
}

# Set categorical features
cat = c('enq_purpose')
full_e[cat] = lapply(full_e[cat], factor)

# Set date-time features
dt = c('enquiry_dt','dt_opened')
full_e[dt] = lapply(full_e[dt], function(x) as.Date(x, '%d-%B-%y'))


###############################
### Clean Data Segment data ###
###############################

# Remove extraneous features
full_d1 = full_d
full_d = full_d[,c(1:59,83,84)] #Remove all app_* columns (duplicates/not useful)
extra = c('entry_time', 'feature_5', 'feature_6', 'feature_20','feature_14',
          'feature_24', 'feature_45', 'feature_47', 'feature_48', 'feature_51')
full_d  = full_d[, !(colnames(full_d) %in% extra)]

# Remove rows with majority NA features
na=c('feature_8','feature_9','feature_10','feature_18','feature_49')
full_d  = full_d[, !(colnames(full_d) %in% na)]


# Set categorical features
 cat = c('feature_1', 'feature_4', 'feature_11', 'feature_12', 'feature_13', 'feature_16',
         'feature_19', 'feature_22', 'feature_23','feature_25','feature_27','feature_28',
         'feature_32','feature_33','feature_34','feature_36','feature_37','feature_38',
         'feature_43','feature_46','feature_50','feature_55','Bad_label','c')
full_d[cat] = lapply(full_d[cat], factor)

# Set Numeric features
 num = c('feature_3', 'feature_7', 'feature_17', 'feature_26', 'feature_29',
        'feature_30', 'feature_31', 'feature_35', 'feature_39', 'feature_40'
        ,'feature_41', 'feature_42', 'feature_44', 'feature_52', 'feature_56')
full_d[num] = lapply(full_d[num], function(x) as.numeric(as.character(x))) # also converts non-num values to NA
for (x in num){
  # full_d[which(is.na(full_d[,x])), x] = 0
  full_d[which(is.na(full_d[,x])), x] = mean(full_d[,x], na.rm = TRUE)
}

# Set date-time features
dt = c('feature_21', 'dt_opened','feature_54','feature_53','feature_2')
full_d[dt] = lapply(full_d[dt], function(x) as.Date(x, '%d-%B-%y'))


###############################
### Save cleaned variables ###
###############################

full_d = unique(full_d)
full_a = unique(full_a)
full_e = unique(full_e)


# Build training sets for analysis
train_d = full_d[which(full_d$c == 'train'), -which(names(full_d) == 'c')]
train_a = full_a[which(full_a$c == 'train'), -which(names(full_a) == 'c')]
train_e = full_e[which(full_e$c == 'train'), -which(names(full_e) == 'c')]

# Build test sets
test_d = full_d[which(full_d$c == 'test'), -which(names(full_d) == 'c')]
test_a = full_a[which(full_a$c == 'test'), -which(names(full_a) == 'c')]
test_e = full_e[which(full_e$c == 'test'), -which(names(full_e) == 'c')]

save(full_a, full_e, full_d, file = 'clean.Rdata')
save(train_a, train_e, train_d, file = 'train.Rdata')
save(test_a, test_e, test_d, file =  'test.Rdata')

