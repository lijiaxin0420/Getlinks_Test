rm(list = ls(all = TRUE))

# define the data directory
data_directory = 'test_data/'

# Build complete DB
train_a = read.csv(file.path(data_directory, 'raw_account_70_new.csv'), header = TRUE, na.strings = c('', ' ', 'NA', 'NOT DISCLOSED'), stringsAsFactors = TRUE)
train_e = read.csv(file.path(data_directory, 'raw_enquiry_70_new.csv'), header = TRUE, na.strings = c('', ' ', 'NA', 'NOT DISCLOSED'), stringsAsFactors = TRUE)
train_d = read.csv(file.path(data_directory, 'raw_data_70_new.csv'), header = TRUE, na.strings = c('', ' ', 'NA', 'NOT DISCLOSED'), stringsAsFactors = TRUE)
test_a  = read.csv(file.path(data_directory, 'raw_account_30_new.csv'), header = TRUE, na.strings = c('', ' ', 'NA', 'NOT DISCLOSED'), stringsAsFactors = TRUE)
test_e  = read.csv(file.path(data_directory, 'raw_enquiry_30_new.csv'), header = TRUE, na.strings = c('', ' ', 'NA', 'NOT DISCLOSED'), stringsAsFactors = TRUE)
test_d  = read.csv(file.path(data_directory, 'raw_data_30_new.csv'), header = TRUE, na.strings = c('', ' ', 'NA', 'NOT DISCLOSED'), stringsAsFactors = TRUE)
train_d$c = as.factor('train')
train_a$c = as.factor('train')
train_e$c = as.factor('train')
test_d$c  = as.factor('test')
test_a$c  = as.factor('test')
test_e$c  = as.factor('test')
# Combine into a single data.table with no duplicate rows. 
full_d = unique(rbind(train_d, test_d))
full_a = unique(rbind(train_a, test_a))
full_e = unique(rbind(train_e, test_e))

save(full_d, full_e, full_a, file =  'full.Rdata')
