library(GMSE)
source('notebook/goose_predict.R')

input_name <- '~/Dropbox/Islay_goose_data_from_Tom_Jan_2018/Dataset/toy_data.csv'

gmse_goose_multiplot(data_file=input_name, iterations=2, proj_yrs = 2, max_HB=1000, manage_target = 25000)

