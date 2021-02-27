options(warn = -1)
library(dplyr)
library(jsonlite)
library(lubridate)
library(reshape)
library(timeDate)
library(xgboost)
library(zoo)
library(stringr)
library(data.table)

filepath = paste0(getwd(), "/app/services/pdfunctions")

### Load static data
# persistent data load
### Load Collateral
load( paste0(filepath,"/DATA/collateral_columns.RData"))
### Load pefindo model
model_xgb_load = readRDS(file = paste0(filepath,"/DATA/model_xgb_24MAY2020_1436.RDS"))
### Load pefindo variable importance
xgb_importance = read.csv(file =paste0(filepath,"/DATA/xgb_importance_24MAY2020.csv"))
### Load SLIK model
model_xgb_SLIK = readRDS(file = paste0(filepath,"/DATA/Dev_Hit_model_xgb.RDS"))
### Load SLIK variable importance
xgb_importance_SLIK = read.csv(paste0(filepath,'/DATA/xgb_importance_dimred.csv'), stringsAsFactors = FALSE)
### Load amenity data
amenity <- read.csv( paste0(filepath,"/DATA/AMENITY.csv") )
### Load demographic model
model_xgb_demog = readRDS(file =paste0(filepath,"/DATA/model_cust_VERSION_D2_11JUNE.RDS"))
### Load banking model
model_banking = readRDS(file =paste0(filepath,"/DATA/model_bank_optimized_8JUNE1800.RDS"))
### Load digital model
model_digital = readRDS(file =paste0(filepath,"/DATA/model_digital_optimized_25MAY2020_2248.RDS") )
### Calibration constants
calibrated_constants <- readRDS(paste0(filepath,"/DATA/20200901_all_calibrated_constants.RDS"))

# -------------------------------------------------------------------------------------------------------------- #

hitung <- function(x){
  res = x + 120
  return(res)
}

#### 1. SLK. FUNGSI MENGHITUNG PD SCORE MODUL BIRO SLIK ####

pdscore_bureau_slik <- function(input_data){
  
  ##################### 1. Buat tabel DATA POKOK DEBITUR #############################
  result_ideb <- as.data.frame(input_data[["ResultIdeb"]])
  ideb_pokok <- data.frame()
  row_length <- nrow(result_ideb)
  for (i in 1:row_length){
    ideb_indv <- result_ideb[["Ideb"]][["IdebIndividu"]][["individual"]][["dataPokokDebitur"]][[i]]
    ideb_pokok <- rbind(ideb_pokok,ideb_indv)
  }
  
  # buat tabel demografi/ data pokok debitur
  demog <- ideb_pokok
  # remove duplicated results
  
  demog_unique = demog[,c("resultid","seq","pelapor","noIdentitas")] # cek duplikat berdasarkan keterangan pelapor juga
  demog_unique = demog_unique[!duplicated(demog_unique),]
  colnames(demog_unique) = c("resultid","seq_demog","pelapor","noidentitas")
  
  ##################### 2. Buat tabel KREDIT/FASILITAS #############################
  
  data_kredit <- data.frame()
  for (i in 1:row_length){
    kredit_indv <- result_ideb[["Ideb"]][["IdebIndividu"]][["individual"]][["fasilitas"]][["kreditPembiayan"]][[i]]
    data_kredit <- rbind(data_kredit,kredit_indv)
  }
  kredit <- data_kredit
  kredit = kredit[kredit$kondisiKet %in% c("Fasilitas Aktif","FASILITAS AKTIF"),]
  
  ####-------- cek jika nasabah memiliki fasilitas slik aktif ------####
  if(nrow(kredit) == 0){
    scored_data_bureau_final_slik <- data.frame("CRN" = input_data$DemographicData$IDNumber, "xgb_bureau" = NA,
                                              "xgb_bureau_recode" = NA)
  }else{
    
    kredit_sample = kredit[kredit$resultid %in% demog_unique$resultid,]
    
    kredit123= merge(x = kredit_sample, y = demog_unique, all.x = TRUE, 
                     by.x =c('resultid','ljk'), by.y = c('resultid','pelapor'))
    
    kredit123$isCreditCard = ifelse(kredit123$jenisKreditPembiayaan=='30',1,0)
    kredit123$isUnsecured = ifelse(kredit123$jenisKreditPembiayaan=='05' &
                                     kredit123$jenisPenggunaan =='3' &
                                     kredit123$sektorEkonomi=='009000',1,0)
    
    ## Ambil data umur dari input demografik nasabah
    kredit123_5 = kredit123
    kredit123_5$tanggalLahir <- as.Date(as.character(input_data$DemographicData$DateOfBirth)) 
    kredit123_5$Age <- floor(difftime(today(), kredit123_5$tanggalLahir, units = "days")/365)
    
    df_age <-kredit123_5
    df_age2 <- as.data.frame(aggregate(Age ~ noidentitas, data = df_age, max))
    df_age2$CRN <- df_age2$noidentitas
    df_age2$noidentitas <-NULL
    
    ######################################   GET TRADELINE DATA  ###############################################
    
    
    col_needed   = c('ktp', 'resultid','fasilitasid', 'bcc_date', 'payment_history_start_date','payment_history_end_date','bureau_score',
                     'date_open', 'date_closed','date_reported','date_of_last_payment','bucket_payment_history1', 'collectibility_payment_history1',
                     'high_credit_sanctioned_amount','account_desc','account_desc_derived', 'ownership_type','reporting_member',
                     'cc_petty_bal_thresh','other_loans_petty_bal_thresh','outstanding_balance','past_due_days','over_due_amount','loan_limit',
                     'interest_rate', 'restructure_freq','restructure_reason')
    
    
    tradeline_df = kredit123_5
    tradeline_df$ktp= input_data$DemographicData$IDNumber
    tradeline_df$interest_rate=tradeline_df$sukuBungaImbalan
    tradeline_df$restructure_freq=tradeline_df$frekuensiRestrukturisasi
    tradeline_df$restructure_reason=tradeline_df$restrukturisasiKet
    
    
    ##### get BCC date 
    # note :  since no exact date between when the report are pulled, 
    #i'll use max between update date (when the bureau update the data) & created date (when reporting member report)
    #and max it again per customer and result_id (search ID)
    
    #temp = tradeline_df[,c('resultid','tanggalUpdate','tanggalUpdate2','tanggalUpdate_1','tanggalUpdate_2', 'update_date_format')]
    
    tradeline_df$tanggalUpdate_cek <- as.character(tradeline_df$tanggalUpdate, "%Y-%m-%d")
    tradeline_df$update_date_format = as.Date(tradeline_df$tanggalUpdate_cek, "%Y%m%d")
    tradeline_df$tanggalUpdate_cek = NULL
    
    
    tradeline_df$tanggalDibentuk_cek <- as.character(tradeline_df$tanggalDibentuk, "%Y-%m-%d")
    tradeline_df$created_date_format = as.Date(tradeline_df$tanggalDibentuk_cek, "%Y%m%d")
    tradeline_df$tanggalDibentuk_cek = NULL
    
    tradeline_df$month_year_format = as.Date(paste(tradeline_df$tahun,tradeline_df$bulan,'01',sep = '-'))
    tradeline_df$max_date = apply(tradeline_df[,c('update_date_format','created_date_format','month_year_format')],1,max,na.rm = TRUE)
    
    #get max date per customer - result id
    
    bcc_df = setDT(tradeline_df)[,list(
      bcc_date = max(max_date, na.rm = TRUE)
    ), by=c('ktp','resultid')]
    
    #remove duplicate record that have the same cust_id, bcc_date
    bcc_df2 = bcc_df[!duplicated(bcc_df[,c('ktp','bcc_date')]),]
    
    tradeline_df= merge(x = tradeline_df, y = bcc_df2, all.x = TRUE, by=c('ktp','resultid'))
    
    #remove duplicate search that have the same bcc_date and custid (KTP)
    tradeline_df2=tradeline_df[!is.na(tradeline_df$bcc_date)]
    
    ######
    
    tradeline_df$payment_history_start_date = as.Date(paste(tradeline_df$tahunBulan24,'01',sep = ''), "%Y%m%d")
    tradeline_df$payment_history_end_date = as.Date(paste(tradeline_df$tahunBulan01,'01',sep = ''), "%Y%m%d")
    tradeline_df$bureau_score = NA #no data
    
    tradeline_df$tanggalMulai_cek <- as.character(tradeline_df$tanggalMulai, "%Y-%m-%d")
    tradeline_df$date_open = as.Date(tradeline_df$tanggalMulai_cek, "%Y%m%d")
    tradeline_df$tanggalMulai_cek = NULL
    
    tradeline_df$tanggalJatuhTempo_cek <- as.character(tradeline_df$tanggalJatuhTempo, "%Y-%m-%d")
    tradeline_df$date_closed = as.Date(tradeline_df$tanggalJatuhTempo_cek, "%Y%m%d")
    tradeline_df$tanggalJatuhTempo_cek = NULL
    
    tradeline_df$date_reported = apply(tradeline_df[,c('update_date_format','created_date_format')],1,max,na.rm = TRUE)
    
    tradeline_df$date_of_last_payment = NA  #no data
    
    tradeline_df$bucket_payment_history1=''
    tradeline_df$collectibility_payment_history1=''
    
    tradeline_df_ori = tradeline_df
    
    for (i in 13:24)
    {
      strkol = as.character(i)
      if(i<10)
      {
        strkol=paste('0',i, sep = '')
      }
      
      source_kol = paste('tahunBulan',strkol,'Ht',sep = '')
      dest_kol = paste('tahunBulan',strkol,'bucket', sep = '')
      kol_kol =  paste('tahunBulan',strkol,'Kol', sep = '')
      
      tradeline_df[[source_kol]] = as.numeric(tradeline_df[[source_kol]])
      tradeline_df[[dest_kol]] = ifelse(tradeline_df[[source_kol]]>180,'7',
                                        ifelse(tradeline_df[[source_kol]]>120,'6',
                                               ifelse(tradeline_df[[source_kol]]>90,'5',
                                                      ifelse(tradeline_df[[source_kol]]>60,'4',
                                                             ifelse(tradeline_df[[source_kol]]>30,'3',
                                                                    ifelse(tradeline_df[[source_kol]]>0,'2',
                                                                           ifelse(tradeline_df[[source_kol]]==0,'1','X')))))))
      tradeline_df[[kol_kol]][is.na(tradeline_df[[kol_kol]])]='X'
      tradeline_df[[dest_kol]][is.na(tradeline_df[[dest_kol]])]='X'
      tradeline_df[[kol_kol]]=as.character(tradeline_df[[kol_kol]])
      tradeline_df$bucket_payment_history1=paste(tradeline_df$bucket_payment_history1,tradeline_df[[dest_kol]], sep = '')
      tradeline_df$collectibility_payment_history1=paste(tradeline_df$collectibility_payment_history1,tradeline_df[[kol_kol]], sep = '')
      
    }
    
    tradeline_df$max_platfon = apply(tradeline_df[,c('plafonAwal','plafon')],1,max,na.rm = TRUE)
    
    high_limit_df = setDT(tradeline_df)[,list(
      high_credit_sanctioned_amount = max(max_platfon, na.rm = TRUE)
    ), by=c('ktp','resultid')]
    
    tradeline_df= merge(x = tradeline_df, y = high_limit_df, all.x = TRUE, by=c('ktp','resultid'))
    
    
    tradeline_df$ljkket_upper = toupper(tradeline_df$ljkKet)
    
    tradeline_df$account_desc= ifelse(tradeline_df$jenisKreditPembiayaan=='30','Credit Card',
                                      ifelse(tradeline_df$jenisKreditPembiayaan=='05' & tradeline_df$jenisPenggunaan =='3' & tradeline_df$sektorEkonomi=='009000','Unsecured Loan',
                                             ifelse(tradeline_df$jenisKreditPembiayaan=='05' & tradeline_df$sektorEkonomi %in% c('001100','001110','001120','001130','001210','001220','001230','001300'),'Mortgage Loan',
                                                    ifelse(tradeline_df$jenisKreditPembiayaan=='05' & tradeline_df$sektorEkonomi %in% c('004120','004130','004140','004150','004160','004180','004190','004900'),'Multi Purpose Loan',
                                                           ifelse(tradeline_df$jenisKreditPembiayaan %in% c('10','20') & tradeline_df$sektorEkonomi %in% c('002100','002200','002300','002900'),'Auto Loan - Joint Finance, Multifinance',
                                                                  ifelse(tradeline_df$jenisKreditPembiayaan=='05' & tradeline_df$sektorEkonomi %in% c('002100','002200','002300','002900'),'Auto Loan',
                                                                         ifelse(tradeline_df$jenisKreditPembiayaan=='80','Current Account Loan',
                                                                                ifelse(tradeline_df$jenisKreditPembiayaan %in% c('10','20'),'Other Multifinance Loan',
                                                                                       ifelse(grepl("BANK", tradeline_df$ljkket_upper)==FALSE,'Other Multifinance Loan',
                                                                                              ifelse(tradeline_df$jenisPenggunaan=='1','Other Working Capita Loan',
                                                                                                     ifelse(tradeline_df$jenisPenggunaan=='2','Other Investment Loan',"Other Loan")))))))))))
    
    # tradeline_df$account_desc_derived = NA 
    tradeline_df$account_desc_derived = ifelse(tradeline_df$account_desc=='Credit Card','Credit Card',
                                               ifelse(tradeline_df$account_desc=='Unsecured Loan','Unsecured Loan',
                                                      ifelse(tradeline_df$account_desc=='Mortgage Loan','Secured Loan',
                                                             ifelse(tradeline_df$account_desc=='Multi Purpose Loan','Secured Loan',
                                                                    ifelse(tradeline_df$account_desc=='Auto Loan - Joint Finance, Multifinance','Secured Loan',
                                                                           ifelse(tradeline_df$account_desc=='Auto Loan','Secured Loan',
                                                                                  ifelse(tradeline_df$account_desc=='Current Account Loan','Other Working Capita Loan',tradeline_df$account_desc)))))))
    
    
    tradeline_df$kategoridebiturket_upper = toupper(tradeline_df$kategoriDebiturKet)
    
    tradeline_df$ownership_type = tradeline_df$kategoridebiturket_upper #No data as requested, putting debitor categry for now
    tradeline_df$reporting_member = tradeline_df$ljkket_upper
    tradeline_df$cc_petty_bal_thresh =NA
    tradeline_df$other_loans_petty_bal_thresh=NA
    tradeline_df$outstanding_balance = as.numeric(tradeline_df$bakiDebet)
    tradeline_df$over_due_amount = as.numeric(tradeline_df$tunggakanBunga)+as.numeric(tradeline_df$tunggakanPokok)+as.numeric(tradeline_df$denda)
    tradeline_df$past_due_days = tradeline_df$jumlahHariTunggakan
    tradeline_df$loan_limit = tradeline_df$plafon
    
    
    final_tradeline = tradeline_df[, ..col_needed]
    final_tradeline = final_tradeline[!is.na(final_tradeline$ktp),]
    
    final_tradeline = final_tradeline[!duplicated(final_tradeline[,c("ktp","resultid","fasilitasid")]),]
    
    final_tradeline$period2 <- as.character(today(), "%Y-%m-%d") # NEED TO RECHECK
    final_tradeline$period3 <- as.Date(final_tradeline$period2, "%Y-%m-%d")
    final_tradeline$period2 = NULL
    
    final_tradeline$reference_date = final_tradeline$period3
    
    ##################################################################
    ###################### FEATURE ENGINERING ########################
    ##################################################################
    
    Tradeline_data <-  final_tradeline 
    
    #FINA : FIXING Col name
    Tradeline_data$CRN = Tradeline_data$ktp
    Tradeline_data$Bureau_score = 0 # no datab assume 0
    Tradeline_data$DATE_OPENED = Tradeline_data$date_open
    Tradeline_data$DATE_CLOSED = Tradeline_data$date_closed
    Tradeline_data$Reference_date = Tradeline_data$reference_date
    Tradeline_data$DateReported_trades = Tradeline_data$date_reported
    Tradeline_data$PaymentHistoryStartDate = Tradeline_data$payment_history_start_date
    Tradeline_data$PaymentHistoryEndDate = Tradeline_data$payment_history_end_date
    Tradeline_data$PaymentHistory1 = Tradeline_data$bucket_payment_history1
    Tradeline_data$HighCreditSanctionedAmount = Tradeline_data$high_credit_sanctioned_amount
    Tradeline_data$Ownership_type =1 # no data. assume 1
    Tradeline_data$ACCOUNT_DESC =  Tradeline_data$account_desc
    Tradeline_data$CC_petty_bal_thresh=300000
    Tradeline_data$Other_loans_petty_bal_thresh = 300000
    Tradeline_data$BCC_DATE = Tradeline_data$bcc_date
    Tradeline_data$ReportingMemberShortName = Tradeline_data$reporting_member
    Tradeline_data$Over_due_amount = Tradeline_data$over_due_amount
    Tradeline_data$Out_standing_Balance = Tradeline_data$outstanding_balance
    Tradeline_data$interest_rate = as.numeric(Tradeline_data$interest_rate)
    
    Tradeline_data$typeofcreditor = ifelse(grepl("BANK", Tradeline_data$reporting_member)==TRUE,"BANKS",
                                           ifelse(grepl("BPR", Tradeline_data$reporting_member)==TRUE,"BPR","Others"))
    
    Tradeline_data$worstDPDBucket = ifelse(grepl("7", Tradeline_data$bucket_payment_history1)==TRUE,7,
                                           ifelse(grepl("6", Tradeline_data$bucket_payment_history1)==TRUE,6,
                                                  ifelse(grepl("5", Tradeline_data$bucket_payment_history1)==TRUE,5,
                                                         ifelse(grepl("4", Tradeline_data$bucket_payment_history1)==TRUE,4,
                                                                ifelse(grepl("3", Tradeline_data$bucket_payment_history1)==TRUE,3,
                                                                       ifelse(grepl("2", Tradeline_data$bucket_payment_history1)==TRUE,2,1))))))
    
    
    Tradeline_data$resturcture_flag = as.numeric(Tradeline_data$restructure_freq)
    Tradeline_data$isInternalReportingMember = ifelse(Tradeline_data$reporting_member %in% c("BANK OCBC NISP","BANK OCBC NISP SYARIAH (UUS)"),1,0)
    
    
    Business_Loan1 <- c("Other Working Capita Loan","Current Account Loan","Multi Purpose Loan")
    Agriculture_Loan1 <- c()
    Auto_Loan1 <- c("Auto Loan","Auto Loan - Joint Finance, Multifinance")
    Credit_card1 <- c("Credit Card")
    Housing_Loan1 <- c("Mortgage Loan")
    Personal_Loan1 <- c("Unsecured Loan")
    Short_Term_Secured_Loan1 <- c()
    Others1 <- c("Other Investment Loan","Other Loan","Other Multifinance Loan") #NOT SURE
    
    
    
    Define_loan_types <-function(a,b,c,d,e,f,g,h){
      
      Business_Loan <<- a
      Agriculture_Loan <<- b
      Auto_Loan <<- c
      Credit_card <<- d
      Housing_Loan <<- e
      Personal_Loan <<- f
      Short_Term_Secured_Loan <<- g
      Others <<- h
      
      
    }
    
    
    
    Define_loan_types(Business_Loan1,Agriculture_Loan1,Auto_Loan1,Credit_card1,Housing_Loan1,Personal_Loan1,Short_Term_Secured_Loan1,Others1)
    
    # Defining Reporting Member type- to be used for classifying it as Internal/ External
    # Please pass string array to function 'Define_reporting_member_type' to define bucket for classifying reporting member as internal
    
    Define_reporting_member_type <-function(x){
      Internal <<- x
    }
    
    Internal_bank_list <- c("BANK OCBC NISP","BANK OCBC NISP SYARIAH (UUS)")
    Define_reporting_member_type(Internal_bank_list)
    
    
    #Defining formats
    Tradeline_data <- as.data.frame(Tradeline_data)
    Tradeline_data$CRN <- as.character(Tradeline_data$CRN)
    
    
    #Deduping on CRN,BCC date & reference date- data_uniqueCRN_ref_BCC_date
    data_uniqueCRN_ref_BCC_date<- Tradeline_data[!duplicated(Tradeline_data[,c("CRN","Reference_date","BCC_DATE")]),]
    data_uniqueCRN_ref_BCC_date <- data.table(data_uniqueCRN_ref_BCC_date)
    # Sorting based on CRN and BCC date
    data_uniqueCRN_ref_BCC_date <- data_uniqueCRN_ref_BCC_date[order(data_uniqueCRN_ref_BCC_date$CRN,-data_uniqueCRN_ref_BCC_date$BCC_DATE),]
    # Only retaining data with BCC_date < Reference date
    data_uniqueCRN_ref_BCC_date <- data_uniqueCRN_ref_BCC_date[data_uniqueCRN_ref_BCC_date$Reference_date>data_uniqueCRN_ref_BCC_date$BCC_DATE,c("CRN","Reference_date","BCC_DATE") ]
    
    
    Tradeline_data2 <- merge(Tradeline_data,data_uniqueCRN_ref_BCC_date[,c("CRN","BCC_DATE")],by=c("CRN","BCC_DATE"))
    
    # Deduping the trades based on CRN, Loan type, Date opened, Highest Credit sanction amount 
    Tradeline_data2<-arrange(Tradeline_data2,CRN,Ownership_type,ACCOUNT_DESC,DATE_OPENED,HighCreditSanctionedAmount,desc(DateReported_trades))
    Tradeline_data2<-Tradeline_data2[!duplicated(Tradeline_data2[c("CRN","ACCOUNT_DESC","DATE_OPENED","HighCreditSanctionedAmount")]),]
    
    
    
    #---------------------------------      2. FEATURE ENGINEERING- CREATING DPD HISTORY VARIABLES    --------------------------------------------------
    
    # 
    feature_engineering_DPD_variables <- function(Data_tradeline){
      
      Tradeline_data2 <- Data_tradeline
      Tradeline_data2 <- as.data.frame(Tradeline_data2)
      
      
      ## Creating DPD flags for 5+, 30+, 60+, 90+ DPD ever
      # Creating flags
      #90+ dpd bucket 5
      Tradeline_data2$index_90_plus =13 - regexpr('5',Tradeline_data2$bucket_payment_history1)
      Tradeline_data2$index_90_plus = ifelse(Tradeline_data2$index_90_plus==14, -1,Tradeline_data2$index_90_plus)
      Tradeline_data2$delq_90_plus_flag = ifelse(Tradeline_data2$index_90_plus==-1, "",1)
      temp=Tradeline_data2[,c('bucket_payment_history1','index_90_plus','delq_90_plus_flag')]
      #60+ dpd bucket 4
      Tradeline_data2$index_60_plus =13 - regexpr('4',Tradeline_data2$bucket_payment_history1)
      Tradeline_data2$index_60_plus = ifelse(Tradeline_data2$index_60_plus==14, -1,Tradeline_data2$index_60_plus)
      Tradeline_data2$delq_60_plus_flag = ifelse(Tradeline_data2$index_60_plus==-1, "",1)
      temp=Tradeline_data2[,c('bucket_payment_history1','index_60_plus','delq_60_plus_flag')]
      #30+ dpd bucket 3
      Tradeline_data2$index_30_plus =13 - regexpr('3',Tradeline_data2$bucket_payment_history1)
      Tradeline_data2$index_30_plus = ifelse(Tradeline_data2$index_30_plus==14, -1,Tradeline_data2$index_30_plus)
      Tradeline_data2$delq_30_plus_flag = ifelse(Tradeline_data2$index_30_plus==-1, "",1)
      temp=Tradeline_data2[,c('bucket_payment_history1','index_30_plus','delq_30_plus_flag')]
      #index_5_plus = assume 1+ dpd bucket 2
      Tradeline_data2$index_5_plus =13 - regexpr('2',Tradeline_data2$bucket_payment_history1)
      Tradeline_data2$index_5_plus = ifelse(Tradeline_data2$index_5_plus==14, -1,Tradeline_data2$index_5_plus)
      Tradeline_data2$delq_5_plus_flag = ifelse(Tradeline_data2$index_5_plus==-1, "",1)
      temp=Tradeline_data2[,c('bucket_payment_history1','index_5_plus','delq_5_plus_flag')]
      
      #Creating delinquency flag -> Anyone who has gone either 5+DPD,or 30+DPD,or 60+DPD,or 90+DPD ever- is tagged as delinquent
      
      Tradeline_data2$delq_flag<-ifelse(is.na(as.numeric(apply(Tradeline_data2[,c("delq_5_plus_flag","delq_30_plus_flag","delq_60_plus_flag","delq_90_plus_flag")],1,max))),0,1)
      
      Tradeline_data2$delq_dt_90_plus<-ifelse(Tradeline_data2$delq_90_plus_flag=="",
                                              "",
                                              ifelse(Tradeline_data2$index_90_plus==1,
                                                     as.character(Tradeline_data2$PaymentHistoryStartDate),
                                                     as.character(ymd(as.Date(Tradeline_data2$PaymentHistoryStartDate,"%Y-%m-%d"))-((as.numeric(Tradeline_data2$index_90_plus)-1)*30))))
      Tradeline_data2$delq_dt_60_plus<-ifelse(Tradeline_data2$delq_60_plus_flag=="",
                                              "",
                                              ifelse(Tradeline_data2$index_60_plus==1,
                                                     as.character(Tradeline_data2$PaymentHistoryStartDate),
                                                     as.character(ymd(as.Date(Tradeline_data2$PaymentHistoryStartDate,"%Y-%m-%d"))-((as.numeric(Tradeline_data2$index_60_plus)-1)*30))))
      Tradeline_data2$delq_dt_30_plus<-ifelse(Tradeline_data2$delq_30_plus_flag=="",
                                              "",
                                              ifelse(Tradeline_data2$index_30_plus==1,
                                                     as.character(Tradeline_data2$PaymentHistoryStartDate),
                                                     as.character(ymd(as.Date(Tradeline_data2$PaymentHistoryStartDate,"%Y-%m-%d"))-((as.numeric(Tradeline_data2$index_30_plus)-1)*30))))
      Tradeline_data2$delq_dt_5_plus<-ifelse(Tradeline_data2$delq_5_plus_flag=="",
                                             "",
                                             ifelse(Tradeline_data2$index_5_plus==1,
                                                    as.character(Tradeline_data2$PaymentHistoryStartDate),
                                                    as.character(ymd(as.Date(Tradeline_data2$PaymentHistoryStartDate,"%Y-%m-%d"))-((as.numeric(Tradeline_data2$index_5_plus)-1)*30))))
      
      temp=Tradeline_data2[,c('bucket_payment_history1','delq_flag','PaymentHistoryStartDate','index_5_plus','delq_dt_90_plus')]
      
      # Missing value imputation for DATE_OPENED {
      
      Tradeline_data2_null_opened_dt <- Tradeline_data2[is.na(Tradeline_data2$DATE_OPENED),] # Null trade open date
      
      # If DATE_OPENED is null, but PaymentHistoryEndDate is not null, impute DATE_OPENED with PaymentHistoryEndDate, otherwise by DateReported
      Tradeline_data2_null_opened_dt$DATE_OPENED<-as.Date(ifelse(is.na(Tradeline_data2_null_opened_dt$PaymentHistoryEndDate),
                                                                 Tradeline_data2_null_opened_dt$DateReported,
                                                                 Tradeline_data2_null_opened_dt$PaymentHistoryEndDate),"%Y-%m-%d",origin = "1970-01-01")
      
      Tradeline_data2_not_null_opened_dt <- Tradeline_data2[!is.na(Tradeline_data2$DATE_OPENED),]
      Tradeline_data2<- rbind(Tradeline_data2_null_opened_dt,Tradeline_data2_not_null_opened_dt) #Merging to get all records with mon null DATE_OPENED
      #                                           }
      
      #Finding difference between Reference_date and DATE_OPENED
      Tradeline_data2$diff_open_dt <- round(as.numeric(difftime(Tradeline_data2$Reference_date,Tradeline_data2$DATE_OPENED,units = "days")))
      
      # Tradeline_data2$diff_open_dt <-ifelse(is.na(Tradeline_data2$DATE_OPENED), 999999,
      #                          round(as.numeric(difftime(Tradeline_data2$Reference_date,Tradeline_data2$DATE_OPENED,units = "days"))))
      
      
      
      ## Difference between the reference date and delinquency dates
      
      Tradeline_data2$diff_delq_90_plus_dt <- ifelse(Tradeline_data2$delq_dt_90_plus == ""|is.na(Tradeline_data2$delq_dt_90_plus),999999,round(as.numeric(difftime(Tradeline_data2$Reference_date,as.Date(Tradeline_data2$delq_dt_90_plus,"%Y-%m-%d"),units = "days"))))
      Tradeline_data2$diff_delq_60_plus_dt <- ifelse(Tradeline_data2$delq_dt_60_plus == ""|is.na(Tradeline_data2$delq_dt_60_plus),999999,round(as.numeric(difftime(Tradeline_data2$Reference_date,as.Date(Tradeline_data2$delq_dt_60_plus,"%Y-%m-%d"),units = "days"))))
      Tradeline_data2$diff_delq_30_plus_dt <- ifelse(Tradeline_data2$delq_dt_30_plus == ""|is.na(Tradeline_data2$delq_dt_30_plus),999999,round(as.numeric(difftime(Tradeline_data2$Reference_date,as.Date(Tradeline_data2$delq_dt_30_plus,"%Y-%m-%d"),units = "days"))))
      Tradeline_data2$diff_delq_5_plus_dt <- ifelse(Tradeline_data2$delq_dt_5_plus == ""|is.na(Tradeline_data2$delq_dt_5_plus),999999,round(as.numeric(difftime(Tradeline_data2$Reference_date,as.Date(Tradeline_data2$delq_dt_5_plus,"%Y-%m-%d"),units = "days"))))
      
      ## Creating variable max_dt in order to identifying the last payment date as maximum of "DateOfLastPayment","PaymentHistoryStartDate","DateReported"
      Tradeline_data2$DateOfLastPayment = Tradeline_data2$PaymentHistoryStartDate
      Tradeline_data2$max_dt <- apply(Tradeline_data2[,c("DateOfLastPayment","PaymentHistoryStartDate","DateReported_trades")],1,max)
      
      
      ## Clubbing individual account descriptions to the buckets defined in the beginning & tagging Reporting Member as internal/external  {
      
      Tradeline_data2$ACCOUNT_DESC <-iconv(Tradeline_data2$ACCOUNT_DESC,"ASCII",sub="")
      
      Tradeline_data2$ACCOUNT_DESC_derived <- ifelse((Tradeline_data2$ACCOUNT_DESC %in% Business_Loan),as.character("Business Loan"),
                                                     ifelse((Tradeline_data2$ACCOUNT_DESC %in% Agriculture_Loan),as.character("Agriculture Loan"),
                                                            ifelse((Tradeline_data2$ACCOUNT_DESC %in% Auto_Loan),as.character("Auto Loan"),
                                                                   ifelse((Tradeline_data2$ACCOUNT_DESC %in% Credit_card),as.character("Credit Card"),
                                                                          ifelse((Tradeline_data2$ACCOUNT_DESC %in% Housing_Loan),as.character("Housing Loan"),
                                                                                 ifelse((Tradeline_data2$ACCOUNT_DESC %in% Others),as.character("Other"),
                                                                                        ifelse((Tradeline_data2$ACCOUNT_DESC %in% Personal_Loan),as.character("Personal Loan"),
                                                                                               ifelse((Tradeline_data2$ACCOUNT_DESC %in% Short_Term_Secured_Loan),as.character("Short Term Secured Loan"),
                                                                                                      ifelse(is.na(Tradeline_data2$ACCOUNT_DESC),as.character("Other"),as.character(Tradeline_data2$ACCOUNT_DESC))))))))))
      
      
      
      
      Tradeline_data2$ReportingMemberShortName_derived <- ifelse((Tradeline_data2$ReportingMemberShortName %in% Internal),as.character("Internal"),as.character("External"))
      
      # Defining Loan status- Live/ Closed {
      
      # To make sure if closed date is populated and greater than Reference date then status of loan is closed
      
      Tradeline_data2$status_Loan <- ifelse((!is.na(Tradeline_data2$DATE_CLOSED) & Tradeline_data2$DATE_CLOSED<Tradeline_data2$Reference_date),"Closed",
                                            ifelse(!is.na(Tradeline_data2$DATE_CLOSED),"Live",
                                                   ifelse(Tradeline_data2$ACCOUNT_DESC_derived!="Credit Card" & Tradeline_data2$Out_standing_Balance==0,"Closed",
                                                          ifelse(round(difftime(Tradeline_data2$Reference_date,Tradeline_data2$max_dt,units="days"))>366 & Tradeline_data2$Out_standing_Balance<5000,"Closed","Live"))))
      
      ## Change the status of the loan for missing to closed loans {
      
      Tradeline_data2$status_Loan[is.na(Tradeline_data2$status_Loan)]<-"Closed"
      #                                     }
      
      ## Creating variable for Loan amount, Amount Over due and amount paid off {
      Tradeline_data2$Amount<- ifelse((Tradeline_data2$ACCOUNT_DESC == "Credit Card") ,as.numeric(Tradeline_data2$Out_standing_Balance),
                                      as.numeric(Tradeline_data2$HighCreditSanctionedAmount))
      Tradeline_data2$Amount <- ifelse(is.na(Tradeline_data2$Amount),0,Tradeline_data2$Amount)
      
      Tradeline_data2$Over_due_amount <- ifelse(is.na(Tradeline_data2$Over_due_amount),0,Tradeline_data2$Over_due_amount)
      Tradeline_data2$Amt_paid_off  <- Tradeline_data2$Amount- as.numeric(Tradeline_data2$Out_standing_Balance)
      #                                                                         }              
      
      ## Excluding CD and TW trades of clients- Specific- leave for now
      
      # data_Tradeline_data2 <- Tradeline_data2[!((Tradeline_data2$ReportingMemberShortName_derived == "Internal")&(Tradeline_data2$ACCOUNT_DESC %in% c("Two-wheeler Loan","Consumer Loan","Personal Loan"))),]
      # # data_Tradeline_data2 <- data_Tradeline_data2[data_Tradeline_data2$PaymentHistoryStartDate<="2018-03-01",]
      # data_Tradeline_data2 <- data.table(data_Tradeline_data2)
      
      months <- c(91,183,275,365,548,730,1095,1460,1825)
      Tradeline_data2$mob_bucket = ifelse(Tradeline_data2$diff_open_dt<=183,'mob_less6m',
                                          ifelse(Tradeline_data2$diff_open_dt<=365,'mob_6m_1y',
                                                 ifelse(Tradeline_data2$diff_open_dt<=730,'mob_1y_2y','mob_more2y')))
      
      Treadline_train = Tradeline_data2[Tradeline_data2$val2==0,]
      avg_intRate_train_map = setDT(Treadline_train)[,list(
        avg_prod_mob_intrate_all = mean(interest_rate, na.rm = TRUE)
      ), by=c('ACCOUNT_DESC_derived',"mob_bucket")]
      
      Tradeline_data2= merge(x = Tradeline_data2, y = avg_intRate_train_map, all.x = TRUE, by.x =c('ACCOUNT_DESC_derived',"mob_bucket"), by.y = c('ACCOUNT_DESC_derived',"mob_bucket"))
      Tradeline_data2$ratio_interest_rate = Tradeline_data2$interest_rate/Tradeline_data2$avg_prod_mob_intrate_all
      data_tradeline_with_DPD_var <- Tradeline_data2
      return(data_tradeline_with_DPD_var)
      
    }
    
    
    Tradeline_data_with_DPD_var <- feature_engineering_DPD_variables(Tradeline_data2)
    
    # ----------------------------------   3.  FEATURE ENGINEERING- LOAN TYPE BASED VARIABLES  ----------------------------------------
    
    # Defining function to create these variables
    
    Tradeline_data_with_DPD_var$score = 0
    Tradeline_data_with_DPD_var$HighCreditSanctionedAmount = as.numeric(Tradeline_data_with_DPD_var$HighCreditSanctionedAmount)
    Tradeline_data_with_DPD_var$WrittenOffAmountTotal = ifelse(Tradeline_data_with_DPD_var$past_due_days>180, Tradeline_data_with_DPD_var$Out_standing_Balance,0)
    Tradeline_data_with_DPD_var$writeoff_status = ifelse(Tradeline_data_with_DPD_var$past_due_days>180, 1,0)
    Tradeline_data_with_DPD_var$suit_filed_status=NA#no data assume 0
    Tradeline_data_with_DPD_var$SettlementAmount=0 #no data assume 0
    
    feature_engineering_loan_type_variables <- function(Data_tradeline){
      
      data_Tradeline_data2 <-data.table(Data_tradeline) #rename data_Tradeline_data2 later 
      
      
      TL_data_CRN <-data_Tradeline_data2[,j=list(score = max(score,0),
                                                 num_Live = length(CRN[(status_Loan=="Live")]),
                                                 Bureau_tenure_Other= max(diff_open_dt[(ACCOUNT_DESC_derived=="Other")],0),
                                                 num_loans = .N,
                                                 loan_amt_Individual = sum(Amount[(Ownership_type==1)],na.rm=TRUE),
                                                 tsl_HL = min(diff_open_dt[(ACCOUNT_DESC_derived=="Housing Loan")],999999),
                                                 tsl_live = min(diff_open_dt[status_Loan=="Live"],999999)
                                                 
      ),by=CRN]
      
      TL_data_CRN[,Utilization:=ifelse((TL_data_CRN$Sanctioned_Amt_CC==0|is.na(TL_data_CRN$Sanctioned_Amt_CC)),0,TL_data_CRN$tot_outs_amt_CC/TL_data_CRN$Sanctioned_Amt_CC)]
      
      return (TL_data_CRN)
      
    }
    
    
    
    Tradeline_data_with_loan_type_var <- feature_engineering_loan_type_variables(Tradeline_data_with_DPD_var) 
    
    
    
    #---------------------------------     4.  Creating the loan type variables at rolling time windows  ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    
    
    feature_engineering_Loan_type_trend_variables <- function(data_Tradeline_data2){
      
      full_data2 <- data_Tradeline_data2
      
      #fina: make CRN level
      full_data2=data.frame(full_data2[,c('CRN')])
      full_data2=data.frame(full_data2[!duplicated(full_data2),])
      colnames(full_data2)[1] <- "CRN"
      
      
      months <- c(730,1095)
      month_names<-c("1m_to_2yr","2yr_to_3yr", "3yr_to_5yr")
      
      
      for (i in 1:length(months)){
        
        if(i==1){
          dataset <- subset(data_Tradeline_data2,(diff_open_dt<=months[i]))
          
        } else {
          dataset <- subset(data_Tradeline_data2,(diff_open_dt<=months[i]) & (diff_open_dt>months[i-1]) )
          
        }
        dataset <- subset(data_Tradeline_data2,(diff_open_dt<=months[i]) & (diff_open_dt>months[i-1]) )
        dataset<- data.table(dataset)
        Fact_Acct2 <- dataset[,j=list(
          Num_Live_last = length(CRN[(status_Loan=="Live")])
          
          
        ),by=CRN]
        
        colnames(Fact_Acct2) <- paste0(colnames(Fact_Acct2),"_",month_names[i])
        colnames(Fact_Acct2)[1] <- "CRN"
        
        full_data2 <- merge(full_data2,Fact_Acct2,by= "CRN",all.x = TRUE)
        
      }
      
      data_Tradeline_data2<- full_data2
      
      return(data_Tradeline_data2)
      
    }
    
    
    Tradeline_data_with_loan_type_trend_var<- feature_engineering_Loan_type_trend_variables(Tradeline_data_with_DPD_var) #Not on CRN level, Tradeline level
    
    
    #---------------------------------     5.  Creating the loan type variables at monthly level  ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    
    
    # Defining function to create these variables
    
    feature_engineering_loan_type_monthly_variables <- function(Data_tradeline){
      
      
      # Data_tradeline = Tradeline_data_with_DPD_var
      full_data_CRN <- Data_tradeline
      #fina: make CRN level
      full_data_CRN=data.frame(full_data_CRN[,c('CRN')])
      full_data_CRN=data.frame(full_data_CRN[!duplicated(full_data_CRN),])
      colnames(full_data_CRN)[1] <- "CRN"
      
      
      months <- c(31,92,183,274,365,548,730,1095,1460,1825)
      month_names<-c("1m","3m","6m","9m","1yr","18m","2yr","3yr","4yr","5yr")
      
      
      for (i in 1:length(months)){
        
        dataset <- subset(Data_tradeline,(diff_open_dt<=months[i]))
        dataset <- data.table(dataset)
        TL_CRN <- dataset[,j=list(num_trades_open_last = length(CRN),
                                  Num_CC_last = length(CRN[ACCOUNT_DESC_derived=="Credit Card"]),
                                  Num_BL_last = length(CRN[ACCOUNT_DESC_derived=="Business Loan"]),
                                  loan_amt_last = max(sum(Amount[ACCOUNT_DESC_derived !="Credit Card"]),0),
                                  Amt_BL_last = sum(Amount[ACCOUNT_DESC_derived=="Business Loan"],0),
                                  Amt_Live_last = sum(Amount[(status_Loan=="Live")],0),
                                  Amt_Individual_last = sum(Amount[(Ownership_type==1)],0),
                                  Num_Other_last = length(CRN[ACCOUNT_DESC_derived=="Other"]),
                                  Amt_sanct_CC_last = max(sum(Amount[ACCOUNT_DESC_derived=="Credit Card"]),0)
                                  
        ),by=CRN]
        
        colnames(TL_CRN) <- paste0(colnames(TL_CRN),"_",month_names[i])
        colnames(TL_CRN)[1] <- "CRN"
        
        full_data_CRN <- merge(full_data_CRN,TL_CRN,by= "CRN",all.x = TRUE)
      }
      
      ## To create monthly delinquency variables
      
      BUREAU_TL_data_CRN_level <- full_data_CRN # (same as Tradeline_data_with_DPD_var/ Data_tradeline)
      #months <- c(91,183,275,365,548,730,1095,1460,1825)
      months <- c(365,548)
      month_names<-c("1yr","18m")
      Data_tradeline <- data.table(Data_tradeline)
      for (i in 1:length(months)){
        
        delq_monthly_flags = Data_tradeline[,j=list(
          Num_delq_5_plus_last = length(CRN[(diff_delq_5_plus_dt< months[i])])
          
        ),by=CRN]
        
        colnames(delq_monthly_flags) <- paste0(colnames(delq_monthly_flags),"_",month_names[i])
        colnames(delq_monthly_flags)[1] <- "CRN"
        
        BUREAU_TL_data_CRN_level <- merge(BUREAU_TL_data_CRN_level,delq_monthly_flags,by= "CRN",all.x = TRUE)
        
      }
      
      return (BUREAU_TL_data_CRN_level)
    }
    
    Tradeline_data_with_loan_type_monthly_var <- feature_engineering_loan_type_monthly_variables(Tradeline_data_with_DPD_var) #Trade level
    
    
    
    
    # ----------------------------------   6.  FEATURE ENGINEERING- Interest Rate + Restructured Flag + BANK /NON BANK VARIABLES  ----------------------------------------
    
    
    
    feature_engineering_new_variables <- function(Data_tradeline){
      
      data_Tradeline_data2 <-data.table(Data_tradeline) #rename data_Tradeline_data2 later 
      
      
      TL_data_CRN <-data_Tradeline_data2[,j=list(
        
        num_bank = length(CRN[(typeofcreditor=="BANKS")]),
        num_internal = length(CRN[(isInternalReportingMember==1)]),
        tot_outs_amt_external = sum(Out_standing_Balance[(isInternalReportingMember==0)],na.rm=TRUE),
        Amt_paid_off_bank = sum(Amt_paid_off[(typeofcreditor=="BANKS")],na.rm=TRUE),
        max_worstDPDBucket = max(worstDPDBucket,0),
        max_worstDPDBucket_CC = max(worstDPDBucket[(ACCOUNT_DESC_derived=="Credit Card")],0),
        Bureau_tenure_noBankBPR= max(diff_open_dt[(typeofcreditor=="Others")],0),
        tsl_bank = min(diff_open_dt[(typeofcreditor=="BANKS")],999999),
        tsl_noBankBPR = min(diff_open_dt[(typeofcreditor=="Others")],999999)
        
      ),by=CRN]
      
      return (TL_data_CRN)
      
    }
    
    Tradeline_data_with_new_var <- feature_engineering_new_variables(Tradeline_data_with_DPD_var) 
    
    
    #Merge all these features later and bring them to CRN level
    
    all_aggregate_feature_merge <- merge(Tradeline_data_with_loan_type_var,Tradeline_data_with_loan_type_trend_var,by= "CRN",all.x = TRUE)
    all_aggregate_feature_merge <- merge(all_aggregate_feature_merge,Tradeline_data_with_loan_type_monthly_var,by= "CRN",all.x = TRUE)
    all_aggregate_feature_merge <- merge(all_aggregate_feature_merge,Tradeline_data_with_new_var,by= "CRN",all.x = TRUE)
    
    ## load xgb SLIK
    
    model_xgb = model_xgb_SLIK
    xgb_importance = xgb_importance_SLIK
    
    #####     Model Wrapper Function #####
    options(scipen=999)
    Nmiss <- function(x)
    {
      sum(as.numeric(x) %in% c(-999999,999999))
    }
    
    tradeline_data = all_aggregate_feature_merge
    
    tradeline_data = as.data.frame(tradeline_data)
    
    var_imp <- xgb_importance$Feature
    
    var_list_xgb <- model_xgb$feature_names
    
    dd <- setdiff(var_list_xgb,var_imp)
    
    ## Make dummy variables for 'non important' 136 variables in model
    for (i in 1:length(dd)){
      tradeline_data[[dd[i]]] <- NA
    }
    
    ## Converting Test and Train in XGB
    dall2 <- xgb.DMatrix(data = data.matrix(tradeline_data[,var_list_xgb]))
    
    xgb_bureau <- predict(model_xgb,dall2)
    
    scored_data <- cbind(tradeline_data,xgb_bureau)
    
    #### PD Recode #####
    all_aggregate_feature_merge2 <- merge(all_aggregate_feature_merge,df_age2,by= "CRN",all.x = TRUE)
    all_aggregate_feature_merge2$Bureau_tenure_noBankBPR <- 0
    all_aggregate_feature_merge2$Num_BL_last_18m <- 0
    all_aggregate_feature_merge2$max_worstDPDBucket_CC2 <-all_aggregate_feature_merge2$max_worstDPDBucket_CC
    
    all_aggregate_feature_merge2$max_worstDPDBucket_CC <- ifelse(all_aggregate_feature_merge2$max_worstDPDBucket_CC2>2, 3,
                                                                 ifelse(all_aggregate_feature_merge2$max_worstDPDBucket_CC>0, all_aggregate_feature_merge2$max_worstDPDBucket_CC,
                                                                        ifelse(all_aggregate_feature_merge2$Age <=35,3,0))
                                                                 
    )
    
    # Recode Scoring
    tradeline_data = as.data.frame(all_aggregate_feature_merge2)
    
    ## Make dummy variables for 'non important' 136 variables in model
    for (i in 1:length(dd)){
      tradeline_data[[dd[i]]] <- NA
    }
    
    ## Converting Test and Train in XGB
    dall2 <- xgb.DMatrix(data = data.matrix(tradeline_data[,var_list_xgb]))
    
    xgb_bureau_recode <- predict(model_xgb,dall2)
    
    scored_data <- cbind(scored_data,xgb_bureau_recode)
    
    scored_data_bureau_final = scored_data[,c("CRN", "xgb_bureau","xgb_bureau_recode")]
    scored_data_bureau_final_slik <- scored_data_bureau_final
  }
  
  # save(scored_data_bureau_final_slik, file = "scored_data_SLIK_final.Rdata")
  return(scored_data_bureau_final_slik)
  
}

# -------------------------------------------------------------------------------------------------------------- #

#### 2. PEF1. FUNGSI UNTUK MEMBUAT TABEL PEFINDO ####

pefindo_tables <- function(raw_json_2){
  # apply fungsi ke raw json
  json_file <- simple_rapply(raw_json_2, function(x) if(is.null(x)) NA else x)
  
  ##### Flatten data json raw
  flat <- enframe(unlist(json_file))
  column_name <- flat$name
  flat_df <- as.data.frame(t(flat))
  colnames(flat_df) <- column_name
  rownames(flat_df) <- NULL
  
  ### Data frame raw dalam bentuk 1 baris banyak kolom
  flat_df <- flat_df[-c(1),]
  
  
  ############ 1. Buat CONTRACT PAYMENT CALENDAR MASTER table ####################
  
  # cek berapa facilities yang dimiliki dan lihat payment calendarnya per facility
  payment_df <- list()
  list_facility <- raw_json_2[['Contracts']][["ContractList"]][["Contract"]]
  
  # buat dataframe untuk tiap payment per facility dan digabung ke dalam sebuah list dataframes
  for(i in 1:dim(list_facility)[1]){
    payment_calendar <- list_facility[i,][["PaymentCalendarList"]][["CalendarItem"]][[1]]
    payment_calendar <- payment_calendar %>%
      mutate(contractcode = list_facility[i,][["ContractCode"]],
             ktp = raw_json_2$DemographicData$IDNumber,
             pefindoid = raw_json_2[["Parameters"]][["IDNumber"]],
             interest_rate_currency = InterestRate,
             interest_rate_value = InterestRate,
             interest_rate_local_value = InterestRate,
             contract_sequence_id = i,
             report_date = raw_json_2[["ReportInfo"]][["Created"]])%>%
      select(!InterestRate)
    payment_df[[i]]<- as.data.frame(as.list(payment_calendar))
  }
  
  # gabung semua dataframe payment per facility dan rapihkan nama kolomnya
  CONTRACT_PAYMENT_CALENDAR_MASTER <- do.call("rbind", payment_df)
  col_name <- colnames(CONTRACT_PAYMENT_CALENDAR_MASTER)
  col_name_clean <- str_replace_all(tolower(gsub("([a-z])([A-Z])","\\1_\\2",col_name)),fixed("."),"_")
  colnames(CONTRACT_PAYMENT_CALENDAR_MASTER) = col_name_clean

  ############ 2. Buat CONTRACT SUMMARY DEBTOR table ####################

  # ambil dari raw data contract summary
  contract_summary <- as_tibble(t(as.data.frame(unlist(as.list(raw_json_2[["ContractSummary"]][["Debtor"]])))))

  # buat dataframe
  CONTRACT_SUMMARY_DEBTOR <- contract_summary %>%
    mutate(ktp = raw_json_2$DemographicData$IDNumber,
           report_date = raw_json_2[["ReportInfo"]][["Created"]],
           pefindoid = raw_json_2[["Parameters"]][["IDNumber"]]) %>%
    select(!contains("Currency"))
  col_name <- colnames(CONTRACT_SUMMARY_DEBTOR)
  col_name_clean <- str_replace_all(tolower(gsub("([a-z])([A-Z])","\\1_\\2",col_name)),fixed("."),"_")
  colnames(CONTRACT_SUMMARY_DEBTOR) = col_name_clean

  ############ 3. Buat MASTER DATA table ####################

  # ambil bagian contract dari data jason raw
  X <- jsonlite::flatten(raw_json_2[["Contracts"]][["ContractList"]][["Contract"]])
  colnames(X) <- str_replace_all(tolower(gsub("([a-z])([A-Z])","\\1_\\2",colnames(X))),fixed("."),"_")

  # buat master data table
  MASTER_DATA <- X %>%
    select(start_date,maturity_date,contract_status,contract_type,role_of_client,creditor_type,
           creditor,phase_of_contract,outstanding_amount_local_value,past_due_amount_local_value,
           last_update,economic_sector,purpose_of_financing,credit_classification, real_end_date,
           initial_total_amount_local_value , total_amount_local_value,
           restructured_count, restructuring_date, restructuring_reason, last_interest_rate,
           initial_interest_rate) %>%
    mutate(ktp = raw_json_2$DemographicData$IDNumber,
           pefindoid = raw_json_2[["Parameters"]][["IDNumber"]],
           contract_sequence_id = rownames(X),
           report_date = raw_json_2[["ReportInfo"]][["Created"]])%>%
    dplyr::rename(reporting_memeber = creditor_type,
                  account_desc = contract_type,
                  ownership_type = role_of_client,
                  outstanding_balance = outstanding_amount_local_value,
                  overdue_amount = past_due_amount_local_value,
                  initial_limit = initial_total_amount_local_value,
                  limit = total_amount_local_value,
                  last_interest_rate_local_value = last_interest_rate,
                  initial_interest_rate_local_value = initial_interest_rate)


  ################# 4. Buat COLLA MASTER table ####################


  # ambil bagian collateral dari raw json data
  contract <- as.data.frame(raw_json_2[["Contracts"]][["ContractList"]][["Contract"]])
  contract_length <- nrow(contract)
  collateral = data.frame()

  # load nama kolom untuk collateral

  colla_colnames_a <- c(colla_colnames,"contract_sequence_id","contractcode")

  # save(colla_colnames, file ="collateral_columns.RData")
  for(i in 1:contract_length){
    colla <- jsonlite::flatten(as.data.frame(contract[i,][['CollateralsList']]))
    if(ncol(colla) == 1){

      # jika contract tidak punya collateral (list kosong), ubah menjadi list dengan nilai NA
      colla_1 <- simple_rapply(colla, function(x) if(is.na(x)|| x=="" || is.null(x)) NA else x)

      # buat dataframe kosong untuk menyimpan data collateral jika ada
      empty_df <- setNames(data.frame(matrix(ncol = length(colla_colnames_a), nrow = 0)), colla_colnames_a)
      for(i in 1:length(colla_1)){
        if(is.null(colla_1[i])){
          empty_df[i,] <- NA
        }
        else{
          empty_df[i,] <- jsonlite::flatten(as.data.frame(colla_1[i]))
        }
      }
      colla = empty_df
    }else{
      colnames(colla) <- str_replace_all(tolower(gsub("([a-z])([A-Z])","\\1_\\2",colnames(colla))),fixed("."),"_")
      colnames(colla) <- substring(colnames(colla),12)
      colla['contract_sequence_id'] = i
      colla['contractcode'] = contract[i,][['ContractCode']]
      if("bank_value" %in% colnames(colla)){
        colla <- colla %>%
          select(!bank_value)%>%
          mutate(bank_value_currency = NA,
                 bank_value_value = NA,
                 bank_value_local_value =NA)
      }else
      {
        colla = colla
      }
    }
    collateral <- rbind(collateral,colla)
  }
  
  if(!is.na(collateral[1,][["bank_valuation_date"]])){
    collateral <- collateral[rowSums(is.na(collateral)) != ncol(collateral), ]
  }else{
    collateral <- collateral
  }
  
  # buat dataframe
  COLLA_MASTER <- collateral %>%
    select(!c(bank_valuation_date,collateral_acceptance_date,collateral_rating
              ,main_address_address_line,valuation_date,collateral_description,collateral_owner_name))%>%
    mutate(
      ktp =raw_json_2$DemographicData$IDNumber,
      pefindoid = raw_json_2[["Parameters"]][["IDNumber"]],
      report_date = raw_json_2[["ReportInfo"]][["Created"]]
    )
  
  # save dataframe
  my_tables <- list(CONTRACT_PAYMENT_CALENDAR_MASTER,CONTRACT_SUMMARY_DEBTOR,MASTER_DATA,COLLA_MASTER)
  return(my_tables)
}

# -------------------------------------------------------------------------------------------------------------- #

#### 3. PEF2. FUNGSI MENGHITUNG PD SCORE MODUL BIRO PEFINDO ####

pdscore_bureau_pefindo <- function(CONTRACT_PAYMENT_CALENDAR_MASTER,CONTRACT_SUMMARY_DEBTOR,MASTER_DATA,
                                   COLLA_MASTER){
  options(scipen = 999)
  
  contract_pymntcalender = CONTRACT_PAYMENT_CALENDAR_MASTER
  rm(CONTRACT_PAYMENT_CALENDAR_MASTER)
  
  contract_pymntcalender$date2 <- strptime(as.character(contract_pymntcalender$date), "%Y-%m-%d")
  contract_pymntcalender$date_perf <- format(as.Date(contract_pymntcalender$date2, format = "%Y-%m-%d"),"%Y-%m-%d")
  contract_pymntcalender$date2 = NULL
  contract_pymntcalender$date = NULL
  
  contract_pymntcalender$report_date2 <- strptime(as.character(contract_pymntcalender$report_date), "%Y-%m-%d")
  contract_pymntcalender$report_date = NULL
  contract_pymntcalender$report_date <- format(as.Date(contract_pymntcalender$report_date2, format = "%Y-%m-%d"),"%Y-%m-%d")
  contract_pymntcalender$report_date2 = NULL
  
  table(contract_pymntcalender$report_date)
  
  contract_pymntcalender_filter <- contract_pymntcalender %>% 
    dplyr::mutate(bucket= ifelse(past_due_days == 0, '1', 
                                 ifelse(past_due_days <= 30, '2',
                                        ifelse(past_due_days <= 60, '3',
                                               ifelse(past_due_days <= 90, '4',
                                                      ifelse(past_due_days > 90, '5', '0'))))))
  
  contract_pymntcalender_filter <- contract_pymntcalender_filter %>% 
    dplyr::mutate(bucket = ifelse(is.na(bucket) == TRUE, '0', bucket),
                  past_due_days = ifelse(is.na(past_due_days) == TRUE, 0, past_due_days),
                  outstanding_amount_local_value = ifelse(is.na(outstanding_amount_local_value) == TRUE, 0, outstanding_amount_local_value),
                  past_due_amount_local_value = ifelse(is.na(past_due_amount_local_value) == TRUE, 0, past_due_amount_local_value))
  
  contract_pymntcalender_filter <- contract_pymntcalender_filter %>% 
    dplyr::mutate(kolek= ifelse(negative_status_of_contract == "NotSpecified", '0', 
                                ifelse(negative_status_of_contract == "NoNegativeStatus", '1',
                                       ifelse(negative_status_of_contract == "Monitored", '2',
                                              ifelse(negative_status_of_contract == "Substandard", '3',
                                                     ifelse(negative_status_of_contract == "Doubtful", '4','5'))))),
                  Write_off_amt= ifelse(past_due_days > 180, outstanding_amount_local_value,0),
                  Write_off_status = ifelse(past_due_days > 180, 1,0)) %>% 
    dplyr:: arrange(ktp, report_date, pefindoid, contract_sequence_id, desc(date_perf))
  
  contract_pymntcalender2 = subset(contract_pymntcalender_filter, select=c(ktp,report_date, pefindoid, contract_sequence_id, date_perf, bucket, kolek, past_due_days,outstanding_amount_local_value, past_due_amount_local_value, interest_rate_local_value, Write_off_amt, Write_off_status))
  
  #1 pefindo id bisa punya sequence id yang sama, di cek nama bank , os, limit, dll sama, perlu nodup
  contract_pymntcalender2 = contract_pymntcalender2[!duplicated(contract_pymntcalender2[c('ktp','report_date','pefindoid', 'contract_sequence_id', 'date_perf')]),]
  
  
  # get data banyak fasilitas tiap pefindo id, pilih pefindo id yang banyak fasilitas
  
  contract_sum = CONTRACT_SUMMARY_DEBTOR
  rm(CONTRACT_SUMMARY_DEBTOR)
  
  contract_sum$report_date2 <- strptime(as.character(contract_sum$report_date), "%Y-%m-%d")
  contract_sum$report_date = NULL
  contract_sum$report_date <- format(as.Date(contract_sum$report_date2, format = "%Y-%m-%d"),"%Y-%m-%d")
  contract_sum$report_date2 = NULL
  
  contract_sum$open_contracts = as.numeric(contract_sum$open_contracts)
  contract_sum$closed_contracts = as.numeric(contract_sum$closed_contracts)
  
  contract_sum <- contract_sum %>% 
    dplyr::mutate(n_fac= closed_contracts + open_contracts )
  
  #ambil pefindo id yang banyak fasilitas, terdapat 1 ktp, beberppa pefindoid, beda report date
  contract_sum_filter <- contract_sum %>% 
    dplyr:: arrange(ktp, desc(n_fac))
  
  
  contract_sum_nod = contract_sum_filter[!duplicated(contract_sum_filter[c('ktp')]),]
  
  
  #gabung dengan data payment calender, ambil 1 pefindo yang banyak fasilitas
  #1 ktp bisa beberapa pefindoid 
  
  contract_pymntcalender2a = contract_pymntcalender2[contract_pymntcalender2$pefindoid %in% contract_sum_nod$pefindoid, ]
  
  contract_pymntcalender2a <- contract_pymntcalender2a %>% 
    dplyr:: arrange(ktp, report_date, pefindoid, contract_sequence_id, desc(date_perf))
  
  
  ######## get master data
  
  master = MASTER_DATA
  rm(MASTER_DATA)
  
  
  master$start_date2 <- strptime(as.character(master$start_date), "%Y-%m-%d")
  master$date_open <- format(as.Date(master$start_date2, format = "%Y-%m-%d"),"%Y-%m-%d")
  master$start_date2 = NULL
  
  master$maturity_date2 <- strptime(as.character(master$maturity_date), "%Y-%m-%d")
  master$date_maturity <- format(as.Date(master$maturity_date2, format = "%Y-%m-%d"),"%Y-%m-%d")
  master$maturity_date2 = NULL
  
  master$last_update2 <- strptime(as.character(master$last_update), "%Y-%m-%d")
  master$last_update3 <- format(as.Date(master$last_update2, format = "%Y-%m-%d"),"%Y-%m-%d")
  master$last_update2 = NULL
  
  master$real_end_date2 <- strptime(as.character(master$real_end_date), "%Y-%m-%d")
  master$date_close <- format(as.Date(master$real_end_date2, format = "%Y-%m-%d"),"%Y-%m-%d")
  master$real_end_date2 = NULL
  
  master$report_date2 <- strptime(as.character(master$report_date), "%Y-%m-%d")
  master$report_date = NULL
  master$report_date <- format(as.Date(master$report_date2, format = "%Y-%m-%d"),"%Y-%m-%d")
  master$report_date2 = NULL
  
  
  master$last_update = NULL
  master$maturity_date = NULL
  master$start_date = NULL
  master$real_end_date = NULL
  
  #1 pefindo id bisa punya sequence id yang sama, di cek nama bank , os, limit, dll sama, perlu nodup
  master_a = master[!duplicated(master[c('ktp','report_date','pefindoid', 'contract_sequence_id')]),]
  
  #get pefindoid that have many facility, 1 ktp have many pefindoid 
  master_b = master_a[master_a$pefindoid %in% contract_sum_nod$pefindoid, ]
  
  master_b <- master_b %>% 
    dplyr::mutate(reporting_member_upd= ifelse(str_detect(creditor, "OCBC NISP") == "TRUE", 'Internal',
                                               ifelse(str_detect(creditor, "NISP") == "TRUE", 'Internal',
                                                      ifelse(str_detect(creditor, "nisp") == "TRUE", 'Internal',
                                                             ifelse(str_detect(creditor, "ocbc nisp") == "TRUE", 'Internal', 'Eksternal')))),
                  account_desc_derive= ifelse(master_b$account_desc=='CreditCard' & master_b$purpose_of_financing =='OtherConsumerCredit','Credit Card',
                                              ifelse(master_b$account_desc=='ConsumerCredit' & master_b$purpose_of_financing =='OtherConsumerCredit' ,'Unsecured Loan',
                                                     ifelse(master_b$account_desc=='OtherWithLoanAgreement' & master_b$purpose_of_financing =='MortgageBetween21To70SQM','Mortgage Loan',
                                                            ifelse(master_b$account_desc=='OtherWithLoanAgreement' & master_b$purpose_of_financing =='MortgageBasicHousingReadyPlot','Mortgage Loan',
                                                                   ifelse(master_b$account_desc=='OtherWithLoanAgreement' & master_b$purpose_of_financing =='MortgageAbove70SQM','Mortgage Loan',
                                                                          ifelse(master_b$account_desc=='OtherWithLoanAgreement' & master_b$purpose_of_financing =='HomeRepair','Mortgage Loan',
                                                                                 ifelse(master_b$account_desc=='OtherWithLoanAgreement' & master_b$purpose_of_financing =='MortgageUpTo21SQM','Mortgage Loan',
                                                                                        ifelse(master_b$account_desc=='Murabahah' & master_b$purpose_of_financing =='ShopHouseCredit','Mortgage Loan', 
                                                                                               ifelse(master_b$account_desc=='OtherWithLoanAgreement' & master_b$purpose_of_financing =='OtherConsumerCredit' & master_b$economic_sector == 'ID_9962','Auto Loan', 
                                                                                                      ifelse(master_b$account_desc=='OtherWithLoanAgreement' & master_b$purpose_of_financing =='OtherConsumerCredit' & master_b$economic_sector == 'ID_9960','Auto Loan', 'Others')))))))))))
  #gabung contract payment calender with master base to get last update performance  917,052
  
  contract_pymntcalender2a$contract_sequence_id = as.character(contract_pymntcalender2a$contract_sequence_id)
  
  contract_pymntcalender_cif_filter <- contract_pymntcalender2a %>% 
    dplyr::left_join(select(master_b, report_date,ktp,pefindoid, contract_sequence_id, last_update3),
                     by = c("ktp", "report_date","pefindoid", "contract_sequence_id")) %>% 
    dplyr:: arrange(ktp, report_date, pefindoid, contract_sequence_id)
  
  
  contract_pymntcalender_cif_filter$last_update4 <- timeLastDayInMonth(contract_pymntcalender_cif_filter$last_update3)
  
  contract_pymntcalender_cif_filter$last_update3 = NULL
  contract_pymntcalender_cif_filter$last_update3 = contract_pymntcalender_cif_filter$last_update4
  contract_pymntcalender_cif_filter$last_update4 = NULL
  
  contract_pymntcalender_cif_filter$last_update3 = as.Date(contract_pymntcalender_cif_filter$last_update3)
  
  contract_pymntcalender_cif_filter2 = contract_pymntcalender_cif_filter %>%
    dplyr::filter(date_perf <= last_update3)  %>% 
    dplyr:: arrange(ktp, report_date, pefindoid, contract_sequence_id, desc(date_perf))
  
  contract_pymntcalender_cif_filter2 = contract_pymntcalender_cif_filter2 %>%
    dplyr::group_by(ktp, report_date, pefindoid, contract_sequence_id) %>% 
    dplyr::mutate(no = row_number())
  
  #get max dateperf  
  getmaxdateperf <- contract_pymntcalender_cif_filter2 %>% 
    dplyr::mutate(date_perf = ifelse(is.na(date_perf) == TRUE, '0', date_perf),
                  interest_rate_local_value = ifelse(is.na(interest_rate_local_value) == TRUE, 0, interest_rate_local_value))
  
  
  get_max_perf = getmaxdateperf %>%
    dplyr::group_by(ktp,report_date,pefindoid, contract_sequence_id) %>% 
    dplyr::summarise(pymntmaxdate = max(date_perf),
                     interestrate_max = max(interest_rate_local_value))
  
  #gabung base data payment calender with max perf date  
  contract_pymntcalender_cif_filter2 <- contract_pymntcalender_cif_filter2 %>% 
    dplyr::left_join(get_max_perf, by = c("ktp", "report_date","pefindoid", "contract_sequence_id")) 
  
  #get os last pymnt date
  
  contract_pymntcalender_cif_filter2 <- contract_pymntcalender_cif_filter2 %>% 
    dplyr::mutate(Write_off_amt = ifelse(is.na(Write_off_amt) == TRUE, 0, Write_off_amt))
  
  last_os_pastdue = contract_pymntcalender_cif_filter2[!duplicated(contract_pymntcalender_cif_filter2[c('ktp','report_date','pefindoid', 'contract_sequence_id')]),]
  
  #gabung base data  master with max perf date 
  master_cif_filter_a <- master_b %>% 
    dplyr::left_join(get_max_perf, by = c("ktp", "report_date", "pefindoid", "contract_sequence_id")) 
  
  
  #gabung dngan last os 
  master_cif_filter_b <- master_cif_filter_a %>% 
    dplyr::left_join(select(last_os_pastdue,outstanding_amount_local_value,
                            past_due_amount_local_value, interest_rate_local_value, report_date,ktp,pefindoid, contract_sequence_id), 
                     by = c("ktp", "report_date", "pefindoid", "contract_sequence_id")) 
  
  master_cif_filter_b <- master_cif_filter_b %>% 
    dplyr::mutate(outstanding_amount_local_value = ifelse(is.na(outstanding_amount_local_value) == TRUE, '0', outstanding_amount_local_value),
                  past_due_amount_local_value = ifelse(is.na(past_due_amount_local_value) == TRUE, '0', past_due_amount_local_value))
  
  master_cif_filter_b <- master_cif_filter_b %>% 
    dplyr::mutate(last_update_derive = pymntmaxdate, 
                  last_update = last_update3,
                  outstanding_lastpymnt = outstanding_amount_local_value , 
                  pastdueamt_lastpymnt  = past_due_amount_local_value)  
  
  
  master_cif_filter_b$idno = NULL
  master_cif_filter_b$last_update3 = NULL
  master_cif_filter_b$pymntmaxdate = NULL
  master_cif_filter_b$outstanding_amount_local_value = NULL
  master_cif_filter_b$past_due_amount_local_value = NULL
  master_cif_filter_b$last_update = NULL
  master_cif_filter_b$interest_rate_local_value = NULL
  master_cif_filter_b$interestrate_max = NULL
  
  
  master_cif_filter_b <- master_cif_filter_b %>% 
    dplyr::mutate(initial_limit = ifelse(is.na(initial_limit) == TRUE, 0, initial_limit),
                  limit = ifelse(is.na(limit) == TRUE, 0, limit),
                  SettlementAmount = ifelse(phase_of_contract == "Closed", outstanding_lastpymnt, 0))
  
  master_cif_filter_b <- master_cif_filter_b %>% 
    dplyr::mutate(limit_use= limit,
                  last_update_derive = ifelse(is.na(last_update_derive) == TRUE, 0, last_update_derive),
                  date_close = ifelse(is.na(date_close) == TRUE, 0, date_close),
                  SettlementAmount = ifelse(is.na(SettlementAmount) == TRUE, 0, SettlementAmount))
  
  master_cif_filter_b <- master_cif_filter_b %>% 
    dplyr::mutate(type_of_creditor= ifelse(master_cif_filter_b$reporting_memeber == "Banks", 'Banks',
                                           ifelse(master_cif_filter_b$reporting_memeber == "BPR", 'BPR',
                                                  ifelse(master_cif_filter_b$reporting_memeber == "NonBankingFinancialInstitutions", 'NonBank', 'NA'))))
  
  
  #######################  JOIN DATA
  
  #make template 36 (pefindo have 36 historical)
  templat_a <- data.frame(no = 1:36)
  
  #merge master with template 36 row   646,416
  master_cif_filter_b_tmplt= merge(x = master_cif_filter_b, y = templat_a, all = TRUE)
  
  master_cif_filter_b_tmplt <- master_cif_filter_b_tmplt %>% 
    dplyr:: arrange(ktp, report_date, pefindoid, contract_sequence_id, no)
  
  #merge data contract master with paymnet calender monthly   646,416
  master_pymntmonthly <- master_cif_filter_b_tmplt %>% 
    dplyr::left_join(contract_pymntcalender_cif_filter2, by = c("ktp", "report_date","pefindoid", "contract_sequence_id","no")) %>% 
    dplyr:: arrange(ktp, report_date, pefindoid, contract_sequence_id, no)
  
  master_pymntmonthly <- master_pymntmonthly %>% 
    dplyr::mutate(bucket = ifelse(is.na(bucket) == TRUE, '0', bucket),
                  kolek = ifelse(is.na(kolek) == TRUE, '0', kolek))
  
  master_pymntmonthly <- master_pymntmonthly %>%
    dplyr::mutate(date_perf = ifelse(is.na(date_perf) == TRUE, 'not_specified', date_perf),
                  Write_off_amt = ifelse(is.na(Write_off_amt) == TRUE, 0, Write_off_amt),
                  Write_off_status = ifelse(is.na(Write_off_status) == TRUE, 0, Write_off_status))
  
  #
  get_min_perf = master_pymntmonthly %>%
    dplyr::group_by(ktp, report_date,pefindoid, contract_sequence_id) %>% 
    dplyr::summarise(pymntmindate = min(date_perf),
                     write_off_amt_sum = sum(Write_off_amt),
                     write_off_status = max(Write_off_status))
  
  #gabung dengan data master  
  master_cif_filter_c <- master_cif_filter_b %>%
    dplyr::left_join(get_min_perf, by = c("ktp", "report_date",  "pefindoid", "contract_sequence_id"))
  
  
  
  #1 pefindo id punya sequence id yang sama, ddicek limit os dan nama bank sama, nodup
  getkolek = master_pymntmonthly %>%
    dplyr::group_by(ktp, report_date,pefindoid, contract_sequence_id, no) %>% 
    dplyr::summarise(num_fac = n(), 
                     kolek_max = max(kolek), 
                     bucket_max = max(bucket))
  
  getkolek$no2 = paste0("_", getkolek$no)
  getkolek_a <- getkolek %>%
    dplyr::mutate(kolek_max = ifelse((kolek_max) == '0', 'x', kolek_max),
                  bucket_max = ifelse((bucket_max) == '0', 'x', bucket_max))
  
  #transpose kolek, 1 - date terbaru
  kolek_T = subset(getkolek_a, select=c(ktp, report_date,pefindoid, contract_sequence_id, no2, kolek_max))
  kolek_t2 = cast(kolek_T, ktp+report_date+pefindoid+contract_sequence_id~no2)
  kolek_t2$paymenthistorykolek1 =paste(kolek_t2$`_1`,kolek_t2$`_2`,kolek_t2$`_3`, kolek_t2$`_4`,
                                       kolek_t2$`_5`, kolek_t2$`_6`, kolek_t2$`_7`, kolek_t2$`_8`,
                                       kolek_t2$`_9`, kolek_t2$`_10`, kolek_t2$`_11`, kolek_t2$`_12`,sep = "")
  kolek_t2$paymenthistorykolek2 =paste(kolek_t2$`_13`,kolek_t2$`_14`,kolek_t2$`_15`, kolek_t2$`_16`,
                                       kolek_t2$`_17`, kolek_t2$`_18`, kolek_t2$`_19`, kolek_t2$`_20`,
                                       kolek_t2$`_21`, kolek_t2$`_22`, kolek_t2$`_23`, kolek_t2$`_24`,sep = "")
  kolek_t2$paymenthistorykolek3 =paste(kolek_t2$`_25`,kolek_t2$`_26`,kolek_t2$`_27`, kolek_t2$`_28`,
                                       kolek_t2$`_29`, kolek_t2$`_30`, kolek_t2$`_31`, kolek_t2$`_32`,
                                       kolek_t2$`_33`, kolek_t2$`_34`, kolek_t2$`_35`, kolek_t2$`_36`,sep = "")
  kolek_t3 <- kolek_t2 %>%
    dplyr::select(c("ktp", "report_date", "pefindoid", "contract_sequence_id", "paymenthistorykolek1", "paymenthistorykolek2", "paymenthistorykolek3"))
  
  bucket_t = subset(getkolek_a, select=c(ktp, report_date,pefindoid, contract_sequence_id, no2, bucket_max))
  bucket_t2 = cast(bucket_t, ktp+report_date+pefindoid+contract_sequence_id~no2)
  bucket_t2$paymenthistorybucket1 =paste(bucket_t2$`_1`,bucket_t2$`_2`,bucket_t2$`_3`, bucket_t2$`_4`,
                                         bucket_t2$`_5`, bucket_t2$`_6`, bucket_t2$`_7`, bucket_t2$`_8`,
                                         bucket_t2$`_9`, bucket_t2$`_10`, bucket_t2$`_11`, bucket_t2$`_12`,sep = "")
  bucket_t2$paymenthistorybucket2 =paste(bucket_t2$`_13`,bucket_t2$`_14`,bucket_t2$`_15`, bucket_t2$`_16`,
                                         bucket_t2$`_17`, bucket_t2$`_18`, bucket_t2$`_19`, bucket_t2$`_20`,
                                         bucket_t2$`_21`, bucket_t2$`_22`, bucket_t2$`_23`, bucket_t2$`_24`,sep = "")
  bucket_t2$paymenthistorybucket3 =paste(bucket_t2$`_25`,bucket_t2$`_26`,bucket_t2$`_27`, bucket_t2$`_28`,
                                         bucket_t2$`_29`, bucket_t2$`_30`, bucket_t2$`_31`, bucket_t2$`_32`,
                                         bucket_t2$`_33`, bucket_t2$`_34`, bucket_t2$`_35`, bucket_t2$`_36`,sep = "")
  bucket_t3 <- bucket_t2 %>%
    dplyr::select(c("ktp", "report_date", "pefindoid", "contract_sequence_id", "paymenthistorybucket1", "paymenthistorybucket2", "paymenthistorybucket3"))
  
  gabung_bucket_kolek <- bucket_t3 %>%
    dplyr::left_join(kolek_t3, by = c("ktp", "report_date", "pefindoid", "contract_sequence_id"))
  
  #gabung base data with kolek bucket
  master_cif_filter_d <- master_cif_filter_c %>%
    dplyr::left_join(gabung_bucket_kolek, by = c("ktp", "report_date", "pefindoid", "contract_sequence_id"))
  
  master_cif_filter_d$restructuring_date2 <- strptime(as.character(master_cif_filter_d$restructuring_date), "%Y-%m-%d")
  master_cif_filter_d$restructuring_date = NULL
  master_cif_filter_d$restructuring_date <- format(as.Date(master_cif_filter_d$restructuring_date2, format = "%Y-%m-%d"),"%Y-%m-%d")
  master_cif_filter_d$restructuring_date2 = NULL
  
  
  master_cif_filter_d_nod = master_cif_filter_d[!duplicated(master_cif_filter_d[c('ktp')]),]
  
  
  master_cif_filter_d$status_Loan <- ifelse(master_cif_filter_d$phase_of_contract == "Open","Live", "Closed")
  
  ##########################  get collateral data
  
  colla = COLLA_MASTER
  rm(COLLA_MASTER)
  
  
  colla$report_date2 <- strptime(as.character(colla$report_date), "%Y-%m-%d")
  colla$report_date = NULL
  colla$report_date <- format(as.Date(colla$report_date2, format = "%Y-%m-%d"),"%Y-%m-%d")
  colla$report_date2 = NULL
  
  
  colla$CRN <-  colla$ktp
  
  colla_filter_again = colla[!duplicated(colla[c('ktp','report_date','pefindoid', 'contract_sequence_id', 'proof_of_ownership')]),]
  
  master_cif_filter_d$contract_sequence_id <- as.integer(master_cif_filter_d$contract_sequence_id)
  
  #gabung with data base  
  
  
  master_colla_again <- master_cif_filter_d %>% 
    dplyr::inner_join(colla_filter_again, by = c("ktp", "report_date", "pefindoid", "contract_sequence_id"))
  
  
  master_colla_again2 = master_colla_again[!is.na(master_colla_again$CRN),]
  
  #tambahan, krna di data asli, tdk ada collateral type blank
  master_colla_again2 = master_colla_again[!is.na(master_colla_again$collateral_type),]
  
  
  feature_engineering_loan_type_variables <- function(Data_tradeline){
    
    master_colla_again2 <- Data_tradeline  
    master_colla_again2 <- as.data.frame(master_colla_again2)
    
    data_Tradeline_data2 <-data.table(Data_tradeline) #rename data_Tradeline_data2 later 
    
    TL_data_CRN <-data_Tradeline_data2[,j=list(
      num_colla = .N,
      num_colla_inventory = length(CRN[(collateral_type=="Inventory")]),
      num_colla_Insured = length(CRN[(insurance == "Insured")]),
      
      
      num_colla_NonCashFixedAsset_insured = length(CRN[((collateral_type=="NonCashFixedAsset")&(insurance=="Insured"))]),
      
      
      #individual
      num_colla_Insured_individual  = length(CRN[((insurance=="Insured")&(ownership_type=="MainDebtor"))]),
      
      
      
      
      #Live
      num_colla_Insured_Live  = length(CRN[((insurance=="Insured")&(phase_of_contract=="Open"))]),
      num_colla_no_shared_Live  = length(CRN[((is_shared=="No")&(phase_of_contract=="Open"))]),
      
      num_colla_Other_noinsured_Live  = length(CRN[((collateral_type=="Other")&(insurance=="Uninsured")&(phase_of_contract=="Open"))]),
      
      
      
      
      
      #Banks
      num_colla_no_shared_Banks  = length(CRN[((is_shared=="No")&(type_of_creditor=="Banks"))]),
      num_colla_Other_noshared_Banks  = length(CRN[((collateral_type=="Other")&(is_shared=="No")&(type_of_creditor=="Banks"))]),
      
      #NonBank
      num_colla_no_shared_NonBank  = length(CRN[((is_shared=="No")&(type_of_creditor=="NonBank"))])
      
    ),by=CRN]
    
    # TL_data_CRN[,Utilization:=ifelse((TL_data_CRN$Sanctioned_Amt_CC==0|is.na(TL_data_CRN$Sanctioned_Amt_CC)),0,TL_data_CRN$tot_outs_amt_CC/TL_data_CRN$Sanctioned_Amt_CC)]
    
    return (TL_data_CRN)
    
  }
  
  Tradeline_data_with_colla <- feature_engineering_loan_type_variables(master_colla_again2) 
  
  
  Tradeline_data_with_colla_keep = Tradeline_data_with_colla[,c("CRN", "num_colla_no_shared_Banks", "num_colla_Insured_Live", "num_colla_no_shared_NonBank",
                                                                "num_colla_Insured_individual", "num_colla_Insured", "num_colla_NonCashFixedAsset_insured",
                                                                "num_colla_no_shared_Live", "num_colla_Other_noshared_Banks", "num_colla_Other_noinsured_Live"
  )]
  
  
  ###################################  get feature delq, adn facility
  
  Tradeline_data <-  master_cif_filter_d
  
  Tradeline_data$CC_petty_bal_thresh <-  300000
  Tradeline_data$Other_loans_petty_bal_thresh <-  300000
  Tradeline_data$BCC_DATE <- Tradeline_data$last_update
  Tradeline_data$PaymentHistoryStartDate <- Tradeline_data$last_update
  Tradeline_data$PaymentHistoryEndDate <- Tradeline_data$pymntmindate 
  Tradeline_data$DateReported <- Tradeline_data$last_update
  Tradeline_data$DATE_OPENED <- Tradeline_data$date_open
  Tradeline_data$DateOfLastPayment <- Tradeline_data$last_update_derive
  Tradeline_data$DATE_CLOSED <- Tradeline_data$date_close
  Tradeline_data$Ownership_type <- Tradeline_data$ownership_type
  Tradeline_data$ACCOUNT_DESC <- Tradeline_data$account_desc 
  Tradeline_data$HighCreditSanctionedAmount <- Tradeline_data$limit_use
  Tradeline_data$PaymentHistory <- paste0(Tradeline_data$paymenthistorybucket1, Tradeline_data$paymenthistorybucket2, Tradeline_data$paymenthistorybucket3)
  Tradeline_data$WrittenOffAmountTotal = Tradeline_data$write_off_amt_sum
  Tradeline_data$writeoff_status = Tradeline_data$write_off_status
  Tradeline_data$CRN <-  Tradeline_data$ktp
  Tradeline_data$Reference_date <- Tradeline_data$report_date
  Tradeline_data$ACCOUNT_DESC_derived <- Tradeline_data$account_desc_derive
  
  
  Tradeline_data$account_desc_derive = NULL
  Tradeline_data$report_date = NULL
  #Tradeline_data$cifno = NULL
  Tradeline_data$write_off_amt_sum = NULL
  Tradeline_data$limit_use = NULL
  Tradeline_data$account_desc  = NULL
  Tradeline_data$ownership_type = NULL
  Tradeline_data$date_close = NULL
  Tradeline_data$last_update_derive = NULL
  Tradeline_data$date_open = NULL
  Tradeline_data$pymntmindate  = NULL
  Tradeline_data$limit  = NULL
  Tradeline_data$initial_limit  = NULL
  Tradeline_data$reporting_memeber  = NULL
  Tradeline_data$creditor  = NULL
  
  #Tradeline_data2 = Tradeline_data[,]
  table(Tradeline_data$ACCOUNT_DESC_derived)
  
  
  #cek double data in pefindo data  32,901
  Tradeline_data2<-Tradeline_data[!duplicated(Tradeline_data[c("ktp","Reference_date","pefindoid","contract_sequence_id")]),]
  Tradeline_data2$ReportingMemberShortName_derived <- Tradeline_data2$reporting_member_upd   
  
  
  Tradeline_data2$Out_standing_Balance = Tradeline_data2$outstanding_balance
  Tradeline_data2$Out_standing_Balance = as.numeric(Tradeline_data2$Out_standing_Balance)
  
  
  Tradeline_data2$Amount<- ifelse((Tradeline_data2$ACCOUNT_DESC_derived == "Credit Card") ,Tradeline_data2$Out_standing_Balance,Tradeline_data2$HighCreditSanctionedAmount)
  Tradeline_data2$Amount <- ifelse(is.na(Tradeline_data2$Amount),0,Tradeline_data2$Amount)
  Tradeline_data2$Amount = as.numeric(Tradeline_data2$Amount)
  
  Tradeline_data2$Over_due_amount = Tradeline_data2$overdue_amount
  
  Tradeline_data2$Over_due_amount <- ifelse(is.na(Tradeline_data2$Over_due_amount),0,Tradeline_data2$Over_due_amount)
  Tradeline_data2$Over_due_amount = as.numeric(Tradeline_data2$Over_due_amount)
  
  
  Tradeline_data2$Amt_paid_off  <- Tradeline_data2$Amount- Tradeline_data2$Out_standing_Balance
  Tradeline_data2$Amt_paid_off = as.numeric(Tradeline_data2$Amt_paid_off)
  
  Tradeline_data2$worstDPDBucket = ifelse(grepl("5", Tradeline_data2$PaymentHistory)==TRUE,5,
                                          ifelse(grepl("4", Tradeline_data2$PaymentHistory)==TRUE,4,
                                                 ifelse(grepl("3", Tradeline_data2$PaymentHistory)==TRUE,3,
                                                        ifelse(grepl("2", Tradeline_data2$PaymentHistory)==TRUE,2,1))))
  
  
  
  
  #---------------------------------      2. FEATURE ENGINEERING- CREATING DPD HISTORY VARIABLES    --------------------------------------------------
  
  feature_engineering_DPD_variables <- function(Data_tradeline){
    
    Tradeline_data2 <- Data_tradeline  
    Tradeline_data2 <- as.data.frame(Tradeline_data2)
    
    # Creating flags
    
    #bucket 90+
    Tradeline_data2$index_90_plus<-regexpr("5", Tradeline_data2$PaymentHistory)
    Tradeline_data2$delq_90_plus_flag <- ifelse(Tradeline_data2$index_90_plus == -1, "", 1)
    Tradeline_data2$N_90<-str_count(Tradeline_data2$PaymentHistory, "5")
    
    #bucket 60+
    Tradeline_data2$index_60_plus<-regexpr("4", Tradeline_data2$PaymentHistory)
    Tradeline_data2$delq_60_plus_flag <- ifelse(Tradeline_data2$index_60_plus == -1, "", 1)
    Tradeline_data2$N_60<-str_count(Tradeline_data2$PaymentHistory, "4")
    
    
    #bucket 30+
    Tradeline_data2$index_30_plus<-regexpr("3", Tradeline_data2$PaymentHistory)
    Tradeline_data2$delq_30_plus_flag <- ifelse(Tradeline_data2$index_30_plus == -1, "", 1)
    Tradeline_data2$N_30<-str_count(Tradeline_data2$PaymentHistory, "3")
    
    #bucket 1+
    Tradeline_data2$index_5_plus<-regexpr("2", Tradeline_data2$PaymentHistory)
    Tradeline_data2$delq_5_plus_flag <- ifelse(Tradeline_data2$index_5_plus == -1, "", 1)
    Tradeline_data2$N_5<-str_count(Tradeline_data2$PaymentHistory, "2")
    
    #Creating delinquency flag -> Anyone who has gone either 5+DPD,or 30+DPD,or 60+DPD,or 90+DPD ever- is tagged as delinquent
    
    Tradeline_data2$delq_flag<-ifelse(is.na(as.numeric(apply(Tradeline_data2[,c("delq_5_plus_flag","delq_30_plus_flag","delq_60_plus_flag","delq_90_plus_flag")],1,max))),0,1)
    
    #ganti ke 31, jika pake 30, untuk tgl2 31 akan meleset, hasilnya akan tgl 1 bulan depan dr bulan seharusnya
    Tradeline_data2$delq_dt_90_plus<-ifelse(Tradeline_data2$delq_90_plus_flag=="",
                                            "",
                                            ifelse(Tradeline_data2$index_90_plus==1,
                                                   as.character(Tradeline_data2$PaymentHistoryStartDate),
                                                   as.character(ymd(as.Date(Tradeline_data2$PaymentHistoryStartDate,"%Y-%m-%d"))-((as.numeric(Tradeline_data2$index_90_plus)-1)*31))))
    
    Tradeline_data2$delq_dt_60_plus<-ifelse(Tradeline_data2$delq_60_plus_flag=="",
                                            "",
                                            ifelse(Tradeline_data2$index_60_plus==1,
                                                   as.character(Tradeline_data2$PaymentHistoryStartDate),
                                                   as.character(ymd(as.Date(Tradeline_data2$PaymentHistoryStartDate,"%Y-%m-%d"))-((as.numeric(Tradeline_data2$index_60_plus)-1)*31))))
    Tradeline_data2$delq_dt_30_plus<-ifelse(Tradeline_data2$delq_30_plus_flag=="",
                                            "",
                                            ifelse(Tradeline_data2$index_30_plus==1,
                                                   as.character(Tradeline_data2$PaymentHistoryStartDate),
                                                   as.character(ymd(as.Date(Tradeline_data2$PaymentHistoryStartDate,"%Y-%m-%d"))-((as.numeric(Tradeline_data2$index_30_plus)-1)*31))))
    Tradeline_data2$delq_dt_5_plus<-ifelse(Tradeline_data2$delq_5_plus_flag=="",
                                           "",
                                           ifelse(Tradeline_data2$index_5_plus==1,
                                                  as.character(Tradeline_data2$PaymentHistoryStartDate),
                                                  as.character(ymd(as.Date(Tradeline_data2$PaymentHistoryStartDate,"%Y-%m-%d"))-((as.numeric(Tradeline_data2$index_5_plus)-1)*31))))
    
    
    # Missing value imputation for DATE_OPENED {
    
    Tradeline_data_null_opened_dt <- Tradeline_data2[is.na(Tradeline_data2$DATE_OPENED),] # Null trade open date
    
    # If DATE_OPENED is null, but PaymentHistoryEndDate is not null, impute DATE_OPENED with PaymentHistoryEndDate, otherwise by DateReported
    Tradeline_data_null_opened_dt$DATE_OPENED<-as.Date(ifelse(is.na(Tradeline_data_null_opened_dt$PaymentHistoryEndDate),
                                                              Tradeline_data_null_opened_dt$DateReported,
                                                              Tradeline_data_null_opened_dt$PaymentHistoryEndDate),"%Y-%m-%d",origin = "1970-01-01")
    
    Tradeline_data_not_null_opened_dt <- Tradeline_data2[!is.na(Tradeline_data2$DATE_OPENED),]
    Tradeline_data2<- rbind(Tradeline_data_null_opened_dt,Tradeline_data_not_null_opened_dt) #Merging to get all records with mon null DATE_OPENED
    
    #                                           }
    
    #Finding difference between Reference_date and DATE_OPENED
    Tradeline_data2$diff_open_dt <- round(as.numeric(difftime(Tradeline_data2$Reference_date,Tradeline_data2$DATE_OPENED,units = "days")))
    
    
    
    ## Difference between the reference date and delinquency dates
    
    Tradeline_data2$diff_delq_90_plus_dt <- ifelse(Tradeline_data2$delq_dt_90_plus == ""|is.na(Tradeline_data2$delq_dt_90_plus),999999,round(as.numeric(difftime(Tradeline_data2$Reference_date,as.Date(Tradeline_data2$delq_dt_90_plus,"%Y-%m-%d"),units = "days"))))
    Tradeline_data2$diff_delq_60_plus_dt <- ifelse(Tradeline_data2$delq_dt_60_plus == ""|is.na(Tradeline_data2$delq_dt_60_plus),999999,round(as.numeric(difftime(Tradeline_data2$Reference_date,as.Date(Tradeline_data2$delq_dt_60_plus,"%Y-%m-%d"),units = "days"))))
    Tradeline_data2$diff_delq_30_plus_dt <- ifelse(Tradeline_data2$delq_dt_30_plus == ""|is.na(Tradeline_data2$delq_dt_30_plus),999999,round(as.numeric(difftime(Tradeline_data2$Reference_date,as.Date(Tradeline_data2$delq_dt_30_plus,"%Y-%m-%d"),units = "days"))))
    Tradeline_data2$diff_delq_5_plus_dt <- ifelse(Tradeline_data2$delq_dt_5_plus == ""|is.na(Tradeline_data2$delq_dt_5_plus),999999,round(as.numeric(difftime(Tradeline_data2$Reference_date,as.Date(Tradeline_data2$delq_dt_5_plus,"%Y-%m-%d"),units = "days"))))
    
    ## Creating variable max_dt in order to identifying the last payment date as maximum of "DateOfLastPayment","PaymentHistoryStartDate","DateReported"
    
    Tradeline_data2$max_dt <- apply(Tradeline_data2[,c("DateOfLastPayment","PaymentHistoryStartDate","DateReported")],1,max)
    
    
    data_tradeline_with_DPD_var <- Tradeline_data2
    return(data_tradeline_with_DPD_var)
    
  }
  
  
  Tradeline_data_with_DPD_var <- feature_engineering_DPD_variables(Tradeline_data2)
  
  
  # ----------------------------------   3.  FEATURE ENGINEERING- LOAN TYPE BASED VARIABLES  ----------------------------------------
  
  
  # Defining function to create these variables
  
  feature_engineering_loan_type_variables <- function(Data_tradeline){
    
    data_Tradeline_data2 <-data.table(Data_tradeline) #rename data_Tradeline_data2 later 
    
    
    TL_data_CRN <-data_Tradeline_data2[,j=list(
      num_loans = .N,
      Bureau_tenure_CC= max(diff_open_dt[(ACCOUNT_DESC_derived=="Credit Card")],0),
      Bureau_tenure_NonBank= max(diff_open_dt[type_of_creditor=="NonBank"],0),
      max_worstDPDBucket_Other = max(worstDPDBucket[(ACCOUNT_DESC_derived=="Others")],0),
      num_Other = length(CRN[ACCOUNT_DESC_derived=="Others"])
    ),by=CRN]
    
    return (TL_data_CRN)
    
  }
  
  
  
  Tradeline_data_with_loan_type_var <- feature_engineering_loan_type_variables(Tradeline_data_with_DPD_var) 
  
  
  #---------------------------------     4.  Creating the loan type variables at rolling time windows  ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  feature_engineering_Loan_type_trend_variables <- function(data_Tradeline_data2){
    
    full_data2 <- data_Tradeline_data2
    
    full_data2 = data.frame(full_data2[,c('CRN')])
    full_data2 = data.frame(full_data2[!duplicated(full_data2),])
    colnames(full_data2)[1] <- "CRN"
    
    
    months <- c(366, 730,1095)
    month_names<-c("to_1yr", "1_to_2yr", "2yr_to_3yr", ">3yr")
    
    
    for (i in 1:length(months)){
      
      if(i==1){
        dataset <- subset(data_Tradeline_data2,(diff_open_dt<=months[i]))
        
      } else {
        dataset <- subset(data_Tradeline_data2,(diff_open_dt<=months[i]) & (diff_open_dt>months[i-1]) )
        
      }
      dataset <- subset(data_Tradeline_data2,(diff_open_dt<=months[i]) & (diff_open_dt>months[i-1]) )
      dataset<- data.table(dataset)
      Fact_Acct2 <- dataset[,j=list(
        loan_amt_last = sum(Amount[ACCOUNT_DESC_derived !="Credit Card"],na.rm=TRUE),
        Num_Other_last = length(CRN[ACCOUNT_DESC_derived == "Others"])
      ),by=CRN]
      
      colnames(Fact_Acct2) <- paste0(colnames(Fact_Acct2),"_",month_names[i])
      colnames(Fact_Acct2)[1] <- "CRN"
      
      full_data2 <- merge(full_data2,Fact_Acct2,by= "CRN",all.x = TRUE)
      
    }
    
    
    data_Tradeline_data2<- full_data2
    
    return(data_Tradeline_data2)
    
  }
  
  
  Tradeline_data_with_loan_type_trend_var<- feature_engineering_Loan_type_trend_variables(Tradeline_data_with_DPD_var) #Not on CRN level, Tradeline level
  
  Tradeline_data_with_loan_type_trend_var_kep = Tradeline_data_with_loan_type_trend_var[,c("CRN", "Num_Other_last_2yr_to_3yr", "loan_amt_last_2yr_to_3yr")]
  
  
  #---------------------------------     5.  Creating the loan type variables at monthly level  ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  
  # Defining function to create these variables
  
  feature_engineering_loan_type_monthly_variables <- function(Data_tradeline){
    
    
    
    full_data_CRN <- Data_tradeline
    
    #from fina, to make customer level
    full_data_CRN = data.frame(full_data_CRN[,c('CRN')])
    full_data_CRN = data.frame(full_data_CRN[!duplicated(full_data_CRN),])
    colnames(full_data_CRN)[1] <- "CRN"
    
    months <- c(365,548,730,1095,1460)
    month_names<-c("1yr","18m","2yr","3yr","4yr")
    
    
    for (i in 1:length(months)){
      
      dataset <- subset(Data_tradeline,(diff_open_dt<=months[i]))
      dataset <- data.table(dataset)
      TL_CRN <- dataset[,j=list(
        Amt_NonBank_last = sum(Amount[(type_of_creditor=="NonBank")],na.rm=TRUE),
        Num_Other_last = length(CRN[ACCOUNT_DESC_derived=="Others"]),
        num_NonBank_last = length(CRN[(type_of_creditor=="NonBank")]),
        Num_eksternal_last = length(CRN[(ReportingMemberShortName_derived=="Eksternal")])
      ),by=CRN]
      
      
      colnames(TL_CRN) <- paste0(colnames(TL_CRN),"_",month_names[i])
      colnames(TL_CRN)[1] <- "CRN"
      
      full_data_CRN <- merge(full_data_CRN,TL_CRN,by= "CRN",all.x = TRUE)
      
      
    }
    
    ## To create monthly delinquency variables
    
    BUREAU_TL_data_CRN_level <- full_data_CRN # (same as Tradeline_data_with_DPD_var/ Data_tradeline)
    #months <- c(91,183,275,365,548,730,1095,1460,1825)
    months <- c(365,548,730,1095,1460)
    month_names<-c("1yr","18m","2yr","3yr","4yr")
    Data_tradeline <- data.table(Data_tradeline)
    for (i in 1:length(months)){
      delq_monthly_flags = Data_tradeline[,j=list(
        Num_delq_5_plus_Other_last = length(CRN[(diff_delq_5_plus_dt< months[i] )&(ACCOUNT_DESC_derived=="Others")]),
        Num_delq_5_plus_Eksternal_last = length(CRN[(diff_delq_5_plus_dt< months[i])&(ReportingMemberShortName_derived=="Eksternal")]),
        Num_delq_5_plus_Individual_last = length(CRN[(diff_delq_5_plus_dt< months[i] )&(Ownership_type=="MainDebtor")]),
        Num_delq_5_plus_live_last = length(CRN[(diff_delq_5_plus_dt< months[i] )&(phase_of_contract=="Open")])
        
      ),by=CRN]
      
      colnames(delq_monthly_flags) <- paste0(colnames(delq_monthly_flags),"_",month_names[i])
      colnames(delq_monthly_flags)[1] <- "CRN"
      
      BUREAU_TL_data_CRN_level <- merge(BUREAU_TL_data_CRN_level,delq_monthly_flags,by= "CRN",all.x = TRUE)
      
    }
    
    
    return (BUREAU_TL_data_CRN_level)
  }
  
  Tradeline_data_with_loan_type_monthly_var <- feature_engineering_loan_type_monthly_variables(Tradeline_data_with_DPD_var) #Trade level
  
  Tradeline_data_with_loan_type_monthly_var_keep = Tradeline_data_with_loan_type_monthly_var[,c("CRN", "Num_Other_last_3yr", "num_NonBank_last_18m",
                                                                                                "num_NonBank_last_3yr", "num_NonBank_last_2yr", "Amt_NonBank_last_3yr",
                                                                                                "Num_eksternal_last_3yr", "Num_delq_5_plus_Other_last_18m", "Num_delq_5_plus_Eksternal_last_18m",
                                                                                                "Num_delq_5_plus_Eksternal_last_2yr", "Num_delq_5_plus_Individual_last_18m",
                                                                                                "Num_delq_5_plus_live_last_1yr", "Num_delq_5_plus_Eksternal_last_1yr")]
  
  
  
  #gabung data
  All_tradeline <- merge(Tradeline_data_with_loan_type_var,Tradeline_data_with_loan_type_trend_var_kep,by= "CRN",all.x = TRUE)
  All_tradeline <- merge(All_tradeline,Tradeline_data_with_loan_type_monthly_var_keep,by= "CRN",all.x = TRUE)
  
  ###########  get prediction model
  
  #gabung all data
  
  Final_data = merge(All_tradeline,Tradeline_data_with_colla_keep, by = "CRN", all.x= T)
  
  Final_data <- data.frame(Final_data)
  Final_data_ori = Final_data
  
  model_xgb = model_xgb_load
  
  var_imp <- xgb_importance$Feature
  
  var_list_xgb <- model_xgb$feature_names
  dd <- setdiff(var_list_xgb,var_imp)
  
  
  ## Make dummy variables for 'non important' 136 variables in model
  for (i in 1:length(dd)){
    Final_data[[dd[i]]] <- NA
  }
  
  
  ## Converting Test and Train in XGB
  dall2 <- xgb.DMatrix(data = data.matrix(Final_data[,var_list_xgb]))
  
  
  ### Predicting using the model
  xgb_bureau <- predict(model_xgb,dall2)
  xgb_bureau_recode <- xgb_bureau
  
  scored_data_bureau <- cbind(Final_data_ori,xgb_bureau,xgb_bureau_recode)
  
  scored_data_bureau_final <- scored_data_bureau %>%
    select(CRN,xgb_bureau,xgb_bureau_recode) 
  
  return(scored_data_bureau_final)
}

# -------------------------------------------------------------------------------------------------------------- #




# -------------------------------------------------------------------------------------------------------------- #
#### 4. BNK. FUNGSI MENGHITUNG PD SCORE MODUL BANKING ####
pdscore_banking <- function(json_input,cfmast_data,ddmast_data,ddhist_data,del_cif1_x,del_cdhist){
  options(scipen = 999)

  # ambil data cif nasabah
  demog_data <- json_input[["DemographicData"]]

  #### Buat tabel untuk menghitung year in bank dari data cfmast ####
  df_year_in_bank = cfmast_data


  # cek apakah CIF ada di tabel cfmast
  if(nrow(df_year_in_bank) == 0 ){
    del_ddmast5 <- data.frame(cifno = demog_data[['CIF']] , dd_bal_avg3b = NA )
    ddhist_ag5 <- data.frame(cifno = demog_data[['CIF']] , ratio_trlocadd_c_avg6_12 = NA , ratio_trlocadd_num_c_avg3_12 = NA )
    cdhist <- data.frame(cifno = demog_data[['CIF']] , cd_avg_tx_amt_d_12 = NA )
    yr_in_bnk2 <- data.frame(cfcif_2 = demog_data[['CIF']],yr_with_bank = NA)
  }else {

    current_date = Sys.Date()

    # Hitung year in bank
    df_year_in_bank2 = df_year_in_bank %>%
      mutate(yr_with_bank = round(interval(cforgd,current_date)/years(1) )) %>%
      arrange(cfcif_2, desc(yr_with_bank))

    yr_in_bnk2 = df_year_in_bank2[!duplicated(df_year_in_bank2$cfcif_2),]

    ### Ambil data ddmast ###
    del_cif = ddmast_data
    ####  PENGECEKAN CIF DI DDMAST.
    ###### Jika CIF tidak ditemukan, del_ddmast5 dan ddhist_ag5 isinya NA
    if(nrow(del_cif) == 0 ){
      del_ddmast5 <- data.frame(cifno = demog_data[['CIF']] , dd_bal_avg3b = NA )
      ddhist_ag5 <- data.frame(cifno = demog_data[['CIF']] , ratio_trlocadd_c_avg6_12 = NA , ratio_trlocadd_num_c_avg3_12 = NA )
    }else {
      ### Ambil data dari ddhist ###
      curr_date = as.Date(Sys.Date())
      ddhist_ag = ddhist_data
      if(nrow(ddhist_ag) == 0 ){
        ddhist_ag5 <- data.frame(cifno = demog_data[['CIF']] , ratio_trlocadd_c_avg6_12 = NA , ratio_trlocadd_num_c_avg3_12 = NA )
      }else {
        ## Ambil data acctno yang distinct ##
        distinct_cif <- del_cif %>%
          distinct(acctno, cifno, .keep_all = TRUE)

        ddhist_ag2 = ddhist_ag %>%
          inner_join(distinct_cif, by = c("tracct" = "acctno"))

        ddhist_ag3 = ddhist_ag2 %>%
          mutate(hist_month = (year(curr_date) - year(treffd))*12+ (month(curr_date) - month(treffd)))


        ddhist_ag4 = ddhist_ag3 %>%
          dplyr::group_by(cifno, trdorc, hist_month) %>%
          dplyr::summarise(sum_trloca = sum(trloca),
                           num = n())


        ddhist_ag5 = ddhist_ag4 %>%
          group_by(cifno)%>%
          summarize(trlocadd_c_1 = sum(sum_trloca[trdorc=='C' & hist_month == 1]),
                    trlocadd_c_2 = sum(sum_trloca[trdorc=='C' & hist_month == 2]),
                    trlocadd_c_3 = sum(sum_trloca[trdorc=='C' & hist_month == 3]),
                    trlocadd_c_4 = sum(sum_trloca[trdorc=='C' & hist_month == 4]),
                    trlocadd_c_5 = sum(sum_trloca[trdorc=='C' & hist_month == 5]),
                    trlocadd_c_6 = sum(sum_trloca[trdorc=='C' & hist_month == 6]),
                    trlocadd_c_7 = sum(sum_trloca[trdorc=='C' & hist_month == 7]),
                    trlocadd_c_8 = sum(sum_trloca[trdorc=='C' & hist_month == 8]),
                    trlocadd_c_9 = sum(sum_trloca[trdorc=='C' & hist_month == 9]),
                    trlocadd_c_10 = sum(sum_trloca[trdorc=='C' & hist_month == 10]),
                    trlocadd_c_11 = sum(sum_trloca[trdorc=='C' & hist_month == 11]),
                    trlocadd_c_12 = sum(sum_trloca[trdorc=='C' & hist_month == 12]),
                    trlocadd_num_c_1 = sum(num[trdorc=='C' & hist_month == 1]),
                    trlocadd_num_c_2 = sum(num[trdorc=='C' & hist_month == 2]),
                    trlocadd_num_c_3 = sum(num[trdorc=='C' & hist_month == 3]),
                    trlocadd_num_c_4 = sum(num[trdorc=='C' & hist_month == 4]),
                    trlocadd_num_c_5 = sum(num[trdorc=='C' & hist_month == 5]),
                    trlocadd_num_c_6 = sum(num[trdorc=='C' & hist_month == 6]),
                    trlocadd_num_c_7 = sum(num[trdorc=='C' & hist_month == 7]),
                    trlocadd_num_c_8 = sum(num[trdorc=='C' & hist_month == 8]),
                    trlocadd_num_c_9 = sum(num[trdorc=='C' & hist_month == 9]),
                    trlocadd_num_c_10 = sum(num[trdorc=='C' & hist_month == 10]),
                    trlocadd_num_c_11 = sum(num[trdorc=='C' & hist_month == 11]),
                    trlocadd_num_c_12 = sum(num[trdorc=='C' & hist_month == 12])
          )


        ddhist_ag5$trlocadd_c_avg6=rowMeans(ddhist_ag5[c('trlocadd_c_1','trlocadd_c_2','trlocadd_c_3',
                                                         'trlocadd_c_4','trlocadd_c_5','trlocadd_c_6')],na.rm = T)

        ddhist_ag5$trlocadd_c_avg12=rowMeans(ddhist_ag5[c('trlocadd_c_1','trlocadd_c_2','trlocadd_c_3',
                                                          'trlocadd_c_4','trlocadd_c_5','trlocadd_c_6',
                                                          'trlocadd_c_7','trlocadd_c_8','trlocadd_c_9',
                                                          'trlocadd_c_10','trlocadd_c_11','trlocadd_c_12')],na.rm = T)

        ddhist_ag5$ratio_trlocadd_c_avg6_12 <- ifelse(ddhist_ag5$trlocadd_c_avg12==0,0,ddhist_ag5$trlocadd_c_avg6/ddhist_ag5$trlocadd_c_avg12)

        ddhist_ag5$trlocadd_num_c_avg3=rowMeans(ddhist_ag5[c('trlocadd_num_c_1','trlocadd_num_c_2','trlocadd_num_c_3')],na.rm = T)
        ddhist_ag5$trlocadd_num_c_avg12=rowMeans(ddhist_ag5[c('trlocadd_num_c_1','trlocadd_num_c_2','trlocadd_num_c_3',
                                                              'trlocadd_num_c_4','trlocadd_num_c_5','trlocadd_num_c_6',
                                                              'trlocadd_num_c_7','trlocadd_num_c_8','trlocadd_num_c_9',
                                                              'trlocadd_num_c_10','trlocadd_num_c_11','trlocadd_num_c_12')],na.rm = T)
        ddhist_ag5$ratio_trlocadd_num_c_avg3_12 <- ifelse(ddhist_ag5$trlocadd_num_c_avg12==0,0,ddhist_ag5$trlocadd_num_c_avg3/ddhist_ag5$trlocadd_num_c_avg12)

      }


      #ddbal_avg3b
      ##### PERHITUNGAN VARIABEL DEL_DDMAST5 JIKA CIF DITEMUKAN DI DDMAST
      del_ddmast = ddmast_data

      del_ddmast = del_ddmast %>%
        mutate(hist_month = (year(curr_date) - year(scd_start))*12+ (month(curr_date) - month(scd_start)))


      del_ddmast3 = del_ddmast %>%
        dplyr::group_by(cifno, hist_month) %>%
        dplyr::summarise(sum_cbal = sum(cbal),
                         num = n())

      del_ddmast4 = del_ddmast3 %>%
        dplyr::group_by(cifno) %>%
        dplyr::summarise(dd_bal_1 = sum(sum_cbal[hist_month==1]),
                         dd_bal_2 = sum(sum_cbal[hist_month==2]),
                         dd_bal_3 = sum(sum_cbal[hist_month==3]),
                         dd_bal_4 = sum(sum_cbal[hist_month==4]),
                         dd_bal_5 = sum(sum_cbal[hist_month==5]),
                         dd_bal_6 = sum(sum_cbal[hist_month==6]),

                         dd_num_1 = sum(num[hist_month==1]),
                         dd_num_2 = sum(num[hist_month==2]),
                         dd_num_3 = sum(num[hist_month==3]),
                         dd_num_4 = sum(num[hist_month==4]),
                         dd_num_5 = sum(num[hist_month==5]),
                         dd_num_6 = sum(num[hist_month==6]))


      del_ddmast5 = del_ddmast4

      del_ddmast5[,'dd_bal_avg1a'] <- del_ddmast5[,'dd_bal_1']/del_ddmast5[,'dd_num_1']
      del_ddmast5[is.na(del_ddmast5$dd_bal_avg1a),'dd_bal_avg1a']=0

      del_ddmast5[,'dd_bal_avg2a'] <- del_ddmast5[,'dd_bal_2']/del_ddmast5[,'dd_num_2']
      del_ddmast5[is.na(del_ddmast5$dd_bal_avg2a),'dd_bal_avg2a']=0

      del_ddmast5[,'dd_bal_avg3a'] <- del_ddmast5[,'dd_bal_3']/del_ddmast5[,'dd_num_3']
      del_ddmast5[is.na(del_ddmast5$dd_bal_avg3a),'del_ddmast2l_avg3a']=0

      del_ddmast5$dd_bal_avg3b=rowMeans(del_ddmast5[c('dd_bal_avg1a','dd_bal_avg2a','dd_bal_avg3a')])

    }
    #
    #     ######### AMBIL DATA CIF DARI CDMAST
    #     #TANPA CON
    #     #sql_delcif1 =  " select distinct acctno, cifno from sb.cdmast where cifno in (%s) "
    #     #sql_delcif1_x = sprintf(sql_delcif1, listcif)
    #
    #del_cif1_x = dbGetQuery(con,sql_delcif1_x)
    if(nrow(del_cif1_x) == 0 ){
      cdhist <- data.frame(cifno = demog_data[['CIF']] , cd_avg_tx_amt_d_12 = NA )

    }else {

      #       #### AMBIL DATA CIF DARI CDHIST
      distinct_cif_x = del_cif1_x
      #       #  distinct(acctno, cifno, .keep_all = TRUE)
      #
      #       #list_acct_x = toString(sprintf("'%s'", unlist(distinct_cif_x$acctno)))
      #
      #       # sql_cdhist_x =  " select chacct, chdorc, trloca, chefdt from sb.cdhist where chacct in (%s) "
      #       #sql_cdhist_acct_x = sprintf(sql_cdhist_x, list_acct_x)
      #
      #       #del_cdhist= dbGetQuery(con,sql_cdhist_acct_x)
      #
      ##### PENGECEKAN APAKAH CIF ADA DI CDHIST
      if(nrow(del_cdhist) == 0 ){
        cdhist <- data.frame(cifno = demog_data[['CIF']] , cd_avg_tx_amt_d_12 = NA )
      }else {

        cdhist_ag2 = del_cdhist %>%
          inner_join(distinct_cif_x, by = c("chacct" = "acctno"))

        cdhist_ag3 = cdhist_ag2 %>%
          mutate(hist_month = (year(curr_date) - year(chefdt))*12+ (month(curr_date) - month(chefdt)))


        cdhist_ag4 = cdhist_ag3 %>%
          dplyr::group_by(cifno, chdorc, hist_month) %>%
          dplyr::summarise(sum_trloca = sum(trloca),
                           num = n())


        cdhist_ag5 = cdhist_ag4 %>%
          group_by(cifno)%>%
          summarize(trlocacd_d_1 = sum(sum_trloca[chdorc=='D' & hist_month == 1]),
                    trlocacd_d_2 = sum(sum_trloca[chdorc=='D' & hist_month == 2]),
                    trlocacd_d_3 = sum(sum_trloca[chdorc=='D' & hist_month == 3]),
                    trlocacd_d_4 = sum(sum_trloca[chdorc=='D' & hist_month == 4]),
                    trlocacd_d_5 = sum(sum_trloca[chdorc=='D' & hist_month == 5]),
                    trlocacd_d_6 = sum(sum_trloca[chdorc=='D' & hist_month == 6]),
                    trlocacd_d_7 = sum(sum_trloca[chdorc=='D' & hist_month == 7]),
                    trlocacd_d_8 = sum(sum_trloca[chdorc=='D' & hist_month == 8]),
                    trlocacd_d_9 = sum(sum_trloca[chdorc=='D' & hist_month == 9]),
                    trlocacd_d_10 = sum(sum_trloca[chdorc=='D' & hist_month == 10]),
                    trlocacd_d_11 = sum(sum_trloca[chdorc=='D' & hist_month == 11]),
                    trlocacd_d_12 = sum(sum_trloca[chdorc=='D' & hist_month == 12]),

                    trlocacd_num_d_1 = sum(num[chdorc=='D' & hist_month == 1]),
                    trlocacd_num_d_2 = sum(num[chdorc=='D' & hist_month == 2]),
                    trlocacd_num_d_3 = sum(num[chdorc=='D' & hist_month == 3]),
                    trlocacd_num_d_4 = sum(num[chdorc=='D' & hist_month == 4]),
                    trlocacd_num_d_5 = sum(num[chdorc=='D' & hist_month == 5]),
                    trlocacd_num_d_6 = sum(num[chdorc=='D' & hist_month == 6]),
                    trlocacd_num_d_7 = sum(num[chdorc=='D' & hist_month == 7]),
                    trlocacd_num_d_8 = sum(num[chdorc=='D' & hist_month == 8]),
                    trlocacd_num_d_9 = sum(num[chdorc=='D' & hist_month == 9]),
                    trlocacd_num_d_10 = sum(num[chdorc=='D' & hist_month == 10]),
                    trlocacd_num_d_11 = sum(num[chdorc=='D' & hist_month == 11]),
                    trlocacd_num_d_12 = sum(num[chdorc=='D' & hist_month == 12])
          )

        cdhist = cdhist_ag5

        cdhist[is.na(cdhist)] = 0

        cdhist$trlocacd_d_sum12 <- cdhist$trlocacd_d_1 + cdhist$trlocacd_d_2 + cdhist$trlocacd_d_3 +
          cdhist$trlocacd_d_4 + cdhist$trlocacd_d_5 + cdhist$trlocacd_d_6 + cdhist$trlocacd_d_7 +
          cdhist$trlocacd_d_8 + cdhist$trlocacd_d_9 + cdhist$trlocacd_d_10 + cdhist$trlocacd_d_11 +
          cdhist$trlocacd_d_12

        cdhist$trlocacd_num_d_sum12 <- cdhist$trlocacd_num_d_1 + cdhist$trlocacd_num_d_2 +
          cdhist$trlocacd_num_d_3 + cdhist$trlocacd_num_d_4 + cdhist$trlocacd_num_d_5 +
          cdhist$trlocacd_num_d_6 + cdhist$trlocacd_num_d_7 + cdhist$trlocacd_num_d_8 +
          cdhist$trlocacd_num_d_9 + cdhist$trlocacd_num_d_10 + cdhist$trlocacd_num_d_11 +
          cdhist$trlocacd_num_d_12

        cdhist$cd_avg_tx_amt_d_12 <- ifelse(cdhist$trlocacd_num_d_sum12==0,0,cdhist$trlocacd_d_sum12/cdhist$trlocacd_num_d_sum12)
      }
    }
  }


  # LOGIC JIKA DDMAST DAN cdmast TIDAK ADA

  ddhist_ag5$cifno = as.numeric(as.character(ddhist_ag5$cifno ))
  del_ddmast5$cifno = as.numeric(as.character(del_ddmast5$cifno ))
  cdhist$cifno = as.numeric(as.character(cdhist$cifno ))
  yr_in_bnk2$cifno = as.numeric(as.character(yr_in_bnk2$cfcif_2 ))

  banking_module_all <- ddhist_ag5 %>%
    left_join(del_ddmast5, by = "cifno") %>%
    left_join(cdhist, by = "cifno") %>%
    left_join(yr_in_bnk2, by = ("cifno"))

  banking_module_all$yr_wth_bnk = banking_module_all$yr_with_bank
  var_list1 <- c("ratio_trlocadd_c_avg6_12","dd_bal_avg3b","ratio_trlocadd_num_c_avg3_12",
                 "cd_avg_tx_amt_d_12",
                 "yr_wth_bnk")

  #check apakah semua nilai variabel banking bernilai NA. Jika TRUE maka xgb_banking = NA
  check_na <- apply(banking_module_all[var_list1],1,function(x) all(is.na(x)))

  if(check_na){
    scored_banking_final <- data.frame(cifno = demog_data[['CIF']] , xgb_banking = NA)
  } else {
    banking_module_all[is.na(banking_module_all)] = 0
    # ------------------------------ Calculate PDScore for Banking Module ------------------------------#
    dscore <- xgb.DMatrix(data = data.matrix(banking_module_all[,var_list1]))

    xgb_banking <- predict(model_banking,dscore)
    scored_banking <- cbind(banking_module_all,xgb_banking)
    scored_banking_final <- scored_banking[,c("cifno","xgb_banking")]

    ### untuk testing uat los lms
    scored_banking_final$cifno = demog_data[['CIF']]
  }
  return(scored_banking_final)
}

# -------------------------------------------------------------------------------------------------------------- #

#### 5. DIG. FUNGSI MENGHITUNG PD SCORE MODUL DIGITAL ####
pdscore_digital <- function(json_input,ddmast_data,ddhist_data,ebwmemberaccountchannel_tr3,ebwmemberaccountchannel_th3){
  file_date <- rollback(as.Date(Sys.Date()) )

  # ambil data cif nasabah

  demog_data <- json_input[["DemographicData"]]

  # ebwmemberaccountchannel_tr =  " select distinct on (cifnumber) cifnumber ,membersince
  # from sb.ebwmemberaccountchannel_tr where cifnumber in (%s) order by
  # cifnumber ,membersince"
  #
  # ebwmemberaccountchannel_tr2 =sprintf(ebwmemberaccountchannel_tr,listcif)
  # ebwmemberaccountchannel_tr3 = dbGetQuery(con,ebwmemberaccountchannel_tr2)
  #
  #
  # ebwmemberaccountchannel_th =  " select distinct on (cifnumber) cifnumber ,membersince
  # from sb.ebwmemberaccountchannel_th where cifnumber in (%s) order by
  # cifnumber ,membersince"
  #
  # ebwmemberaccountchannel_th2 =sprintf(ebwmemberaccountchannel_th,listcif)
  # ebwmemberaccountchannel_th3 = dbGetQuery(con,ebwmemberaccountchannel_th2)

  ebwmemberaccountchannel_tr_th = rbind(ebwmemberaccountchannel_tr3,ebwmemberaccountchannel_th3)


  # cek apakah cif muncul di data ebw
  if (nrow(ebwmemberaccountchannel_tr_th) == 0){
    delk_net1 <- data.frame(cifnumber = demog_data[['CIF']], net_bnk_yr = NA)
  }else{
    delk_net1 = ebwmemberaccountchannel_tr_th %>%
      dplyr::group_by(cifnumber) %>%
      dplyr::summarise(net_membersince =min(membersince))

    delk_net1$net_bnk_yr <- year(file_date) - as.numeric(format(delk_net1$net_membersince,'%Y'))
  }

  ####
  # transactions from Net banking
  ## ambil data dari ddmast dan ddhist ##
  ddmast_real <- ddmast_data
  ddhist_ag = ddhist_data

  #-------cek apakah ada histori transaksi digital di ddhist_ag_digi ---- #
  if (nrow(ddhist_ag) != 0){
    ddhist_ag_digi <- ddhist_ag %>%
      filter(trbr == 88333)

    if(nrow(ddhist_ag_digi) != 0){
      #gabung ddhist
      ddhist_digi = ddhist_ag_digi %>%
        inner_join(ddmast_real, by= c("tracct" = "acctno"))


      curr_date = as.Date(Sys.Date())

      ddhist_digi = ddhist_digi %>%
        mutate(hist_month = (year(curr_date) - year(treffd))*12+ (month(curr_date) - month(treffd)))  %>%
        arrange(cifno,tracct,hist_month )



      ddhist_ag4 = ddhist_digi %>%
        dplyr::group_by(cifno, trdorc, hist_month) %>%
        dplyr::summarise(sum_trloca = sum(trloca),
                         num = n())

      #get 12 mnth
      ddhist_ag4a = ddhist_ag4[ddhist_ag4$hist_month <= 12,]


      #bikin template 12

      templat_a <- data.frame(no = 1:12)
      ddhist_ag4a_temp = ddhist_ag4a[!duplicated(ddhist_ag4a[c('cifno', 'trdorc')]),c('cifno', 'trdorc')]


      #merge master with template 12
      ddhist_ag4a_temp2 = merge(x=ddhist_ag4a_temp, y=templat_a, all= TRUE)

      ddhist_ag4a_temp2 = ddhist_ag4a_temp2 %>%
        dplyr :: arrange(cifno,trdorc, no)

      ddhist_ag4a_temp2$hist_month = ddhist_ag4a_temp2$no
      ddhist_ag4a_temp2$no = NULL

      ddhist_ag4b = ddhist_ag4a_temp2 %>%
        dplyr :: left_join(ddhist_ag4a, by = c("cifno", "trdorc", "hist_month"))


      #transpose data
      ddhist_ag4b$trdorc2 = tolower(ddhist_ag4b$trdorc)
      ddhist_ag4b$no2 = paste0("net_trlocadd", "_", ddhist_ag4b$trdorc2, "_" , ddhist_ag4b$hist_month )

      ddhist_ag4d = ddhist_ag4b[ddhist_ag4b$trdorc %in% 'D' , c("cifno", "no2", "sum_trloca")]
      ## jika ddhist_ag4d kosong atau na :
      if (nrow(ddhist_ag4d) == 0) {
        ddhist_ag4d2 <- data.frame (cifno = demog_data[['CIF']], net_trlocadd_d_avg6 = NA, net_trlocadd_d_11 = NA)
      }
      else{
        ddhist_ag4d2 = cast(ddhist_ag4d, cifno~no2)

        ddhist_ag4d2[is.na(ddhist_ag4d2)] = 0

        ddhist_ag4d2$net_trlocadd_d_avg6=rowMeans(ddhist_ag4d2[c('net_trlocadd_d_1','net_trlocadd_d_2','net_trlocadd_d_3','net_trlocadd_d_4','net_trlocadd_d_5','net_trlocadd_d_6')],na.rm = T)
      }
    } else {
      ddhist_ag4d2 <- data.frame (cifno = demog_data[['CIF']], net_trlocadd_d_avg6 = NA, net_trlocadd_d_11 = NA)
    }

  } else {
    ddhist_ag4d2 <- data.frame (cifno = demog_data[['CIF']], net_trlocadd_d_avg6 = NA, net_trlocadd_d_11 = NA)
    ddhist_ag_digi <- data.frame()
  }

  ### ----------- cek apakah cif ada di kedua tabel ddmast dan ebwmemberaccountchannel ---------####
  if(nrow(ddhist_ag_digi) == 0 && nrow(ebwmemberaccountchannel_tr_th) == 0){
    scored_digital_final <- data.frame(cifno = demog_data[['CIF']] , xgb_digital = NA)
  }else {
    delk_net1$cifnumber = as.numeric(as.character(delk_net1$cifnumber))
    ddhist_ag4d2$cifno = as.numeric(as.character(ddhist_ag4d2$cifno))


    #final data digi
    ddhist_digi_final  = delk_net1 %>%
      inner_join(ddhist_ag4d2, by= c("cifnumber" = "cifno"))

    df1 = ddhist_digi_final
    df1 = df1[,c("cifnumber", "net_bnk_yr", "net_trlocadd_d_11", "net_trlocadd_d_avg6")]

    #make feature that not use in final model
    df1$net_trlocadd_num_d_11 = 0
    df1$net_trlocadd_c_2 = 0
    df1$net_trlocadd_num_c_2 = 0
    df1$net_trlocadd_d_9 = 0
    df1$net_trlocadd_num_d_9 = 0
    df1$net_trlocadd_num_d_12 = 0
    df1$net_trlocadd_d_12 = 0
    df1$net_trlocadd_c_5 = 0
    df1$net_trlocadd_num_c_5 = 0
    df1$net_trlocadd_num_c_10 = 0
    df1$net_trlocadd_c_10 = 0
    df1$net_trlocadd_c_12 = 0
    df1$net_trlocadd_num_c_12 = 0
    df1$net_trlocadd_c_11 = 0
    df1$net_trlocadd_num_c_11 = 0

    df1$net_bnk_yr.1 = df1$net_bnk_yr


    var_list2 = c("net_bnk_yr",
                  "net_trlocadd_d_11",
                  "net_trlocadd_num_d_11",
                  "net_trlocadd_c_2",
                  "net_trlocadd_num_c_2",
                  "net_trlocadd_d_9",
                  "net_trlocadd_num_d_9",
                  "net_trlocadd_num_d_12",
                  "net_trlocadd_d_12",
                  "net_trlocadd_c_5",
                  "net_trlocadd_num_c_5",
                  "net_trlocadd_num_c_10",
                  "net_trlocadd_c_10",
                  "net_trlocadd_c_12",
                  "net_trlocadd_num_c_12",
                  "net_trlocadd_c_11",
                  "net_trlocadd_num_c_11",
                  "net_trlocadd_d_avg6",
                  "net_bnk_yr.1")

    dscore <- xgb.DMatrix(data = data.matrix(df1[,var_list2]))

    xgb_digital <- predict(model_digital,dscore)
    scored_digital = cbind(df1,xgb_digital)

    scored_digital_final <-scored_digital %>%
      select(cifnumber,xgb_digital)%>%
      mutate(cifno = cifnumber)

    #untuk testing uat los lms
    scored_digital_final$cifno = demog_data[['CIF']]
  }
  return(scored_digital_final)
}

# -------------------------------------------------------------------------------------------------------------- #

#### 6. DEM. FUNGSI MENGHITUNG PD SCORE MODUL DEMOGRAFIK NASABAH ####
feature_engineering_demog <- function(json_input){
  
  demog_data <- json_input[["DemographicData"]]
  demog_data_a <- simple_rapply(demog_data, function(x) if(is.null(x)) NA else x)
  demog_data <- as_tibble(demog_data_a)
  
  
  # find the closest bank in radius 3 km with applicant based on their id card zipcode
  zipcode <- demog_data_a$DomisiliZipcode
  if(is.na(zipcode) | zipcode == "" | !any(amenity$postcode == zipcode)){
    amenity_3km <- NA
  }else{
    amenity_3km <- amenity$amenity_bank_3km[amenity$postcode ==  zipcode]
  }
  
  demog_data$amenity_bank_3km = amenity_3km # for now get static data from csv
  
  # get customer age based on demographic data
  if(is.na(demog_data$DateOfBirth) | demog_data$DateOfBirth == ""){
    demog_data$customer_age = NA
  }else{
    demog_data$customer_age = as.period(interval(start = demog_data$DateOfBirth, end = demog_data$IncomingDate))$year                                    
  }
  
  # education demographic data
  demog_data$eduHighOrLess=0
  if(is.na(demog_data$Education) | demog_data$Education == ""){
    demog_data$eduHighOrLess = NA
  }else{
    demog_data[(demog_data[,'Education']=='A' | demog_data[,'Education']=='B' | demog_data[,'Education']=='C' | demog_data[,'Education']=='Y'| demog_data[,'Education']=='X' |demog_data[,'Education']==''|demog_data[,'Education']=='Z'|is.na(demog_data$Education) ),c('eduHighOrLess')]=1
  }
  
  # gender demographic data
  demog_data$male=0
  if(is.na(demog_data$Gender) | demog_data$Gender == ""){
    demog_data$male = NA
  }else{
    demog_data[(demog_data[,'Gender']=='M') ,c('male')]=1
  }
  
  # process the home ownership status of applicant
  demog_data$family_house=0
  if(is.na(demog_data$HomeOwnership)| demog_data$HomeOwnership == "" ){
    demog_data$family_house = NA
  }else{
    demog_data[(demog_data[,'HomeOwnership']=='H02') ,c('family_house')]=1
  }
  
  model_xgb = model_xgb_demog
  var_list_xgb1 <- model_xgb$feature_names
  
  library("xgboost")
  #predict the data
  dall <- xgb.DMatrix(data = data.matrix(demog_data[,var_list_xgb1]))
  
  xgb_demog <- predict(model_xgb,dall)
  
  
  scored_data_demog <- cbind(demog_data,xgb_demog)
  
  ### PD recode
  demog_data$male <- 0
  demog_data$family_house <-0
  
  dall <- xgb.DMatrix(data = data.matrix(demog_data[,var_list_xgb1]))
  
  xgb_demog_recode <- predict(model_xgb,dall)
  scored_data_demog <- cbind(scored_data_demog,xgb_demog_recode)
  
  
  scored_data_demog_final = scored_data_demog[,c("ApplicationNumber","GUID","Name","MarketingCode","CustomerType","IDNumber", "CIF","xgb_demog","xgb_demog_recode")]
  
  
  return(scored_data_demog_final)
}

# -------------------------------------------------------------------------------------------------------------- #
#### 7. NTB. PDSCORE UNTUK NASABAH NTB #### 
NTB_PDScore_final <-function(scored_data_bureau_final,scored_data_demog_final){
  scored_data1 <- merge(scored_data_demog_final,scored_data_bureau_final,by.x = "IDNumber",by.y = "CRN",all.x=T)
  
  ### calibration
  calibrated_constants
  # calibrated_constants
  # cal_cust      cal_bnk   cal_bureua      cal_net 
  # -0.007953087  0.084341943 -0.197682944  0.288260659
  
  scored_data1$logodd_demogs <- log(scored_data1$xgb_demog/(1-scored_data1$xgb_demog))
  scored_data1$PD_demogs_temp <- scored_data1$logodd_demogs + calibrated_constants[1]
  scored_data1$PD_demogs_calib = 1/(1+exp(-scored_data1$PD_demogs_temp))
  
  scored_data1$logodd_bureau <- log(scored_data1$xgb_bureau/(1-scored_data1$xgb_bureau))
  if (scored_data1$bureau == "pefindo"){
    scored_data1$PD_bureau_temp <- scored_data1$logodd_bureau + calibrated_constants[3]
    #print(calibrated_constants[3])
  }else{
    scored_data1$PD_bureau_temp <- scored_data1$logodd_bureau + calibrated_constants[5]
    #print(calibrated_constants[5])
  }
  scored_data1$PD_bureau_calib = 1/(1+exp(-scored_data1$PD_bureau_temp))
  
  ### PD recode calibration
  # Demog
  scored_data1$logodd_demogs2 <- log(scored_data1$xgb_demog_recode/(1-scored_data1$xgb_demog_recode))
  scored_data1$PD_demogs_temp2 <- scored_data1$logodd_demogs2 + calibrated_constants[1]
  scored_data1$PD_demogs_calib2 = 1/(1+exp(-scored_data1$PD_demogs_temp2))
  
  # Bureau
  scored_data1$logodd_bureau2 <- log(scored_data1$xgb_bureau_recode/(1-scored_data1$xgb_bureau_recode))
  if (scored_data1$bureau == "pefindo"){
    scored_data1$PD_bureau_temp2 <- scored_data1$logodd_bureau2 + calibrated_constants[3]
    #print(calibrated_constants[3])
  }else{
    scored_data1$PD_bureau_temp2 <- scored_data1$logodd_bureau2 + calibrated_constants[5]
    #print(calibrated_constants[5])
  }
  scored_data1$PD_bureau_calib2 = 1/(1+exp(-scored_data1$PD_bureau_temp2))
  
  
  pd_database <- scored_data1[,c("ApplicationNumber","GUID","Name","MarketingCode","CustomerType","IDNumber", "CIF","PD_demogs_calib", "PD_bureau_calib","PD_demogs_calib2", "PD_bureau_calib2")]
  
  pd_database$PD_banking_calib = NA
  pd_database$PD_digital_calib = NA
  
  cal_pd_col_names = c("PD_demogs_calib","PD_banking_calib","PD_bureau_calib","PD_digital_calib")
  recode_cal_pd_col_names = c("PD_demogs_calib2","PD_banking_calib","PD_bureau_calib2","PD_digital_calib")
  
  ### weight redistribution and final PD calculation
  weight_matrix = c(0.1,0.45,0.35,0.1)
  
  w = data.frame(matrix(nrow = nrow(pd_database), ncol = 4))
  wgt_col_names = c("w1_cust","w2_bnk","w3_bureua","w4_net")
  names(w) = wgt_col_names
  
  for (i in 1:4){
    w[,i] <- weight_matrix[i]
  }
  
  ###Final wieghted PD after getting the right modular combination
  pd_database$pd = rowSums(pd_database[,cal_pd_col_names]*w, na.rm = T)/rowSums((!is.na(
    pd_database[,cal_pd_col_names]))* w, na.rm = T)
  # Final pd recode
  pd_database$pd_recode = rowSums(pd_database[,recode_cal_pd_col_names]*w, na.rm = T)/rowSums((!is.na(
    pd_database[,recode_cal_pd_col_names]))* w, na.rm = T)
  
  
  pd_database_to_los <- pd_database[,c("Name","ApplicationNumber","GUID","MarketingCode","CustomerType","IDNumber", "CIF","pd","pd_recode")]
  
  return(pd_database_to_los)
  
}

# -------------------------------------------------------------------------------------------------------------- #

#### 8. ETB. PDSCORE UNTUK NASABAH ETB ####
ETB_PDScore_final <-function(scored_data_bureau_final,scored_data_demog_final,scored_banking_final,scored_digital_final){
  
  scored_data <- merge(scored_data_demog_final,scored_data_bureau_final,by.x = "IDNumber",by.y = "CRN",all.x=T)
  scored_data_bank <- merge(scored_banking_final,scored_data,by.x="cifno",by.y="CIF",all.x = T)
  scored_data_bank_dig <- merge(scored_data_bank,scored_digital_final,by.x="cifno",by.y="cifno",all.x = T)
  scored_data1 = scored_data_bank_dig
  scored_data1$CIF = scored_data1$cifno
  ### calibration
  
  # calibrated_constants
  # cal_cust      cal_bnk   cal_bureua      cal_net 
  # -0.007953087  0.084341943 -0.197682944  0.288260659
  
  
  scored_data1$logodd_demogs <- log(scored_data1$xgb_demog/(1-scored_data1$xgb_demog))
  scored_data1$PD_demogs_temp <- scored_data1$logodd_demogs + calibrated_constants[1]
  scored_data1$PD_demogs_calib = 1/(1+exp(-scored_data1$PD_demogs_temp))
  
  scored_data1$logodd_banking <- log(scored_data1$xgb_banking/(1-scored_data1$xgb_banking))
  scored_data1$PD_banking_temp <- scored_data1$logodd_banking + calibrated_constants[2]
  scored_data1$PD_banking_calib = 1/(1+exp(-scored_data1$PD_banking_temp))
  
  scored_data1$logodd_digital <- log(scored_data1$xgb_digital/(1-scored_data1$xgb_digital))
  scored_data1$PD_digital_temp <- scored_data1$logodd_digital + calibrated_constants[4]
  scored_data1$PD_digital_calib = 1/(1+exp(-scored_data1$PD_digital_temp))
  
  scored_data1$logodd_bureau <- log(scored_data1$xgb_bureau/(1-scored_data1$xgb_bureau))
  if (scored_data1$bureau == "pefindo"){
    scored_data1$PD_bureau_temp <- scored_data1$logodd_bureau + calibrated_constants[3]
    #print(calibrated_constants[3])
  }else{
    scored_data1$PD_bureau_temp <- scored_data1$logodd_bureau + calibrated_constants[5]
    #print(calibrated_constants[5])
  }
  scored_data1$PD_bureau_calib = 1/(1+exp(-scored_data1$PD_bureau_temp))
  
  ### PD recode calibration
  # Demog
  scored_data1$logodd_demogs2 <- log(scored_data1$xgb_demog_recode/(1-scored_data1$xgb_demog_recode))
  scored_data1$PD_demogs_temp2 <- scored_data1$logodd_demogs2 + calibrated_constants[1]
  scored_data1$PD_demogs_calib2 = 1/(1+exp(-scored_data1$PD_demogs_temp2))
  
  # Bureau
  scored_data1$logodd_bureau2 <- log(scored_data1$xgb_bureau_recode/(1-scored_data1$xgb_bureau_recode))
  if (scored_data1$bureau == "pefindo"){
    scored_data1$PD_bureau_temp2 <- scored_data1$logodd_bureau2 + calibrated_constants[3]
    #print(calibrated_constants[3])
  }else{
    scored_data1$PD_bureau_temp2 <- scored_data1$logodd_bureau2 + calibrated_constants[5]
    #print(calibrated_constants[5])
  }
  scored_data1$PD_bureau_calib2 = 1/(1+exp(-scored_data1$PD_bureau_temp2))
  
  pd_database <- scored_data1[,c("ApplicationNumber","GUID","Name","MarketingCode","CustomerType","IDNumber", 
                                 "CIF","PD_demogs_calib", "PD_bureau_calib","PD_demogs_calib2", "PD_bureau_calib2",
                                 "PD_digital_calib","PD_banking_calib")]
  
  cal_pd_col_names = c("PD_demogs_calib","PD_banking_calib","PD_bureau_calib","PD_digital_calib")
  recode_cal_pd_col_names = c("PD_demogs_calib2","PD_banking_calib","PD_bureau_calib2","PD_digital_calib")
  
  ### weight redistribution and final PD calculation
  weight_matrix = c(0.1,0.45,0.35,0.1)
  
  w = data.frame(matrix(nrow = nrow(pd_database), ncol = 4))
  wgt_col_names = c("w1_cust","w2_bnk","w3_bureua","w4_net")
  names(w) = wgt_col_names
  # w[1:nrow(pd_database),] = weight_matrix
  
  for (i in 1:4){
    w[,i] <- weight_matrix[i]
  }
  
  # #Final wieghted PD after getting the right modular combination
  pd_database$pd = rowSums(pd_database[,cal_pd_col_names]*w, na.rm = T)/rowSums((!is.na(
    pd_database[,cal_pd_col_names]))* w, na.rm = T)
  # Final pd recode
  pd_database$pd_recode = rowSums(pd_database[,recode_cal_pd_col_names]*w, na.rm = T)/rowSums((!is.na(
    pd_database[,recode_cal_pd_col_names]))* w, na.rm = T)
  
  
  pd_database_to_los <- pd_database[,c("ApplicationNumber","GUID","Name","MarketingCode","CustomerType","IDNumber", "CIF","pd","pd_recode")]
  return(pd_database_to_los)
}

# -------------------------------------------------------------------------------------------------------------- #

#### 9. HELPER FUNCTION ####
# Fungsi untuk merubah list NULL menjadi memiliki nilai NA
simple_rapply <- function(x, fn)
{
  if(is.list(x))
  {
    lapply(x, simple_rapply, fn)
  } else
  {
    fn(x)
  }
}
