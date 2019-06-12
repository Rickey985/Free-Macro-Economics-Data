#Macro data
#grab_type: Latest: assuming the report date is today, what is the latest value for each of the months available
#grab_type: Available_at_that_time: what was the value that they thought of, at that time (when it was initially reported)
grab_macro_data=function(sdate=SDATE,edate=EDATE,item=c('Building_Perm','U_6_Underemploy','Infl_Expect_5_10_yrs',
                                                        'EU_CPI','US_10yr_Treasury','Spain_10yr_Govt_Bond_Yield',
                                                        'Germany_10yr_Govt_Bond_Yield','Japan_10yr_Govt_Bond_Yield',
                                                        'US_10yr_Breakeven','US_5yr_5Yr_forward_expected_inflation_rate',
                                                        'High_Yield_OAS','WTI_Crude_Oil','Gold','EUR_USD','YEN_USD',
                                                        'CNY_USD_Mild','Dow_Jones_Transports','USD_trade_weight_index',
                                                        'USD_Broad_Dollar_index','UK_10yr_Government_Bond_Yields',
                                                        'UK_10yr_Unemployment_Rate','US_Capacity_Utilization','Canada_Libor_OIS_3M_Spread',
                                                        'US_Job_Opening','CAD_USD','US_Retail_Sales_ex_Auto_Gas',
                                                        'Ave_Hourly_Earning','US_Core_PCE_CPI','ISM_PMI','BoC_Core_CPI'),
                         grab_type=c('Latest','Available_at_that_time')){
  #' @param grab_type can be either: 'Latest', or 'Available_at_that_time'
  print('Starting to grab Macro Data')
  require(fredr)
  require(rdbnomics)
  #FRED
  require(fredr)
  require(purrr)
  fredr_set_key('e2123b0a0195152b8b83dd503d60416c')
  s_id=c()
  o_SDATE=as.Date(paste(substr(sdate,1,4),substr(sdate,5,6),substr(sdate,7,8),sep='-'))
  o_EDATE=as.Date(paste(substr(edate,1,4),substr(edate,5,6),substr(edate,7,8),sep='-'))
  real_SDATE=o_SDATE
  real_EDATE=o_EDATE
  l=list()
  
  if(('Building_Perm') %in% (item)){
    s_id=c('PERMIT')
    name='Building_Perm'
    print(paste('Starting:',name))
    xyza=tryCatch({    
      if(grab_type[1]=='Latest'){
        xyz=fredr(series_id=s_id,
                  observation_start = as.Date(o_SDATE),  #this is the associated time value
                  observation_end =  as.Date(o_EDATE)
        )
        names(xyz)=c('Date','series','value')
      }
      else{
        xyz=fredr(series_id=s_id,
                  realtime_start = as.Date(real_SDATE),  
                  realtime_end = as.Date(real_EDATE), output_type = 4
        )
        Mo_s=seq(-1*nrow(xyz)+1,0,1)
        setDT(xyz)
        xyz=xyz[,Report_Date:=real_EDATE+months(Mo_s)][,c('Report_Date','series_id','value')]
      }
      xyza=xyz
    }
    ,error=function(cond){message('Error: Try increasing the time period')
      xyz=data.table(Report_date=as.numeric(),series_id=as.numeric(),value=as.numeric())
      xyza=xyz
    })
    l[[name]]=xyza
    
    
  }
  if(('U_6_Underemploy') %in% (item)){
    s_id=c('U6RATE')
    name='U_6_Underemploy'
    print(paste('Starting:',name))
    xyza=tryCatch({
      if(grab_type[1]=='Latest'){
        xyz=fredr(series_id=s_id,
                  observation_start = as.Date(o_SDATE),  #this is the associated time value
                  observation_end =  as.Date(o_EDATE)
        )
        names(xyz)=c('Date','series','value')
      }
      else{
        xyz=fredr(series_id=s_id,
                  realtime_start = as.Date(real_SDATE),  
                  realtime_end = as.Date(real_EDATE), output_type = 4
        )
        Mo_s=seq(-1*nrow(xyz)+1,0,1)
        setDT(xyz)
        xyz=xyz[,Report_Date:=real_EDATE+months(Mo_s)][,c('Report_Date','series_id','value')]
      }
      xyza=xyz
    },
    error=function(cond){message('Error: Try increasing the time period')
      xyz=data.table(Report_date=as.numeric(),series_id=as.numeric(),value=as.numeric())
      xyza=xyz
    })
    l[[name]]=xyza
    
  }
  if(('Infl_Expect_5_10_yrs') %in% (item)){
    s_id=c('GS5')
    name='Infl_Expect_5_10_yrs'
    print(paste('Starting:',name))
    xyza=tryCatch({    
      if(grab_type[1]=='Latest'){
        xyz=fredr(series_id=s_id,
                  observation_start = as.Date(o_SDATE),  #this is the associated time value
                  observation_end =  as.Date(o_EDATE)
        )
        names(xyz)=c('Date','series','value')
      }
      else{
        xyz=fredr(series_id=s_id,
                  realtime_start = as.Date(real_SDATE),  
                  realtime_end = as.Date(real_EDATE), output_type = 4
        )
        Mo_s=seq(-1*nrow(xyz)+1,0,1)
        setDT(xyz)
        xyz=xyz[,Report_Date:=real_EDATE+months(Mo_s)][,c('Report_Date','series_id','value')]
      }
      xyza=xyz}
      ,error=function(cond){
        message('Error: Try increasing the time period')
        xyz=data.table(Report_date=as.numeric(),series_id=as.numeric(),value=as.numeric())
        xyza=xyz
      })
    l[[name]]=xyza
  }
  if(('EU_CPI') %in% (item)){
    s_id=c('CPHPTT01EZM659N')
    name='EU_CPI'
    print(paste('Starting:',name))
    xyza=tryCatch({
      if(grab_type[1]=='Latest'){
        xyz=fredr(series_id=s_id,
                  observation_start = as.Date(o_SDATE),  #this is the associated time value
                  observation_end =  as.Date(o_EDATE)
        )
        names(xyz)=c('Date','series','value')
      }
      else{
        xyz=fredr(series_id=s_id,
                  realtime_start = as.Date(real_SDATE),  
                  realtime_end = as.Date(real_EDATE), output_type = 4
        )
        Mo_s=seq(-1*nrow(xyz)+1,0,1)
        setDT(xyz)
        xyz=xyz[,Report_Date:=real_EDATE+months(Mo_s)][,c('Report_Date','series_id','value')]
      }
      xyza=xyz
    },
    error=function(cond){
      message('Error: Try increasing the time period')
      xyz=data.table(Report_date=as.numeric(),series_id=as.numeric(),value=as.numeric())
      xyza=xyz
    })
    l[[name]]=xyza
    
  }
  if(('US_10yr_Treasury') %in% (item)){
    s_id=c('GS10')
    name='US_10yr_Treasury'
    print(paste('Starting:',name))
    xyza=tryCatch({
      if(grab_type[1]=='Latest'){
        xyz=fredr(series_id=s_id,
                  observation_start = as.Date(o_SDATE),  #this is the associated time value
                  observation_end =  as.Date(o_EDATE)
        )
        names(xyz)=c('Date','series','value')
      }
      else{
        xyz=fredr(series_id=s_id,
                  realtime_start = as.Date(real_SDATE),  
                  realtime_end = as.Date(real_EDATE), output_type = 4
        )
        Mo_s=seq(-1*nrow(xyz)+1,0,1)
        setDT(xyz)
        xyz=xyz[,Report_Date:=real_EDATE+months(Mo_s)][,c('Report_Date','series_id','value')]
      }
      xyza=xyz
    }
    ,error=function(cond){
      message('Error: Try increasing the time period')
      xyz=data.table(Report_date=as.numeric(),series_id=as.numeric(),value=as.numeric())
      xyza=xyz
    })
    l[[name]]=xyza
    
  }
  if(('Spain_10yr_Govt_Bond_Yield') %in% (item)){
    s_id=c('IRLTLT01ESM156N')
    name='Spain_10yr_Govt_Bond_Yield'
    print(paste('Starting:',name))
    xyza=tryCatch({
      if(grab_type[1]=='Latest'){
        xyz=fredr(series_id=s_id,
                  observation_start = as.Date(o_SDATE),  #this is the associated time value
                  observation_end =  as.Date(o_EDATE)
        )
        names(xyz)=c('Date','series','value')
      }
      else{
        xyz=fredr(series_id=s_id,
                  realtime_start = as.Date(real_SDATE),  
                  realtime_end = as.Date(real_EDATE), output_type = 4
        )
        Mo_s=seq(-1*nrow(xyz)+1,0,1)
        setDT(xyz)
        xyz=xyz[,Report_Date:=real_EDATE+months(Mo_s)][,c('Report_Date','series_id','value')]
      }
      xyza=xyz
    },
    error=function(cond){
      message('Error: Try increasing the time period')
      xyz=data.table(Report_date=as.numeric(),series_id=as.numeric(),value=as.numeric())
      xyza=xyz
    })
    l[[name]]=xyza
    
  }
  if(('Germany_10yr_Govt_Bond_Yield') %in% (item)){
    s_id=c('IRLTLT01DEM156N')
    name='Germany_10yr_Govt_Bond_Yield'
    print(paste('Starting:',name))
    xyza=tryCatch({
      if(grab_type[1]=='Latest'){
        xyz=fredr(series_id=s_id,
                  observation_start = as.Date(o_SDATE),  #this is the associated time value
                  observation_end =  as.Date(o_EDATE)
        )
        names(xyz)=c('Date','series','value')
      }
      else{
        xyz=fredr(series_id=s_id,
                  realtime_start = as.Date(real_SDATE),  
                  realtime_end = as.Date(real_EDATE), output_type = 4
        )
        Mo_s=seq(-1*nrow(xyz)+1,0,1)
        setDT(xyz)
        xyz=xyz[,Report_Date:=real_EDATE+months(Mo_s)][,c('Report_Date','series_id','value')]
      }
      xyza=xyz
    },
    error=function(cond){
      message('Error: Try increasing the time period')
      xyz=data.table(Report_date=as.numeric(),series_id=as.numeric(),value=as.numeric())
      xyza=xyz
    })
    l[[name]]=xyza
  }
  if(('Japan_10yr_Govt_Bond_Yield') %in% (item)){
    s_id=c('IRLTLT01JPM156N')
    name='Japan_10yr_Govt_Bond_Yield'
    print(paste('Starting:',name))
    xyza=tryCatch({
      if(grab_type[1]=='Latest'){
        xyz=fredr(series_id=s_id,
                  observation_start = as.Date(o_SDATE),  #this is the associated time value
                  observation_end =  as.Date(o_EDATE)
        )
        names(xyz)=c('Date','series','value')
      }
      else{
        xyz=fredr(series_id=s_id,
                  realtime_start = as.Date(real_SDATE),  
                  realtime_end = as.Date(real_EDATE), output_type = 4
        )
        Mo_s=seq(-1*nrow(xyz)+1,0,1)
        setDT(xyz)
        xyz=xyz[,Report_Date:=real_EDATE+months(Mo_s)][,c('Report_Date','series_id','value')]
      }
      xyza=xyz
    },
    error=function(cond){
      message('Error: Try increasing the time period')
      xyz=data.table(Report_date=as.numeric(),series_id=as.numeric(),value=as.numeric())
      xyza=xyz
    })
    l[[name]]=xyza
    
  }
  if(('US_10yr_Breakeven') %in% (item)){
    s_id=c('T10YIEM')
    name='US_10yr_Breakeven'
    print(paste('Starting:',name))
    xyza=tryCatch({
      if(grab_type[1]=='Latest'){
        xyz=fredr(series_id=s_id,
                  observation_start = as.Date(o_SDATE),  #this is the associated time value
                  observation_end =  as.Date(o_EDATE)
        )
        names(xyz)=c('Date','series','value')
      }
      else{
        xyz=fredr(series_id=s_id,
                  realtime_start = as.Date(real_SDATE),  
                  realtime_end = as.Date(real_EDATE), output_type = 4
        )
        Mo_s=seq(-1*nrow(xyz)+1,0,1)
        setDT(xyz)
        xyz=xyz[,Report_Date:=real_EDATE+months(Mo_s)][,c('Report_Date','series_id','value')]
      }
      xyza=xyz},
      error=function(cond){
        message('Error: Try increasing the time period')
        xyz=data.table(Report_date=as.numeric(),series_id=as.numeric(),value=as.numeric())
        xyza=xyz
      })
    l[[name]]=xyza
  }
  if(('US_5yr_5Yr_forward_expected_inflation_rate') %in% (item)){
    s_id=c('T5YIFRM')
    name='US_5yr_5Yr_forward_expected_inflation_rate'
    print(paste('Starting:',name))
    xyza=tryCatch({
      if(grab_type[1]=='Latest'){
        xyz=fredr(series_id=s_id,
                  observation_start = as.Date(o_SDATE),  #this is the associated time value
                  observation_end =  as.Date(o_EDATE)
        )
        names(xyz)=c('Date','series','value')
      }
      else{
        xyz=fredr(series_id=s_id,
                  realtime_start = as.Date(real_SDATE),  
                  realtime_end = as.Date(real_EDATE),output_type = 4
        )
        Mo_s=seq(-1*nrow(xyz)+1,0,1)
        setDT(xyz)
        xyz=xyz[,Report_Date:=real_EDATE+months(Mo_s)][,c('Report_Date','series_id','value')]
      }
      xyza=xyz
    },
    error=function(cond){
      message('Error: Try increasing the time period')
      xyz=data.table(Report_date=as.numeric(),series_id=as.numeric(),value=as.numeric())
      xyza=xyz
    })
    l[[name]]=xyza
  }
  if(('High_Yield_OAS') %in% (item)){
    s_id=c('BAMLH0A0HYM2')
    name='High_Yield_OAS'
    print(paste('Starting:',name))
    
    xyza=tryCatch({
      if(grab_type[1]=='Latest'){
        xyz=fredr(series_id=s_id,
                  observation_start = as.Date(o_SDATE),  #this is the associated time value
                  observation_end =  as.Date(o_EDATE)
        )
        names(xyz)=c('Date','series','value')
      }
      else{
        xyz=fredr(series_id=s_id,
                  realtime_start = as.Date(real_SDATE),  
                  realtime_end = as.Date(real_EDATE),output_type = 4
        )
        Mo_s=seq(-1*nrow(xyz)+1,0,1)
        setDT(xyz)
        xyz=xyz[,Report_Date:=date][,c('Report_Date','series_id','value')]
      }
      xyza=xyz},
      error=function(cond){
        message('Error: Try increasing the time period')
        xyz=data.table(Report_date=as.numeric(),series_id=as.numeric(),value=as.numeric())
        xyza=xyz
      })
    l[[name]]=xyza
    
  }
  if(('WTI_Crude_Oil') %in% (item)){
    s_id=c('DCOILWTICO')
    name='WTI_Crude_Oil'
    print(paste('Starting:',name))
    xyza=tryCatch({
      if(grab_type[1]=='Latest'){
        xyz=fredr(series_id=s_id,
                  observation_start = as.Date(o_SDATE),  #this is the associated time value
                  observation_end =  as.Date(o_EDATE)
        )
        names(xyz)=c('Date','series','value')
      }
      else{
        xyz=fredr(series_id=s_id,
                  realtime_start = as.Date(real_SDATE),  
                  realtime_end = as.Date(real_EDATE),output_type = 4
        )
        Mo_s=seq(-1*nrow(xyz)+1,0,1)
        setDT(xyz)
        xyz=xyz[,Report_Date:=date][,c('Report_Date','series_id','value')]
      }
      xyza=xyz
    },
    error=function(cond){
      message('Error: Try increasing the time period')
      xyz=data.table(Report_date=as.numeric(),series_id=as.numeric(),value=as.numeric())
      xyza=xyz
    })
    l[[name]]=xyza
    
  }
  if(('Gold') %in% (item)){
    s_id=c('GOLDAMGBD228NLBM')
    name='Gold'
    print(paste('Starting:',name))
    xyza=tryCatch({
      if(grab_type[1]=='Latest'){
        xyz=fredr(series_id=s_id,
                  observation_start = as.Date(o_SDATE),  #this is the associated time value
                  observation_end =  as.Date(o_EDATE)
        )
        names(xyz)=c('Date','series','value')
      }
      else{
        xyz=fredr(series_id=s_id,
                  realtime_start = as.Date(real_SDATE),  
                  realtime_end = as.Date(real_EDATE),output_type = 4
        )
        Mo_s=seq(-1*nrow(xyz)+1,0,1)
        setDT(xyz)
        xyz=xyz[,Report_Date:=date][,c('Report_Date','series_id','value')]
      }
      xyza=xyz
    },
    error=function(cond){
      message('Error: Try increasing the time period')
      xyz=data.table(Report_date=as.numeric(),series_id=as.numeric(),value=as.numeric())
      xyza=xyz
    })
    l[[name]]=xyza
  }
  if(('EUR_USD') %in% (item)){
    s_id=c('EXUSEU')
    name='EUR_USD'
    print(paste('Starting:',name))
    xyza=tryCatch({
      if(grab_type[1]=='Latest'){
        xyz=fredr(series_id=s_id,
                  observation_start = as.Date(o_SDATE),  #this is the associated time value
                  observation_end =  as.Date(o_EDATE)
        )
        names(xyz)=c('Date','series','value')
      }
      else{
        xyz=fredr(series_id=s_id,
                  realtime_start = as.Date(real_SDATE),  
                  realtime_end = as.Date(real_EDATE),output_type = 4
        )
        Mo_s=seq(-1*nrow(xyz)+1,0,1)
        setDT(xyz)
        xyz=xyz[,Report_Date:=real_EDATE+months(Mo_s)][,c('Report_Date','series_id','value')]
      }
      xyza=xyz
    },
    error=function(cond){
      message('Error: Try increasing the time period')
      xyz=data.table(Report_date=as.numeric(),series_id=as.numeric(),value=as.numeric())
      xyza=xyz
    })
    l[[name]]=xyza
  }
  if(('YEN_USD') %in% (item)){
    s_id=c('DEXJPUS')
    name='YEN_USD'
    print(paste('Starting:',name))
    xyza=tryCatch({
      if(grab_type[1]=='Latest'){
        xyz=fredr(series_id=s_id,
                  observation_start = as.Date(o_SDATE),  #this is the associated time value
                  observation_end =  as.Date(o_EDATE)
        )
        names(xyz)=c('Date','series','value')
      }
      else{
        xyz=fredr(series_id=s_id,
                  realtime_start = as.Date(real_SDATE),  
                  realtime_end = as.Date(real_EDATE),output_type = 4
        )
        Mo_s=seq(-1*nrow(xyz)+1,0,1)
        setDT(xyz)
        xyz=xyz[,Report_Date:=date][,c('Report_Date','series_id','value')]
      }
      xyza=xyz
    },
    error=function(cond){
      message('Error: Try increasing the time period')
      xyz=data.table(Report_date=as.numeric(),series_id=as.numeric(),value=as.numeric())
      xyza=xyz
    })
    l[[name]]=xyza
    
  }
  if(('CNY_USD_Mild') %in% (item)){
    s_id=c('EXCHUS')
    name='CNY_USD_Mild'
    print(paste('Starting:',name))
    xyza=tryCatch({
      if(grab_type[1]=='Latest'){
        xyz=fredr(series_id=s_id,
                  observation_start = as.Date(o_SDATE),  #this is the associated time value
                  observation_end =  as.Date(o_EDATE)
        )
        names(xyz)=c('Date','series','value')
      }
      else{
        xyz=fredr(series_id=s_id,
                  realtime_start = as.Date(real_SDATE),  
                  realtime_end = as.Date(real_EDATE),output_type = 4
        )
        Mo_s=seq(-1*nrow(xyz)+1,0,1)
        setDT(xyz)
        xyz=xyz[,Report_Date:=real_EDATE+months(Mo_s)][,c('Report_Date','series_id','value')]
      }
      xyza=xyz
    },
    error=function(cond){
      message('Error: Try increasing the time period')
      xyz=data.table(Report_date=as.numeric(),series_id=as.numeric(),value=as.numeric())
      xyza=xyz
    })
    
    l[[name]]=xyza
  }
  if(('Dow_Jones_Transports') %in% (item)){
    s_id=c('DJTA')
    name='Dow_Jones_Transports'
    print(paste('Starting:',name))
    xyza=tryCatch({
      if(grab_type[1]=='Latest'){
        xyz=fredr(series_id=s_id,
                  observation_start = as.Date(o_SDATE),  #this is the associated time value
                  observation_end =  as.Date(o_EDATE)
        )
        names(xyz)=c('Date','series','value')
      }
      else{
        xyz=data.table(Report_date=as.numeric(),series_id=as.numeric(),value=as.numeric())
      }
      xyza=xyz
    },
    error=function(cond){
      message('Error: Try increasing the time period')
      xyz=data.table(Report_date=as.numeric(),series_id=as.numeric(),value=as.numeric())
      xyza=xyz
    })
    
    l[[name]]=xyza
  }
  if(('USD_trade_weight_index') %in% (item)){
    s_id=c('DTWEXM')
    name='USD_trade_weight_index'
    print(paste('Starting:',name))
    xyza=tryCatch({
      if(grab_type[1]=='Latest'){
        xyz=fredr(series_id=s_id,
                  observation_start = as.Date(o_SDATE),  #this is the associated time value
                  observation_end =  as.Date(o_EDATE)
        )
        names(xyz)=c('Date','series','value')
      }
      else{
        xyz=fredr(series_id=s_id,
                  realtime_start = as.Date(real_SDATE),  
                  realtime_end = as.Date(real_EDATE),output_type = 4
        )
        Mo_s=seq(-1*nrow(xyz)+1,0,1)
        setDT(xyz)
        xyz=xyz[,Report_Date:=date][,c('Report_Date','series_id','value')]
      }
      xyza=xyz
    },
    error=function(cond){
      message('Error: Try increasing the time period')
      xyz=data.table(Report_date=as.numeric(),series_id=as.numeric(),value=as.numeric())
      xyza=xyz
    })
    
    l[[name]]=xyza
  }
  if(('USD_Broad_Dollar_index') %in% (item)){
    s_id=c('TWEXB')
    name='USD_Broad_Dollar_index'
    print(paste('Starting:',name))
    xyza=tryCatch({
      if(grab_type[1]=='Latest'){
        xyz=fredr(series_id=s_id,
                  observation_start = as.Date(o_SDATE),  #this is the associated time value
                  observation_end =  as.Date(o_EDATE)
        )
        names(xyz)=c('Date','series','value')
      }
      else{
        xyz=fredr(series_id=s_id,
                  realtime_start = as.Date(real_SDATE),  
                  realtime_end = as.Date(real_EDATE),output_type = 4
        )
        Mo_s=seq(-1*nrow(xyz)+1,0,1)
        setDT(xyz)
        xyz=xyz[,Report_Date:=date][,c('Report_Date','series_id','value')]
      }
      xyza=xyz
    },
    error=function(cond){
      message('Error: Try increasing the time period')
      xyz=data.table(Report_date=as.numeric(),series_id=as.numeric(),value=as.numeric())
      xyza=xyz
    })
    
    l[[name]]=xyza
  }
  if(('UK_10yr_Government_Bond_Yields') %in% (item)){
    s_id=c('IRLTLT01GBM156N')
    name='UK_10yr_Government_Bond_Yields'
    print(paste('Starting:',name))
    xyza=tryCatch({
      if(grab_type[1]=='Latest'){
        xyz=fredr(series_id=s_id,
                  observation_start = as.Date(o_SDATE),  #this is the associated time value
                  observation_end =  as.Date(o_EDATE)
        )
        names(xyz)=c('Date','series','value')
      }
      else{
        xyz=fredr(series_id=s_id,
                  realtime_start = as.Date(real_SDATE),  
                  realtime_end = as.Date(real_EDATE),output_type = 4
        )
        Mo_s=seq(-1*nrow(xyz)+1,0,1)
        setDT(xyz)
        xyz=xyz[,Report_Date:=real_EDATE+months(Mo_s)][,c('Report_Date','series_id','value')]
      }
      xyza=xyz
    },
    error=function(cond){
      message('Error: Try increasing the time period')
      xyz=data.table(Report_date=as.numeric(),series_id=as.numeric(),value=as.numeric())
      xyza=xyz
    })
    
    l[[name]]=xyza
  }
  if(('UK_10yr_Unemployment_Rate') %in% (item)){
    s_id=c('LMUNRRTTGBM156N')
    name='UK_10yr_Unemployment_Rate'
    print(paste('Starting:',name))
    xyza=tryCatch({
      if(grab_type[1]=='Latest'){
        xyz=fredr(series_id=s_id,
                  observation_start = as.Date(o_SDATE),  #this is the associated time value
                  observation_end =  as.Date(o_EDATE)
        )
        names(xyz)=c('Date','series','value')
      }
      else{
        xyz=fredr(series_id=s_id,
                  realtime_start = as.Date(real_SDATE),  
                  realtime_end = as.Date(real_EDATE),output_type = 4
        )
        Mo_s=seq(-1*nrow(xyz)+1,0,1)
        setDT(xyz)
        xyz=xyz[,Report_Date:=real_EDATE+months(Mo_s)][,c('Report_Date','series_id','value')]
      }
      xyza=xyz
    },
    error=function(cond){
      message('Error: Try increasing the time period')
      xyz=data.table(Report_date=as.numeric(),series_id=as.numeric(),value=as.numeric())
      xyza=xyz
    })
    
    l[[name]]=xyza
  }
  if(('US_Capacity_Utilization') %in% (item)){
    s_id=c('TCU')
    name='US_Capacity_Utilization'
    print(paste('Starting:',name))
    xyza=tryCatch({
      if(grab_type[1]=='Latest'){
        xyz=fredr(series_id=s_id,
                  observation_start = as.Date(o_SDATE),  #this is the associated time value
                  observation_end =  as.Date(o_EDATE)
        )
        names(xyz)=c('Date','series','value')
      }
      else{
        xyz=fredr(series_id=s_id,
                  realtime_start = as.Date(real_SDATE),  
                  realtime_end = as.Date(real_EDATE),output_type = 4
        )
        Mo_s=seq(-1*nrow(xyz)+1,0,1)
        setDT(xyz)
        xyz=xyz[,Report_Date:=real_EDATE+months(Mo_s)][,c('Report_Date','series_id','value')]
      }
      xyza=xyz
    },
    error=function(cond){
      message('Error: Try increasing the time period')
      xyz=data.table(Report_date=as.numeric(),series_id=as.numeric(),value=as.numeric())
      xyza=xyz
    })
    
    l[[name]]=xyza
  }
  if(('Canada_Libor_OIS_3M_Spread') %in% (item)){
    s_id=c('CAD3MTD156N')
    name='Canada_Libor_OIS_3M_Spread'
    print(paste('Starting:',name))
    xyza=tryCatch({
      if(grab_type[1]=='Latest'){
        xyz=fredr(series_id=s_id,
                  observation_start = as.Date(o_SDATE),  #this is the associated time value
                  observation_end =  as.Date(o_EDATE)
        )
        names(xyz)=c('Date','series','value')
      }
      else{
        xyz=data.table(Report_date=as.numeric(),series_id=as.numeric(),value=as.numeric())
      }
      xyza=xyz
    },
    error=function(cond){
      message('Error: Try increasing the time period')
      xyz=data.table(Report_date=as.numeric(),series_id=as.numeric(),value=as.numeric())
      xyza=xyz
    })
    
    l[[name]]=xyza
  }
  
  
  if('US_Job_Opening' %in% item){
    print(paste('Starting:','US_Job_Opening'))
    name='US_Job_Opening'
    xyza=tryCatch({
      if(grab_type[1]=='Latest'){
        a=fredr(series_id='JTS1000JOL',
                observation_start = as.Date(o_SDATE),
                observation_end =  as.Date(o_EDATE)
        )
        b=fredr(series_id='JTS9000JOL',
                observation_start = as.Date(o_SDATE),
                observation_end =  as.Date(o_EDATE)
        )
        xyz=setDT(merge(a,b,by=c('date')))[,value:=value.x+value.y][,series:='US_Job_Opening']
        xyz=xyz[,c('date','series','value')]
        names(xyz)=c('Date','series','value')
      }
      else{
        a=fredr(series_id='JTS1000JOL',
                realtime_start = as.Date(o_SDATE),
                realtime_end = as.Date(o_EDATE), output_type = 4
        )
        setDT(a)
        b=fredr(series_id='JTS9000JOL',
                realtime_start = as.Date(o_SDATE),
                realtime_end = as.Date(o_EDATE), output_type = 4
        )
        setDT(b)
        xyz=setDT(merge(a,b,by=c('date')))[,value:=value.x+value.y][,series:='US_Job_Opening']
        Mo_s=seq(-1*nrow(xyz)+1,0,1)
        setDT(xyz)
        xyz=xyz[,Report_Date:=real_EDATE+months(Mo_s)]
        xyz=xyz[,c('Report_Date','series','value')]
      }
      xyza=xyz
    },
    error=function(cond){
      message('Error: Try increasing the time period')
      xyz=data.table(Report_date=as.numeric(),series_id=as.numeric(),value=as.numeric())
      xyza=xyz
    })
    
    l[[name]]=xyza
    
  }
  if(('CAD_USD')  %in% item){
    print(paste('Starting:','CAD_USD'))
    name='CAD_USD'
    xyza=tryCatch({
      if(grab_type[1]=='Latest'){
        a=fredr(series_id='DEXCAUS',
                observation_start = as.Date(o_SDATE),
                observation_end =  as.Date(o_EDATE)
        )
        xyz=setDT(a)[,value1:=1/value]
        xyz=xyz[,c('date','series_id','value1')]
        names(xyz)=c('Date','series_id','value')
      }
      else{
        a=fredr(series_id='DEXCAUS',
                realtime_start = as.Date(o_SDATE),
                realtime_end = as.Date(o_EDATE),output_type = 4
        )
        xyz=setDT(a)[,value1:=1/value][,Report_Date:=date][,c('Report_Date','series_id','value1')]
      }
      xyza=xyz
    },
    error=function(cond){
      message('Error: Try increasing the time period')
      xyz=data.table(Report_date=as.numeric(),series_id=as.numeric(),value=as.numeric())
      xyza=xyz
    })
    
    l[[name]]=xyza
    
  }
  if(('US_Retail_Sales_ex_Auto_Gas')  %in% item){
    print(paste('Starting:','US_Retail_Sales_ex_Auto_Gas'))
    name='US_Retail_Sales_ex_Auto_Gas'
    xyza=tryCatch({
      if(grab_type[1]=='Latest'){
        a=fredr(series_id='MRTSSM44W72USS',
                observation_start = as.Date(o_SDATE),
                observation_end =  as.Date(o_EDATE)
        )
        xyz=setDT(a)[,value1:=value/1000]
        xyz=xyz[,c('date','series_id','value1')]
        names(xyz)=c('Date','series','value')
      }
      else{
        a=fredr(series_id='MRTSSM44W72USS',
                realtime_start = as.Date(o_SDATE),
                realtime_end = as.Date(o_EDATE), output_type = 4
        )
        xyz=setDT(a)[,value1:=value/1000]
        Mo_s=seq(-1*nrow(xyz)+1,0,1)
        xyz=xyz[,Report_Date:=real_EDATE+months(Mo_s)][,c('Report_Date','series_id','value1')]
        
      }
      xyza=xyz
    },
    error=function(cond){
      message('Error: Try increasing the time period')
      xyz=data.table(Report_date=as.numeric(),series_id=as.numeric(),value=as.numeric())
      xyza=xyz
    })
    
    l[[name]]=xyza
    
  }
  if(('Ave_Hourly_Earning')  %in% item){
    print(paste('Starting:','Ave_Hourly_Earnings'))
    xyza=tryCatch({
      if(grab_type[1]=='Latest'){
        temp_SDATE=paste(substr(sdate-10000,1,4),substr(sdate-10000,5,6),substr(sdate-10000,7,8),sep='-')
        
        a=setDT(fredr(series_id='CES0500000003',
                      observation_start = as.Date(temp_SDATE),
                      observation_end =  as.Date(o_EDATE)
        ))[,lagged:=shift(value,12,typ='lag')][,value1:=value/lagged]
        b=setDT(fredr(series_id='CES0500000003',
                      observation_start = as.Date(o_SDATE),
                      observation_end =  as.Date(o_EDATE)
        ))
        xyz=merge(a,b,by=c('date'))
        
        xyz=xyz[,c('date','series_id.x','value1')]
        names(xyz)=c('Date','series','value')
        
      }
      else{
        temp_SDATE=paste(substr(sdate-10000,1,4),substr(sdate-10000,5,6),substr(sdate-10000,7,8),sep='-')
        
        a=setDT(fredr(series_id='CES0500000003',
                      realtime_start = as.Date(o_SDATE)-years(2),
                      realtime_end = as.Date(o_EDATE), output_type = 4
        ))[,lagged:=shift(value,12,typ='lag')][,value1:=value/lagged]
        b=setDT(fredr(series_id='CES0500000003',
                      realtime_start = as.Date(o_SDATE)-years(2),
                      realtime_end = as.Date(o_EDATE), output_type = 4
        ))
        xyz=merge(a,b,by=c('date'))
        Mo_s=seq(-1*nrow(xyz)+1,0,1)
        xyz=xyz[,Report_Date:=real_EDATE+months(Mo_s)][,c('Report_Date','series_id.x','value1')]
        names(xyz)=c('Date','series','value')
      }
      xyza=xyz
    },
    error=function(cond){
      message('Error: Try increasing the time period')
      xyz=data.table(Report_date=as.numeric(),series_id=as.numeric(),value=as.numeric())
      xyza=xyz
    })
    
    l[[name]]=xyza
  }
  if(('US_Core_PCE_CPI')  %in% item){
    print(paste('Starting:','US_Core_PCE_CPI'))
    name='US_Core_PCE_CPI'
    xyza=tryCatch({
      if(grab_type[1]=='Latest'){
        temp_SDATE=paste(substr(sdate-10000,1,4),substr(sdate-10000,5,6),substr(sdate-10000,7,8),sep='-')
        
        
        a=setDT(fredr(series_id='PCEPILFE',
                      observation_start = as.Date(temp_SDATE),
                      observation_end =  as.Date(o_EDATE)
        ))[,lagged:=shift(value,12,typ='lag')][,value1:=value/lagged]
        b=setDT(fredr(series_id='PCEPILFE',
                      observation_start = as.Date(o_SDATE),
                      observation_end =  as.Date(o_EDATE)
        ))
        xyz=merge(a,b,by=c('date'))
        
        xyz=xyz[,c('date','series_id.x','value1')]
        names(xyz)=c('Date','series','value')
        
      }
      else{
        temp_SDATE=paste(substr(sdate-10000,1,4),substr(sdate-10000,5,6),substr(sdate-10000,7,8),sep='-')
        a=setDT(fredr(series_id='PCEPILFE',
                      realtime_start = as.Date(o_SDATE)-years(2),
                      realtime_end = as.Date(o_EDATE),output_type = 4
        ))[,lagged:=shift(value,12,typ='lag')][,value1:=value/lagged]
        b=setDT(fredr(series_id='PCEPILFE',
                      realtime_start = as.Date(o_SDATE)-years(2),
                      realtime_end = as.Date(o_EDATE),output_type = 4
        ))
        xyz=merge(a,b,by=c('date'))
        Mo_s=seq(-1*nrow(xyz)+1,0,1)
        xyz=xyz[,Report_Date:=real_EDATE+months(Mo_s)][,c('Report_Date','series_id.x','value1')]
        names(xyz)=c('Date','series','value')
        
      }
      xyza=xyz
    },
    error=function(cond){
      message('Error: Try increasing the time period')
      xyz=data.table(Report_date=as.numeric(),series_id=as.numeric(),value=as.numeric())
      xyza=xyz
    })
    
    l[[name]]=xyza
  }
  if(('ISM_PMI')  %in% item){
    print(paste('Starting:','ISM_PMI'))
    name='ISM_PMI'
    xyza=tryCatch({
      if(grab_type[1]=='Latest'){
        xyz=rdb(ids='ISM/pmi/pm')[,c('period','dataset_code','value')]
        setDT(xyz)[,t_period:=period+months(1)]
        xyz=xyz[,c('t_period','dataset_code','value')]
        names(xyz)=c('Date','series','value')
      }
      else{
        message('Error: cannot use Available at that time with ISM_PMI')
        xyz=data.table(Report_date=as.numeric(),series_id=as.numeric(),value=as.numeric())
      }
      xyza=xyz
    },
    error=function(cond){
      message('Error: Try increasing the time period')
      xyz=data.table(Report_date=as.numeric(),series_id=as.numeric(),value=as.numeric())
      xyza=xyz
    })
    
    l[[name]]=xyza
    
    
  }
  if(('BoC_Core_CPI')  %in% item){
    print(paste('Starting:','BoC_Core_CPI'))
    name='BoC_Core_CPI'
    xyza=tryCatch({
      if(grab_type[1]=='Latest'){
        temp_SDATE=paste(substr(sdate-10000,1,4),substr(sdate-10000,5,6),substr(sdate-10000,7,8),sep='-')
        
        a=setDT(fredr(series_id='CPALCY01CAM661N',
                      observation_start = as.Date(temp_SDATE),
                      observation_end =  as.Date(o_EDATE)
        ))[,lagged:=shift(value,12,typ='lag')][,value1:=value/lagged]
        b=setDT(fredr(series_id='CPALCY01CAM661N',
                      observation_start = as.Date(o_SDATE),
                      observation_end =  as.Date(o_EDATE)
        ))
        xyz=merge(a,b,by=c('date'))
        
        
        xyz=xyz[,c('date','series_id.x','value1')]
        names(xyz)=c('Date','series','value')
        
      }
      else{
        temp_SDATE=paste(substr(sdate-10000,1,4),substr(sdate-10000,5,6),substr(sdate-10000,7,8),sep='-')
        
        a=setDT(fredr(series_id='CPALCY01CAM661N',
                      realtime_start = as.Date(temp_SDATE)-years(2),
                      realtime_end = as.Date(o_EDATE),output_type = 4
        ))[,lagged:=shift(value,12,typ='lag')][,value1:=value/lagged]
        b=setDT(fredr(series_id='CPALCY01CAM661N',
                      realtime_start = as.Date(o_SDATE)-years(2),
                      realtime_end = as.Date(o_EDATE),output_type = 4
        ))
        xyz=merge(a,b,by=c('date'))
        Mo_s=seq(-1*nrow(xyz)+1,0,1)
        xyz=xyz[,Report_Date:=real_EDATE+months(Mo_s)][,c('Report_Date','series_id.x','value1')]
        names(xyz)=c('Date','series','value')
        
      }
      xyza=xyz
    },
    error=function(cond){
      message('Error: Try increasing the time period')
      xyz=data.table(Report_date=as.numeric(),series_id=as.numeric(),value=as.numeric())
      xyza=xyz
    })
    
    l[[name]]=xyza
    
  }
  
  return(l) 
}


search_macro_database=function(search='unemployment'){
  print('Starting to grab Macro Data')
  require(fredr)
  require(rdbnomics)
  #FRED
  require(fredr)
  require(purrr)
  fredr_set_key('e2123b0a0195152b8b83dd503d60416c')
  s_id=c()
  l=list()
  xyza=tryCatch({
    xyz=fredr_series_search_text(search_text = search,limit=100)[,c('id','title','observation_start','observation_end','frequency')]
    return(xyz)
  },
  error=function(cond){
    xyz=data.table(id=numeric(),title=numeric(),observation_start=numeric(),observation_end=numeric(),frequency=numeric())
    return(xyz)
  })
  
  return(xyza)
}

grab_macro_manually=function(identity='UNRATE',sdate=SDATE,edate=EDATE){
  print('Starting to grab Macro Data')
  require(fredr)
  require(rdbnomics)
  #FRED
  require(fredr)
  require(purrr)
  fredr_set_key('e2123b0a0195152b8b83dd503d60416c')
  s_id=c()
  o_SDATE=as.Date(paste(substr(sdate,1,4),substr(sdate,5,6),substr(sdate,7,8),sep='-'))
  o_EDATE=as.Date(paste(substr(edate,1,4),substr(edate,5,6),substr(edate,7,8),sep='-'))
  real_SDATE=o_SDATE
  real_EDATE=o_EDATE
  l=list()
  xyza=tryCatch({
    xyz=fredr(series_id=identity,
              observation_start = as.Date(o_SDATE),  #this is the associated time value
              observation_end =  as.Date(o_EDATE)
    )
    names(xyz)=c('Date','series','value')
    return(xyz)
  },
  error=function(cond){
    xyz=data.table(Date=as.numeric(),series=as.numeric(),value=as.numeric())
    return(xyz)
  })
  return(xyza)

}



