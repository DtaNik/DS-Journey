# -*- coding: utf-8 -*-
"""
Created on Mon Apr 30 14:18:26 2018

@author: A0744955
"""

#Import all the libraries
import pyodbc
import pandas as pd
import numpy as np
from statsmodels.tsa.statespace.sarimax import SARIMAX
#from array import array
from pandas import datetime
import warnings
#from datetime import datetime
#import matplotlib.pylab as plt
#from matplotlib import pyplot
from statsmodels.tsa.stattools import adfuller

#################################################################################################################################
##Defining Functions

##Function for changing date format
dateparse = lambda dates: pd.datetime.strptime(dates, '%Y-%m')
dateparse1 = lambda dates: pd.datetime.strptime(dates, '%Y-%m-%d')

#Final model evaluator for Script 1(cluster1)
def eval_mod_c1(X,arima_order,s_order,Trend):
    model= SARIMAX(X,order=arima_order,seasonal_order=s_order,trend=Trend)
    model_fit=model.fit(disp=0)
    error1=model_fit.aic
    error2=model_fit.bic
    return error1,error2

#Final model evaluator for Script 2(cluster2)
def eval_mod_c2(X,arima_order,s_order,Trend):
    model= SARIMAX(X,order=arima_order,seasonal_order=s_order,trend=Trend,enforce_stationarity=False,enforce_invertibility=False)
    model_fit=model.fit(disp=0)
    error1=model_fit.aic
    error2=model_fit.bic
    return error1,error2

#Creating Array
def numpy_make_grid(*args):
    a= np.ones([len(arg) for arg in args])
    b=[(ix*a)[None] for ix in np.ix_(*args)]
    c= np.concatenate(b).reshape((len(args),-1)).transpose()
    return c

ar=numpy_make_grid([0,1,2,3],[0,1,2],[0,1,2,3],[0,1,2],[0,1],[0,1,2],[12,24])
ar=ar.astype('int')
ar.shape

#Creating trend component input
Trend=pd.DataFrame(columns=['trend'])
Trend['trend']=['n','c','ct']

#Looping with arrays for Script 1 and 3
def mod_eval_c1(dataset,a=[]):
    dataset=dataset.astype('float32')
    best_score_aic,best_score_bic,best_cfg,best_s_cfg,trend=float("inf"),float("inf"),None,None,None
    for i in range(len(a)):
        for j in range(len(Trend)):
            order=(a[i,0],a[i,1],a[i,2])
            s_order=(a[i,3],a[i,4],a[i,5],a[i,6])
            try:
                aic=eval_mod_c1(dataset,order,s_order,Trend['trend'].iloc[j])
                if aic[0]<best_score_aic:
                    best_score_aic,best_score_bic,best_cfg,best_s_cfg,trend=aic[0],aic[1],order,s_order,Trend['trend'].iloc[j]
                print('ARIMA%s Seasonal%s AIC=%.3f BIC=%.3f Trend= %s' %(order,s_order,aic[0],aic[1],Trend['trend'].iloc[j]))
            except:
                continue
    print('Best ARIMA%s Seasonal%s AIC=%.3f BIC=%.3f Trend= %s' %(best_cfg,best_s_cfg,best_score_aic,best_score_bic,trend))
    return best_cfg,best_s_cfg,trend

#Looping with arrays for Script 2 and 3
def mod_eval_c2(dataset,a=[]):
    dataset=dataset.astype('float32')
    best_score_aic,best_score_bic,best_cfg,best_s_cfg,trend=float("inf"),float("inf"),None,None,None
    for i in range(len(a)):
        order=(a[i,0],a[i,1],a[i,2])
        s_order=(a[i,3],a[i,4],a[i,5],a[i,6])
        for j in range(len(Trend)):
            try:
                aic=eval_mod_c2(dataset,order,s_order,Trend['trend'].iloc[j])
                if aic[0]<best_score_aic:
                    best_score_aic,best_score_bic,best_cfg,best_s_cfg,trend=aic[0],aic[1],order,s_order,Trend['trend'].iloc[j]
                print('ARIMA%s Seasonal%s AIC=%.3f BIC=%.3f Trend= %s' %(order,s_order,aic[0],aic[1],Trend['trend'].iloc[j]))
            except:
                continue
    print('Best ARIMA%s Seasonal%s AIC=%.3f BIC=%.3f Trend= %s' %(best_cfg,best_s_cfg,best_score_aic,best_score_bic,trend))
    return best_cfg,best_s_cfg,trend

#####################################################################################################################################

#####Setting Date range for data import#####
start_date= datetime((datetime.now().year-3),1,1)

end_date=datetime((datetime.now().year)-1,12,31)


##Importing Flagged Client wise Data
qrystrng="Select * from [dbo].[Webchat_Flag_Data] "

df_clients = pd.read_sql(qrystrng, pyodbc.connect('Trusted_Connection=yes', 
                     driver = '{SQL Server}',
                     server = 'GGNWIDDB2871,13163', 
                     database = 'Analytics_DB'))

print(df_clients[['ClientID','ClientNm']])


##Inputing Client ID and Client Name
ClientID= str(raw_input('Enter the Client ID: '))

Client= df_clients.loc[df_clients['ClientID']==ClientID]

Name= Client['ClientNm']
Name.index=range(0,1)


##Input from User for running script i.e. auto run or manual run
resp_runtype= str(raw_input('Do you want to autorun the script or manipulate the clusters?; Autorun/Manual : '))

if resp_runtype=='Autorun':
    flag=0
else:
    flag=1

   
##Importing Client Data in dataframe 'df'
querystring= ("select format([date],'yyyy-MM') as 'month',"+
               "sum(Offered) as 'Chats',"+
               "avg(AST_in_Mins) as 'AST'"+
               "from [dbo].[Webchat Data Actual]"+
               "where ClientID like '"+ClientID+
               "' and [Date] between '" 
               +start_date.strftime('%Y-%m-%d')+"' and '" 
               +end_date.strftime('%Y-%m-%d')+ "'"+
               "group by format([Date],'yyyy-MM')"+
               " order by format([Date],'yyyy-MM')")


df = pd.read_sql(querystring, pyodbc.connect('Trusted_Connection=yes', 
                     driver = '{SQL Server}',
                     server = 'GGNWIDDB2871,13163', 
                     database = 'Analytics_DB'))


##Importing population count in dataframe 'df_ppt'
querystring_ppt= ("select * from [Webchat Ppt Count] where [Client ID] like '"+ClientID+"'")

df_ppt = pd.read_sql(querystring_ppt, pyodbc.connect('Trusted_Connection=yes', 
                     driver = '{SQL Server}',
                     server = 'GGNWIDDB2871,13163', 
                     database = 'Analytics_DB'))
    



#Creating Factor field in Data frame (Factor= Chat/Population count)
for i in range(len(df['month'])):
    df.iloc[i, 0]= dateparse(df.iloc[i, 0])


if df_ppt.empty==False and pd.isnull(df_ppt.loc[0,'PPT 2016'])==False:
    ppt_count_2016=df_ppt['PPT 2016'] 
    if pd.isnull(df_ppt.loc[0,'PPT 2015'])==True:
        ppt_count_2015= ppt_count_2016
    else:
        ppt_count_2015=df_ppt['PPT 2015']
    
    ppt_count_2017=df_ppt['PPT 2017'] 
    ppt_count_2018=df_ppt['PPT 2018'] 

    df['Factors']= None
    df['Factors']=df['Factors'].astype('float32')
    df['Chats']=df['Chats'].astype('float32')
    
    for i in range(len(df['month'])):
        if df.iloc[i,0].year==2017:
            df.iloc[i,3]=df.iloc[i,1]/ppt_count_2017[0]
        else:
            if df.iloc[i,0].year==2016:
                df.iloc[i,3]=df.iloc[i,1]/ppt_count_2016[0]
            else:
                if df.iloc[i,0].year==2015:
                    df.iloc[i,3]=df.iloc[i,1]/ppt_count_2015[0]
                else:
                    if df.iloc[i,0].year==2018:
                        df.iloc[i,3]=df.iloc[i,1]/ppt_count_2018[0]


##Defining dataframes based on clusters
df_cluster3=pd.DataFrame
df_cluster2= pd.DataFrame
df_cluster4= pd.DataFrame
df_cluster5= pd.DataFrame

df_cluster_comp= pd.DataFrame(columns=['Cluster','DF']) #Cluster for computing best cluster

if len(df)>=36:
    df_cluster2=df[(df.month>=datetime(2015,1,1))]
    df_cluster3=df[(df.month>=datetime(2015,10,1))]
    df_cluster4=df[(df.month>=datetime(2016,1,1))]
    df_cluster5=df[(df.month>=datetime(2017,1,1))]
else: 
    if len(df)<36 and len(df)>=27:
        df_cluster3=df[(df.month>=datetime(2015,10,1))]
        df_cluster4=df[(df.month>=datetime(2016,1,1))]
        df_cluster5=df[(df.month>=datetime(2017,1,1))]
    else:
        if len(df)>=24 and len(df)<27:
            df_cluster4=df[(df.month>=datetime(2016,1,1))]
            df_cluster5=df[(df.month>=datetime(2017,1,1))]
        else:
            if len(df)<24 and len(df)>=12:
                df_cluster5=df[(df.month>=datetime(2017,1,1))]


######Cluster 2#####
if df_cluster2.empty==False:
    if len(df_cluster2.columns)==3:
        ts_cluster2= df_cluster2['Chats']
        ts_cluster2.index=df_cluster2['month']
    else:
        ts_cluster2= df_cluster2['Factors']
        ts_cluster2.index=df_cluster2['month']

    dftest_cluster2 = adfuller(ts_cluster2, autolag='AIC')
    dftest_cluster2 # to check the result
    if dftest_cluster2[0]<dftest_cluster2[4]['1%']:
        flagger_c2=0 #stationary series
    else:
        flagger_c2=1 #non-stationary series


#####Cluster 3#####
if df_cluster3.empty==False:
    if len(df_cluster3.columns)==3:
        ts_cluster3= df_cluster3['Chats']
        ts_cluster3.index=df_cluster3['month']
    else:
        ts_cluster3= df_cluster3['Factors']
        ts_cluster3.index=df_cluster3['month']

    dftest_cluster3 = adfuller(ts_cluster3, autolag='AIC')
    dftest_cluster3 # to check the result
    if dftest_cluster3[0]<dftest_cluster3[4]['1%']:
        flagger_c3=0 #stationary series
    else:
        flagger_c3=1 #non-stationary series


#####Cluster 4#####
if df_cluster4.empty==False:
    if len(df_cluster4.columns)==3:
        ts_cluster4= df_cluster4['Chats']
        ts_cluster4.index=df_cluster4['month']
    else:
        ts_cluster4= df_cluster4['Factors']
        ts_cluster4.index=df_cluster4['month']

    dftest_cluster4 = adfuller(ts_cluster4, autolag='AIC')
    dftest_cluster4 # to check the result
    if dftest_cluster4[0]<dftest_cluster4[4]['1%']:
        flagger_c4=0 #stationary series
    else:
        flagger_c4=1 #non-stationary series

if df_cluster5.empty==False:
    if len(df_cluster5.columns)==3:
        ts_cluster5= df_cluster5['Chats']
        ts_cluster5.index=df_cluster5['month']
    else:
        ts_cluster5= df_cluster5['Factors']
        ts_cluster5.index=df_cluster5['month']

    dftest_cluster5 = adfuller(ts_cluster5, autolag='AIC')
    dftest_cluster5 # to check the result
    if dftest_cluster5[0]<dftest_cluster5[4]['1%']:
        flagger_c5=0 #stationary series
    else:
        flagger_c5=1 #non-stationary series


if df_cluster2.empty==False:
    df_cluster_comp['Cluster']=['ts_cluster2','ts_cluster3','ts_cluster4','ts_cluster5']
    df_cluster_comp['DF']=[dftest_cluster2[0],dftest_cluster3[0],dftest_cluster4[0],dftest_cluster5[0]]
else:
    if df_cluster3.empty==False:
        df_cluster_comp['Cluster']=['ts_cluster3','ts_cluster4','ts_cluster5']
        df_cluster_comp['DF']=[dftest_cluster3[0],dftest_cluster4[0],dftest_cluster5[0]]
    else:
        if df_cluster4.empty==False:
            df_cluster_comp['Cluster']=['ts_cluster4','ts_cluster5']
            df_cluster_comp['DF']=[dftest_cluster4[0],dftest_cluster5[0]]
        else:
            df_cluster_comp['Cluster']=['ts_cluster5']
            df_cluster_comp['DF']=[dftest_cluster5[0]]
        


##df with cluster having minimum adf_test result
df_cluster_min= df_cluster_comp.loc[df_cluster_comp['DF']==np.min(df_cluster_comp.DF)]


##Defining dataframe to store Cluster informations
df_Clusters=pd.DataFrame(columns=['Cluster','Date Frame','DF Test 1% Threshold','DF Test Result'])

if df_cluster2.empty==False:
    df_Clusters['Cluster']=['Cluster2','Cluster3','Cluster4','Cluster5']
    df_Clusters['Date Frame']=['Jan-2015 to Dec-2017','Oct-2015 to Dec-2017','Jan-2016 to Dec-2017','Jan-2017 to Dec-2017']
    df_Clusters['DF Test 1% Threshold']=[dftest_cluster2[4]['1%'],dftest_cluster3[4]['1%'],dftest_cluster4[4]['1%'],dftest_cluster5[4]['1%']]
    df_Clusters['DF Test Result']=[dftest_cluster2[0],dftest_cluster3[0],dftest_cluster4[0],dftest_cluster5[0]]
else:
    if df_cluster3.empty==False:
        df_Clusters['Cluster']=['Cluster3','Cluster4','Cluster5']
        df_Clusters['Date Frame']=['Oct-2015 to Dec-2017','Jan-2016 to Dec-2017','Jan-2017 to Dec-2017']
        df_Clusters['DF Test 1% Threshold']=[dftest_cluster3[4]['1%'],dftest_cluster4[4]['1%'],dftest_cluster5[4]['1%']]
        df_Clusters['DF Test Result']=[dftest_cluster3[0],dftest_cluster4[0],dftest_cluster5[0]]
    else:
        if df_cluster4.empty==False:
            df_Clusters['Cluster']=['Cluster4','Cluster5']
            df_Clusters['Date Frame']=['Jan-2016 to Dec-2017','Jan-2017 to Dec-2017']
            df_Clusters['DF Test 1% Threshold']=[dftest_cluster4[4]['1%'],dftest_cluster5[4]['1%']]
            df_Clusters['DF Test Result']=[dftest_cluster4[0],dftest_cluster5[0]]
        else:
            df_Clusters['Cluster']=['Cluster5']
            df_Clusters['Date Frame']=['Jan-2017 to Dec-2017']
            df_Clusters['DF Test 1% Threshold']=[dftest_cluster5[4]['1%']]
            df_Clusters['DF Test Result']=[dftest_cluster5[0]]

##Defining Timeseries basis flag and ADF Test result
if flag==0:
    try:
        flagger_c3
    except:
        timeseries=ts_cluster4
        timeseries.index=ts_cluster4.index
    else:
        if flagger_c3==0:
            timeseries= ts_cluster3
            timeseries.index=ts_cluster3.index
        else:
            if flagger_c4==0:
                timeseries=ts_cluster4
                timeseries.index=ts_cluster4.index
            else:
                if flagger_c5==0:
                    timeseries=ts_cluster4
                    timeseries.index=ts_cluster4.index
                else:
                    try:
                        flagger_c2
                    except:
                        timeseries=ts_cluster4
                        timeseries.index=ts_cluster4.index
                    else:
                        if flagger_c2==0:
                            timeseries=ts_cluster2
                            timeseries.index=ts_cluster2.index
                        else:
                            if df_cluster_min.iloc[0,0]=='ts_cluster3':
                                timeseries= ts_cluster3
                                timeseries.index=ts_cluster3.index
                            else:
                                if df_cluster_min.iloc[0,0]=='ts_cluster4':
                                    timeseries=ts_cluster4
                                    timeseries.index=ts_cluster4.index
                                else:
                                    timeseries=ts_cluster5
                                    timeseries.index=ts_cluster5.index
else:
    print(df_Clusters)
    resp_cluster=str(raw_input('Select the cluster: (2 for Cluster2, 3 for Cluster3..): '))
    resp_model=str(raw_input('Please select the type of modelling- 1 for default, 2 for enforced and 3 for logarithmic modification : '))

    if resp_cluster=='2':
            timeseries=ts_cluster2
            timeseries.index=ts_cluster2.index
            cluster= 'Cluster 2'
    else:
        if resp_cluster=='3':
                timeseries= ts_cluster3
                timeseries.index=ts_cluster3.index
                cluster='Cluster 3'
        else:
            if resp_cluster=='4':
                    timeseries=ts_cluster4
                    timeseries.index=ts_cluster4.index
                    cluster='Cluster 4'
            else:
                timeseries=ts_cluster5
                timeseries.index=ts_cluster5.index
                cluster='Cluster 5'


if flag==0:
    for m in range(len(df_clients)):
        if df_clients['ClientID'].iloc[m]==ClientID:
            if df_clients['flag_ast'].iloc[m]=="1":
                flagger=1
                warnings.filterwarnings("ignore")
                best_orders_c1=mod_eval_c1(timeseries,ar)
                model_final=SARIMAX(timeseries,order=best_orders_c1[0],seasonal_order=best_orders_c1[1],trend=best_orders_c1[2])
                model_fit=model_final.fit(disp=0)
                if 'Factors' in timeseries.name:
                    Forecast= model_fit.predict(start=len(timeseries), end=len(timeseries)+8)
                    Forecast= Forecast*ppt_count_2018[0]
                    print(Forecast)
                    forecast_ci=model_fit.get_prediction(start=len(timeseries), end=len(timeseries)+8)
                    ci = forecast_ci.conf_int()
                    ci['lower Factors']= ci['lower Factors']*ppt_count_2018[0]
                    ci['upper Factors']= ci['upper Factors']*ppt_count_2018[0]
                else:
                    Forecast= model_fit.predict(start=len(timeseries), end=len(timeseries)+8)
                    print(Forecast)
                    forecast_ci=model_fit.get_prediction(start=len(timeseries), end=len(timeseries)+8)
                    ci = forecast_ci.conf_int()
                    ci['lower Factors']= ci['lower Chats']
                    ci['upper Factors']= ci['upper Chats']
            else:
                if df_clients['flag_ast'].iloc[m]=="2":
                    flagger=2
                    warnings.filterwarnings("ignore")
                    best_orders_c2=mod_eval_c2(timeseries,ar)
                    model_final=SARIMAX(timeseries,order=best_orders_c2[0],seasonal_order=best_orders_c2[1],trend=best_orders_c2[2],enforce_stationarity=False,enforce_invertibility=False)
                    model_fit=model_final.fit(disp=0)
                    if 'Factors' in timeseries.name:
                        Forecast= model_fit.predict(start=len(timeseries), end=len(timeseries)+8)
                        Forecast= Forecast*ppt_count_2018[0]
                        print(Forecast)
                        forecast_ci=model_fit.get_prediction(start=len(timeseries), end=len(timeseries)+8)
                        ci = forecast_ci.conf_int()
                        ci['lower Factors']= ci['lower Factors']*ppt_count_2018[0]
                        ci['upper Factors']= ci['upper Factors']*ppt_count_2018[0]
                    else:
                        Forecast= model_fit.predict(start=len(timeseries), end=len(timeseries)+8)
                        print(Forecast)
                        forecast_ci=model_fit.get_prediction(start=len(timeseries), end=len(timeseries)+8)
                        ci = forecast_ci.conf_int()
                        ci['lower Factors']= ci['lower Chats']
                        ci['upper Factors']= ci['upper Chats']
                else:
                    flagger=3
                    timeseries1= np.log(timeseries)
                    timeseries1.index=timeseries.index
                    warnings.filterwarnings("ignore")
                    best_orders_c3=mod_eval_c2(timeseries1,ar)
                    model_final=SARIMAX(timeseries1,order=best_orders_c3[0],seasonal_order=best_orders_c3[1],trend=best_orders_c3[2],enforce_stationarity=False,enforce_invertibility=False)
                    model_fit=model_final.fit(disp=0)
                    if 'Factors' in timeseries1.name:
                        Forecast1= model_fit.predict(start=len(timeseries1), end=len(timeseries1)+8)
                        Forecast= np.exp(Forecast1)
                        Forecast= Forecast*ppt_count_2018[0]
                        print(Forecast)
                        forecast_ci=model_fit.get_prediction(start=len(timeseries), end=len(timeseries)+8)
                        ci = forecast_ci.conf_int()
                        ci['lower Factors']= np.exp(ci['lower Factors'])
                        ci['upper Factors']= np.exp(ci['upper Factors'])
                        ci['lower Factors']= ci['lower Factors']*ppt_count_2018[0]
                        ci['upper Factors']= ci['upper Factors']*ppt_count_2018[0]
                    else:
                        Forecast1= model_fit.predict(start=len(timeseries1), end=len(timeseries1)+8)
                        Forecast= np.exp(Forecast1)
                        print(Forecast)
                        forecast_ci=model_fit.get_prediction(start=len(timeseries), end=len(timeseries)+8)
                        ci = forecast_ci.conf_int()
                        ci['lower Factors']= ci['lower Chats']
                        ci['upper Factors']= ci['upper Chats']
                        ci['lower Factors']= np.exp(ci['lower Chats'])
                        ci['upper Factors']= np.exp(ci['upper Chats'])
else:
    if resp_model=="1":
        flagger=1
        warnings.filterwarnings("ignore")
        best_orders_c1=mod_eval_c1(timeseries,ar)
        model_final=SARIMAX(timeseries,order=best_orders_c1[0],seasonal_order=best_orders_c1[1],trend=best_orders_c1[2])
        model_fit=model_final.fit(disp=0)
        if 'Factors' in timeseries.name:
            Forecast= model_fit.predict(start=len(timeseries), end=len(timeseries)+8)
            Forecast= Forecast*ppt_count_2018[0]
            print(Forecast)
            forecast_ci=model_fit.get_prediction(start=len(timeseries), end=len(timeseries)+8)
            ci = forecast_ci.conf_int()
            ci['lower Factors']= ci['lower Factors']*ppt_count_2018[0]
            ci['upper Factors']= ci['upper Factors']*ppt_count_2018[0]
        else:
            Forecast= model_fit.predict(start=len(timeseries), end=len(timeseries)+8)
            print(Forecast)
            forecast_ci=model_fit.get_prediction(start=len(timeseries), end=len(timeseries)+8)
            ci = forecast_ci.conf_int()
            ci['lower Factors']= ci['lower Chats']
            ci['upper Factors']= ci['upper Chats']
    else:
        if resp_model=="2":
            flagger=2
            warnings.filterwarnings("ignore")
            best_orders_c2=mod_eval_c2(timeseries,ar)
            model_final=SARIMAX(timeseries,order=best_orders_c2[0],seasonal_order=best_orders_c2[1],trend=best_orders_c2[2],enforce_stationarity=False,enforce_invertibility=False)
            model_fit=model_final.fit(disp=0)
            if 'Factors' in timeseries.name:
                Forecast= model_fit.predict(start=len(timeseries), end=len(timeseries)+8)
                Forecast= Forecast*ppt_count_2018[0]
                print(Forecast)
                forecast_ci=model_fit.get_prediction(start=len(timeseries), end=len(timeseries)+8)
                ci = forecast_ci.conf_int()
                ci['lower Factors']= ci['lower Factors']*ppt_count_2018[0]
                ci['upper Factors']= ci['upper Factors']*ppt_count_2018[0]
            else:
                Forecast= model_fit.predict(start=len(timeseries), end=len(timeseries)+8)
                print(Forecast)
                forecast_ci=model_fit.get_prediction(start=len(timeseries), end=len(timeseries)+8)
                ci = forecast_ci.conf_int()
                ci['lower Factors']= ci['lower Chats']
                ci['upper Factors']= ci['upper Chats']
        else:
            flagger=3
            timeseries1= np.log(timeseries)
            timeseries1.index=timeseries.index
            warnings.filterwarnings("ignore")
            best_orders_c3=mod_eval_c2(timeseries1,ar)
            model_final=SARIMAX(timeseries1,order=best_orders_c3[0],seasonal_order=best_orders_c3[1],trend=best_orders_c3[2],enforce_stationarity=False,enforce_invertibility=False)
            model_fit=model_final.fit(disp=0)
            if 'Factors' in timeseries1.name:
                Forecast1= model_fit.predict(start=len(timeseries1), end=len(timeseries1)+8)
                Forecast= np.exp(Forecast1)
                Forecast= Forecast*ppt_count_2018[0]
                print(Forecast)
                forecast_ci=model_fit.get_prediction(start=len(timeseries), end=len(timeseries)+8)
                ci = forecast_ci.conf_int()
                ci['lower Factors']= np.exp(ci['lower Factors'])
                ci['upper Factors']= np.exp(ci['upper Factors'])
                ci['lower Factors']= ci['lower Factors']*ppt_count_2018[0]
                ci['upper Factors']= ci['upper Factors']*ppt_count_2018[0]
            else:
                Forecast1= model_fit.predict(start=len(timeseries1), end=len(timeseries1)+8)
                Forecast= np.exp(Forecast1)
                print(Forecast)
                forecast_ci=model_fit.get_prediction(start=len(timeseries), end=len(timeseries)+8)
                ci = forecast_ci.conf_int()
                ci['lower Factors']= ci['lower Chats']
                ci['upper Factors']= ci['upper Chats']
                ci['lower Factors']= np.exp(ci['lower Chats'])
                ci['upper Factors']= np.exp(ci['upper Chats'])


##Importing Forecasted Data
querystring_data= ("select * from [CSA Webchat Forecast Dup] where ClientID= '"+ClientID+"'")

df_data = pd.read_sql(querystring_data, pyodbc.connect('Trusted_Connection=yes', 
                     driver = '{SQL Server}',
                     server = 'GGNWIDDB2871,13163', 
                     database = 'Analytics_DB'))


##Defining dataframe to store actual and forecasted data
df_compare=pd.DataFrame(columns=['Date','Actual','Previous Forecast','New Forecast'])
df_compare['Date']=Forecast.index
s=datetime(df_compare['Date'].loc[0].year,df_compare['Date'].loc[0].month,1)
e=datetime(df_compare['Date'].loc[len(df_compare['Date'])-1].year,df_compare['Date'].loc[len(df_compare['Date'])-1].month,1)

qs=("select * from [actual_forecasted_webchat_revised] where [date] between '" +datetime.strftime(s,"%Y-%m-%d")+ "' and '" +datetime.strftime(e,"%Y-%m-%d")+ "' and ClientID='" +ClientID+"'")
df_comp = pd.read_sql(qs, pyodbc.connect('Trusted_Connection=yes', 
                     driver = '{SQL Server}',
                     server = 'GGNWIDDB2871,13163', 
                     database = 'Analytics_DB'))

if len(df_comp['Date'])>len(df_compare):
    for j in range(len(df_comp['Date'])):
        df_comp['Date'].iloc[j]= dateparse1(df_comp['Date'].iloc[j])
    
    for i in range(len(df_compare)):
        if df_compare['Date'].iloc[i] == df_comp['Date'].iloc[i]:
            df_compare['Actual'].iloc[i]= df_comp['Actual Chats'].iloc[i]
        for k in range(len(df_comp)):
            if df_compare['Date'].iloc[i] == df_comp['Date'].iloc[k]:
                df_compare['Previous Forecast'].iloc[i]=df_comp['Forecasted Chats'].iloc[k]
            if df_compare['Date'].iloc[i]== Forecast.index[i]:
                df_compare['New Forecast'].iloc[i]= int(Forecast[i])

    print(df_compare)
else:
    for j in range(len(df_comp['Date'])):
        df_comp['Date'].iloc[j]= dateparse1(df_comp['Date'].iloc[j])
    
    for i in range(len(df_compare)):
        for k in range(len(df_comp['Date'])):
            if df_compare['Date'].iloc[i] == df_comp['Date'].iloc[k]:
                df_compare['Actual'].iloc[i]= df_comp['Actual Chats'].iloc[k]
                df_compare['Previous Forecast'].iloc[i]=df_comp['Forecasted Chats'].iloc[k]
        if df_compare['Date'].iloc[i]== Forecast.index[i]:
            df_compare['New Forecast'].iloc[i]= int(Forecast[i])

    print(df_compare)

#Uploading data on SQL DB basis user input
Response= str(raw_input('Do you want to upload the forecasted chats in database? Yes/No: '))
               
if Response=='Yes':
    conn=pyodbc.connect('Trusted_Connection=yes',
                 driver='{SQL Server}',
                 server='GGNWIDDB2871,13163',
                 database='Analytics_DB')
    crsr=conn.cursor()

    if'Factors'  in timeseries.name:
        if df_data.empty==False:
            for i in range(len(Forecast)):
                qrystring= "update [dbo].[CSA Webchat Forecast Dup] Set Lower_Forecasted_Chats='"+str(ci['lower Factors'][i])+"', [Forecasted Chats]='"+str(Forecast[i])+ "',Upper_Forecasted_Chats='"+str(ci['upper Factors'][i])+ "'  where [ClientID]='"+str(ClientID)+"' and Date='"+datetime.strftime(Forecast.index[i],"%Y-%m-%d")+"'"
                crsr.execute(qrystring)
                conn.commit()
        else:
            for i in range(len(Forecast)):
                qrystring=("insert into [dbo].[CSA Webchat Forecast Dup] (ClientID,ClientName,[Date],Lower_Forecasted_Chats,[Forecasted Chats],Upper_Forecasted_Chats) values('"+str(ClientID)+"','"+str(Name[0])+"','"+datetime.strftime(Forecast.index[i],"%Y-%m-%d")+"','"+str(int(round(ci['lower Factors'][i])))+"','"+str(int(round(Forecast[i])))+ "','"+str(int(round(ci['upper Factors'][i])))+ "')")
                crsr.execute(qrystring)
                conn.commit()
    else:
        if df_data.empty==False:
            for i in range(len(Forecast)):
                qrystring= "update [dbo].[CSA Webchat Forecast Dup] Set Lower_Forecasted_Chats='"+str(ci['lower Factors'][i])+"', [Forecasted Chats]='"+str(Forecast[i])+ "',Upper_Forecasted_Chats='"+str(ci['upper Factors'][i])+ "'  where [ClientID]='"+str(ClientID)+"' and Date='"+datetime.strftime(Forecast.index[i],"%Y-%m-%d")+"'"
                crsr.execute(qrystring)
                conn.commit()
        else:
            for i in range(len(Forecast)):
                qrystring=("insert into [dbo].[CSA Webchat Forecast Dup] (ClientID,ClientName,[Date],Lower_Forecasted_Chats,[Forecasted Chats],Upper_Forecasted_Chats) values('"+str(ClientID)+"','"+str(Name[0])+"','"+datetime.strftime(Forecast.index[i],"%Y-%m-%d")+"','"+str(int(round(ci['lower Factors'][i])))+"','"+str(int(round(Forecast[i])))+ "','"+str(int(round(ci['upper Factors'][i])))+ "')")
                crsr.execute(qrystring)
                conn.commit()
     
    print('Forecast Data has been uploaded successfully!')
         
    if flag==1: 
        resp_flag= str(raw_input('Do you want to store the configuration as permanent in DB ? Yes/No : '))
        if resp_flag=='Yes':
            qrystring= "update [dbo].[Webchat_Flag_Data] Set flag_chat='"+resp_model+"', Comments='"+cluster+"' where ClientID='"+ClientID+"'"
            crsr.execute(qrystring)
            conn.commit()
            print('Configuration has been saved successfully!')  

