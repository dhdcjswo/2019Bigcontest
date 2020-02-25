
# coding: utf-8

# * 4개의 모델을 사용해서 test1_predict.csv, test2_predict.csv를 만드는 파이썬 코드임
# * 인풋 csv 파일
#     * ../preprocess/test1_preprocess.csv
#     * ../preprocess/test2_preprocess.csv
# * 4개의 모델명
#     * 이탈분류 : ../model/churn_yn_model.sav
#     * 이탈시기 : ../model/st_reg_model.sav
#     * 과금 분류 : ../model/spend_yn_model.sav
#     * 과금액 : ../model/as_reg_model.sav
# * 사용할 threshold
#     * 아직 미정
# * 아웃풋 csv 파일
#     * ./test1_predict.csv
#     * ./test2_predict.csv
#     
#     
#     
#     

# # 세팅

# In[1]:


import glob
import os

import pandas as pd
import numpy as np

from itertools import chain
from datetime import timedelta, datetime
import copy

import seaborn as sns

import matplotlib
import matplotlib.pyplot as plt
import matplotlib.font_manager as fm
plt.rcParams["figure.figsize"] = (10,5)

pd.set_option('display.max_columns', None)
pd.set_option('display.max_rows', 70)

import joblib
import vecstack
import xgboost as xgb
import lightgbm as lgb
import keras.backend as K


# # Model1 test1_preprocess

# In[2]:


test1_preprocess = pd.read_csv('../preprocess/test1_data.csv')


# ## <font color = 'red'>test1_preprocess 더미변수</font>

# In[3]:


def oneHotEncode(df,colNames):
    for col in colNames:
        if( df[col].dtype == np.dtype('object')):
            dummies = pd.get_dummies(df[col],prefix=col)
            df = pd.concat([df,dummies],axis=1)

            #drop the encoded column
            df.drop([col],axis = 1 , inplace=True)
    return df

def encoding_cate(x):

    x['pay_yn'] = x['pay_yn'].astype(object)
    x['most_trade_item'] = x['most_trade_item'].astype(object)
    x['most_trade_time'] = x['most_trade_time'].astype(object)

    print('There were {} columns before encoding categorical features'.format(x.shape[1]))
    combined = oneHotEncode(x, ['pay_yn','most_trade_time','most_trade_item'])
    print('There are {} columns after encoding categorical features'.format(combined.shape[1]))
    return combined

## 범주형 함수 encoding 적용
test1_preprocess = encoding_cate(test1_preprocess)


# In[4]:


test1_preprocess.shape


# In[5]:


test1_preprocess.head()


# In[6]:


test1_X = test1_preprocess.iloc[:,1:] # acc_id 등 불필요한 것 걸러내기


# In[7]:


test1_X.head()


# ## model load

# In[8]:


import joblib
import vecstack
import xgboost as xgb
import lightgbm as lgb
import keras.backend as K


# In[9]:


def custom_loss(y_true,y_pred):
    d = y_true-y_pred
    score_max = 30
    score = 30*K.exp(-(K.square(d)/450))
    loss = score_max - score

    return K.sum(loss)

import keras.losses
keras.losses.custom_loss = custom_loss


# In[10]:


glob.glob('../model/*')


# In[11]:


# 이탈 분류 모델
churn_yn_model = joblib.load('../model\\churn_yn_model.pkl')

# 이탈시기 예측 모델
churn_rg_model_1 = joblib.load('../model\\churn_rg_model_1.sav')
churn_rg_model_2 =  joblib.load('../model/churn_rg_model_2.sav')

# 과금 분류 모델
spend_yn_model = joblib.load('../model\\spend_yn_model.pkl')
                                
# 과금액 예측 모델은 나중에 로드함


# ### churn_yn

# In[12]:


#####
test1_churn_yn_pred = churn_yn_model.predict_proba(test1_X)
test1_churn_yn_pred = test1_churn_yn_pred[:,1]


# In[13]:


pd.DataFrame(test1_churn_yn_pred).head(5)


# ### churn_rg_model_1

# In[14]:


#####
test1_churn_rg_pred_1 = churn_rg_model_1.predict(test1_X)


# In[15]:


test1_churn_rg_pred_1


# ### <font color='red'>churn_rg_model_2</font>

# In[16]:


#####
test1_churn_rg_pred_2 = churn_rg_model_2.predict(test1_X)


# In[17]:


test1_churn_rg_pred_2 = test1_churn_rg_pred_2.reshape(-1)


# In[18]:


pd.Series(test1_churn_rg_pred_2).hist(bins=100)


# #### <font color = 'red'>survival_time_pred 생성</font>

# In[19]:


def get_test1_churn_rg_pred(pred_1, pred_2):
    pred_1_int = [7*(i-1)+1 for i in pred_1]
    
    pred = []
    
    for i,j in zip(pred_1_int, pred_2):
        if (i + j) < 0 :
            pred.append(1)
        elif (i + j)/2 > 63 :
            pred.append(63)
        else :
            pred.append((i+j)/2)
    
    return pred


# In[20]:


#####
test1_churn_rg_pred = get_test1_churn_rg_pred(test1_churn_rg_pred_1, test1_churn_rg_pred_2)


# In[21]:


test1_churn_rg_pred[:10]


# ### spend_yn_model

# #### <font color = 'red'>survival_time 예측값을 제일 마지막 column에 붙임</font>

# In[22]:


test1_X['survival_time'] = test1_churn_rg_pred


# In[23]:


test1_X.head()


# In[24]:


#####
test1_spend_yn_pred = spend_yn_model.predict_proba(test1_X)
test1_spend_yn_pred = test1_spend_yn_pred[:,1]


# In[25]:


test1_spend_yn_pred


# ### spend_rg_model

# In[26]:


def custom_loss_as(y_true,y_pred):
    d = y_true-y_pred
    bool_idx_1 = K.greater(d,0)
    score_max = (20*y_true) - 0.3*y_true
    score_under= (((10/9)*(y_pred/y_true)-1/9)*20*y_true) - 0.3*y_pred
    score_over = (20*y_true) - 0.3*y_pred
    loss1 = score_max - score_under
    loss2 = score_max - score_over
    loss = K.switch(bool_idx_1,loss1,loss2)
    return K.sum(loss)

import keras.losses
keras.losses.custom_loss = custom_loss_as


# In[27]:


spend_rg_model =  joblib.load('../model/spend_rg_model.sav')


# In[28]:


test1_spend_rg_pred = spend_rg_model.predict(test1_X)


# In[29]:


test1_spend_rg_pred = test1_spend_rg_pred.reshape(-1)


# In[30]:


test1_spend_rg_pred[:5]


# ##### <font color='red'>음의 값이 나온 경우 0 으로 대체</font>

# In[31]:


#####
test1_spend_rg_pred[test1_spend_rg_pred<0] = 0


# ## test1 예측하기

# In[32]:


test1_df = pd.DataFrame({'acc_id' : test1_preprocess['acc_id'],
                        'survival_time_pred' : test1_churn_rg_pred,
                        'amount_spent_pred' : test1_spend_rg_pred,
                        'churn_proba' : test1_churn_yn_pred,
                        'spent_proba' : test1_spend_yn_pred
   })



def set_threshold(df, spent_threshold, churn_threshold):
    as_st = [[k, l] if ((i>=spent_threshold) & (j>=churn_threshold)) else (0,64) for i,j,k,l in zip(df['spent_proba'], df['churn_proba'], df['amount_spent_pred'],df['survival_time_pred'])]
    df['amount_spent'] = [i[0] for i in as_st]
    df['survival_time'] = [i[1] for i in as_st]
    
    submission = pd.DataFrame({
        'acc_id': df['acc_id'],
        'survival_time': df['survival_time'],
        'amount_spent': df['amount_spent']
    })
    return submission

test1_predict = set_threshold(test1_df, 0.05, 0.05)
#test1_predict.to_csv('./test1_predict.csv')


# In[33]:


test1_predict


# # Model1 test2_preprocess

# In[34]:


#test2_X = test2_preprocess.iloc[:,:] # acc_id 등 불필요한 것 걸러내기


# In[35]:


#test1_preprocess = pd.read_csv('../preprocess/test1_preprocess.csv')
#test2_preprocess = pd.read_csv('../preprocess/test2_preprocess.csv')


# In[36]:


test2_preprocess = pd.read_csv('../preprocess/test2_data.csv')


# In[37]:


test2_preprocess.shape


# In[38]:


test2_preprocess.head()


# In[39]:


test2_preprocess.most_trade_item.unique()


# In[40]:


test2_preprocess.most_trade_time.unique()


# ## <font color = 'red'>test2_preprocess 더미변수</font>

# In[41]:


def oneHotEncode(df,colNames):
    for col in colNames:
        if( df[col].dtype == np.dtype('object')):
            dummies = pd.get_dummies(df[col],prefix=col)
            df = pd.concat([df,dummies],axis=1)

            #drop the encoded column
            df.drop([col],axis = 1 , inplace=True)
    return df

def encoding_cate(x):

    x['pay_yn'] = x['pay_yn'].astype(object)
    x['most_trade_item'] = x['most_trade_item'].astype(object)
    x['most_trade_time'] = x['most_trade_time'].astype(object)

    print('There were {} columns before encoding categorical features'.format(x.shape[1]))
    combined = oneHotEncode(x, ['pay_yn','most_trade_time','most_trade_item'])
    print('There are {} columns after encoding categorical features'.format(combined.shape[1]))
    return combined

## 범주형 함수 encoding 적용
test2_preprocess = encoding_cate(test2_preprocess)


# In[42]:


test2_preprocess.shape


# In[43]:


test2_preprocess.head()


# In[44]:


test2_X = test2_preprocess.iloc[:,1:] # acc_id 등 불필요한 것 걸러내기


# In[45]:


test2_X.head()


# ## model load

# In[46]:


import joblib
import vecstack
import xgboost as xgb
import lightgbm as lgb
import keras.backend as K


# In[47]:


def custom_loss(y_true,y_pred):
    d = y_true-y_pred
    score_max = 30
    score = 30*K.exp(-(K.square(d)/450))
    loss = score_max - score

    return K.sum(loss)

import keras.losses
keras.losses.custom_loss = custom_loss


# In[48]:


glob.glob('../model/*')


# In[49]:


# 이탈 분류 모델
churn_yn_model = joblib.load('../model\\churn_yn_model.pkl')

# 이탈시기 예측 모델
churn_rg_model_1 = joblib.load('../model\\churn_rg_model_1.sav')
churn_rg_model_2 =  joblib.load('../model/churn_rg_model_2.sav')

# 과금 분류 모델
spend_yn_model = joblib.load('../model\\spend_yn_model.pkl')
                                
# 과금액 예측 모델은 나중에 로드함


# ### churn_yn

# In[50]:


#####
test2_churn_yn_pred = churn_yn_model.predict_proba(test2_X)
test2_churn_yn_pred = test2_churn_yn_pred[:,1]


# In[51]:


pd.DataFrame(test2_churn_yn_pred).head(5)


# ### churn_rg_model_1

# In[52]:


#####
test2_churn_rg_pred_1 = churn_rg_model_1.predict(test2_X)


# In[53]:


test2_churn_rg_pred_1


# ### <font color='red'>churn_rg_model_2</font>

# In[54]:


#####
test2_churn_rg_pred_2 = churn_rg_model_2.predict(test2_X)


# In[55]:


test2_churn_rg_pred_2 = test2_churn_rg_pred_2.reshape(-1)


# In[56]:


test2_churn_rg_pred_2


# #### <font color = 'red'>survival_time_pred 생성</font>

# In[57]:


def get_test2_churn_rg_pred(pred_1, pred_2):
    pred_1_int = [7*(i-1)+1 for i in pred_1]
    
    pred = []
    
    for i,j in zip(pred_1_int, pred_2):
        if (i + j) < 0 :
            pred.append(1)
        elif (i + j)/2 > 63 :
            pred.append(63)
        else :
            pred.append((i+j)/2)
    
    return pred


# In[58]:


#####
test2_churn_rg_pred = get_test2_churn_rg_pred(test2_churn_rg_pred_1, test2_churn_rg_pred_2)


# In[59]:


test2_churn_rg_pred[:10]


# ### spend_yn_model

# #### <font color = 'red'>survival_time 예측값을 제일 마지막 column에 붙임</font>

# In[60]:


test2_X['survival_time'] = test2_churn_rg_pred


# In[61]:


test2_X.head()


# In[62]:


#####
test2_spend_yn_pred = spend_yn_model.predict_proba(test2_X)
test2_spend_yn_pred = test2_spend_yn_pred[:,1]


# In[63]:


test2_spend_yn_pred


# ### spend_rg_model

# In[64]:


def custom_loss_as(y_true,y_pred):
    d = y_true-y_pred
    bool_idx_1 = K.greater(d,0)
    score_max = (20*y_true) - 0.3*y_true
    score_under= (((10/9)*(y_pred/y_true)-1/9)*20*y_true) - 0.3*y_pred
    score_over = (20*y_true) - 0.3*y_pred
    loss1 = score_max - score_under
    loss2 = score_max - score_over
    loss = K.switch(bool_idx_1,loss1,loss2)
    return K.sum(loss)

import keras.losses
keras.losses.custom_loss = custom_loss_as


# In[65]:


spend_rg_model =  joblib.load('../model/spend_rg_model.sav')


# In[66]:


test2_spend_rg_pred = spend_rg_model.predict(test2_X)


# In[67]:


test2_spend_rg_pred = test2_spend_rg_pred.reshape(-1)


# In[68]:


test2_spend_rg_pred[:5]


# ##### <font color='red'>음의 값이 나온 경우 0 으로 대체</font>

# In[69]:


#####
test2_spend_rg_pred[test2_spend_rg_pred<0] = 0


# ## test2 예측하기

# In[70]:


test2_df = pd.DataFrame({'acc_id' : test2_preprocess['acc_id'],
                        'survival_time_pred' : test2_churn_rg_pred,
                        'amount_spent_pred' : test2_spend_rg_pred,
                        'churn_proba' : test2_churn_yn_pred,
                        'spent_proba' : test2_spend_yn_pred
   })



def set_threshold(df, spent_threshold, churn_threshold):
    as_st = [[k, l] if ((i>=spent_threshold) & (j>=churn_threshold)) else (0,64) for i,j,k,l in zip(df['spent_proba'], df['churn_proba'], df['amount_spent_pred'],df['survival_time_pred'])]
    df['amount_spent'] = [i[0] for i in as_st]
    df['survival_time'] = [i[1] for i in as_st]
    
    submission = pd.DataFrame({
        'acc_id': df['acc_id'],
        'survival_time': df['survival_time'],
        'amount_spent': df['amount_spent']
    })
    return submission

test2_predict = set_threshold(test2_df, 0.05, 0.05)
#test2_predict.to_csv('./test2_predict.csv')


# In[71]:


test2_predict


# # Model2 test1_preprocess

# In[101]:


#test1_preprocess = pd.read_csv('../preprocess/test1_preprocess.csv')
#test2_preprocess = pd.read_csv('../preprocess/test2_preprocess.csv')


# In[72]:


test1_preprocess = pd.read_csv('../preprocess/test1_data.csv')


# In[73]:


test1_preprocess.shape


# In[74]:


test1_preprocess.head()


# In[75]:


model2_test1_preprocess = test1_preprocess.loc[test1_preprocess.first_app==1, :]


# In[76]:


model2_test1_preprocess.shape


# In[77]:


model2_test1_preprocess.most_trade_item.unique()


# In[78]:


model2_test1_preprocess.most_trade_time.unique()


# ## <font color = 'red'>model2_test1_preprocess 더미변수</font>

# In[79]:


def oneHotEncode(df,colNames):
    for col in colNames:
        if( df[col].dtype == np.dtype('object')):
            dummies = pd.get_dummies(df[col],prefix=col)
            df = pd.concat([df,dummies],axis=1)

            #drop the encoded column
            df.drop([col],axis = 1 , inplace=True)
    return df

def encoding_cate(x):

    x['pay_yn'] = x['pay_yn'].astype(object)
    x['most_trade_item'] = x['most_trade_item'].astype(object)
    x['most_trade_time'] = x['most_trade_time'].astype(object)

    print('There were {} columns before encoding categorical features'.format(x.shape[1]))
    combined = oneHotEncode(x, ['pay_yn','most_trade_time','most_trade_item'])
    print('There are {} columns after encoding categorical features'.format(combined.shape[1]))
    return combined

## 범주형 함수 encoding 적용
model2_test1_preprocess = encoding_cate(model2_test1_preprocess)


# In[80]:


model2_test1_preprocess.shape


# In[81]:


model2_test1_preprocess.head()


# In[82]:


model2_test1_X = model2_test1_preprocess.iloc[:,1:] # acc_id 등 불필요한 것 걸러내기


# In[83]:


model2_test1_X.head()


# ## model load

# In[84]:


import joblib
import vecstack
import xgboost as xgb
import lightgbm as lgb
import keras.backend as K


# In[85]:


def custom_loss(y_true,y_pred):
    d = y_true-y_pred
    score_max = 30
    score = 30*K.exp(-(K.square(d)/450))
    loss = score_max - score

    return K.sum(loss)

import keras.losses
keras.losses.custom_loss = custom_loss


# In[86]:


glob.glob('../model/*')


# In[87]:


# 이탈 분류 모델
churn_yn_model = joblib.load('../model\\first_churn_yn_model.pkl')

# 이탈시기 예측 모델
churn_rg_model_1 = joblib.load('../model\\first_churn_rg_model_1.sav')
churn_rg_model_2 =  joblib.load('../model/first_churn_rg_model_2.sav')

# 과금 분류 모델
spend_yn_model = joblib.load('../model\\first_spend_yn_model.pkl')
                                
# 과금액 예측 모델은 나중에 로드함


# ### churn_yn

# In[88]:


#####
model2_test1_churn_yn_pred = churn_yn_model.predict_proba(model2_test1_X)
model2_test1_churn_yn_pred = model2_test1_churn_yn_pred[:,1]


# In[89]:


pd.DataFrame(model2_test1_churn_yn_pred).head(5)


# ### churn_rg_model_1

# In[90]:


#####
model2_test1_churn_rg_pred_1 = churn_rg_model_1.predict(model2_test1_X)


# In[91]:


model2_test1_churn_rg_pred_1


# ### <font color='red'>churn_rg_model_2</font>

# In[92]:


#####
model2_test1_churn_rg_pred_2 = churn_rg_model_2.predict(model2_test1_X)


# In[93]:


model2_test1_churn_rg_pred_2 = model2_test1_churn_rg_pred_2.reshape(-1)


# In[94]:


pd.Series(model2_test1_churn_rg_pred_2).max()


# #### <font color = 'red'>survival_time_pred 생성</font>

# In[95]:


def get_model2_test1_churn_rg_pred(pred_1, pred_2):
    pred_1_int = [7*(i-1)+1 for i in pred_1]
    
    pred = []
    
    for i,j in zip(pred_1_int, pred_2):
        if (i + j) < 0 :
            pred.append(1)
        elif (i + j)/2 > 63 :
            pred.append(63)
        else :
            pred.append((i+j)/2)
    
    return pred


# In[96]:


#####
model2_test1_churn_rg_pred = get_model2_test1_churn_rg_pred(model2_test1_churn_rg_pred_1, model2_test1_churn_rg_pred_2)


# In[97]:


model2_test1_churn_rg_pred[:10]


# ### spend_yn_model

# #### <font color = 'red'>survival_time 예측값을 제일 마지막 column에 붙임</font>

# In[98]:


model2_test1_X['survival_time'] = model2_test1_churn_rg_pred


# In[99]:


model2_test1_X.head()


# In[100]:


#####
model2_test1_spend_yn_pred = spend_yn_model.predict_proba(model2_test1_X)
model2_test1_spend_yn_pred = model2_test1_spend_yn_pred[:,1]


# In[101]:


model2_test1_spend_yn_pred


# ### spend_rg_model

# In[102]:


def custom_loss_as(y_true,y_pred):
    d = y_true-y_pred
    bool_idx_1 = K.greater(d,0)
    score_max = (20*y_true) - 0.3*y_true
    score_under= (((10/9)*(y_pred/y_true)-1/9)*20*y_true) - 0.3*y_pred
    score_over = (20*y_true) - 0.3*y_pred
    loss1 = score_max - score_under
    loss2 = score_max - score_over
    loss = K.switch(bool_idx_1,loss1,loss2)
    return K.sum(loss)

import keras.losses
keras.losses.custom_loss = custom_loss_as


# In[103]:


spend_rg_model =  joblib.load('../model/spend_rg_model.sav')


# In[104]:


model2_test1_spend_rg_pred = spend_rg_model.predict(model2_test1_X)


# In[105]:


model2_test1_spend_rg_pred = model2_test1_spend_rg_pred.reshape(-1)


# In[106]:


model2_test1_spend_rg_pred[:15]


# ##### <font color='red'>음의 값이 나온 경우 0 으로 대체</font>

# In[107]:


#####
model2_test1_spend_rg_pred[model2_test1_spend_rg_pred<0] = 0


# ## <font color='red'>model2_test1 예측하기</font>

# In[108]:


model2_test1_df = pd.DataFrame({'acc_id' : model2_test1_preprocess['acc_id'],
                        'survival_time_pred' : model2_test1_churn_rg_pred,
                        'amount_spent_pred' : model2_test1_spend_rg_pred,
                        'churn_proba' : model2_test1_churn_yn_pred,
                        'spent_proba' : model2_test1_spend_yn_pred
   })



def set_threshold(df, spent_threshold, churn_threshold):
    as_st = [[k, l] if ((i>=spent_threshold) & (j>=churn_threshold)) else (0,64) for i,j,k,l in zip(df['spent_proba'], df['churn_proba'], df['amount_spent_pred'],df['survival_time_pred'])]
    df['amount_spent'] = [i[0] for i in as_st]
    df['survival_time'] = [i[1] for i in as_st]
    
    submission = pd.DataFrame({
        'acc_id': df['acc_id'],
        'survival_time': df['survival_time'],
        'amount_spent': df['amount_spent']
    })
    return submission

model2_test1_predict = set_threshold(model2_test1_df, 0.05, 0.05)
#model2_test1_predict.to_csv('./model2_test1_predict.csv')


# In[109]:


model2_test1_predict


# # Model2 test1_preprocess

# In[110]:


#test1_preprocess = pd.read_csv('../preprocess/test1_preprocess.csv')
#test2_preprocess = pd.read_csv('../preprocess/test2_preprocess.csv')


# In[111]:


test2_preprocess = pd.read_csv('../preprocess/test2_data.csv')


# In[112]:


test2_preprocess.shape


# In[113]:


test2_preprocess.head()


# In[114]:


model2_test2_preprocess = test2_preprocess.loc[test2_preprocess.first_app==1, :]


# In[115]:


model2_test2_preprocess.shape


# In[116]:


model2_test2_preprocess.most_trade_item.unique()


# In[117]:


model2_test2_preprocess.most_trade_time.unique()


# ## <font color = 'red'>model2_test2_preprocess 컬럼 순서맞추기</font>

# ## <font color = 'red'>model2_test2_preprocess 더미변수</font>

# In[118]:


def oneHotEncode(df,colNames):
    for col in colNames:
        if( df[col].dtype == np.dtype('object')):
            dummies = pd.get_dummies(df[col],prefix=col)
            df = pd.concat([df,dummies],axis=1)

            #drop the encoded column
            df.drop([col],axis = 1 , inplace=True)
    return df

def encoding_cate(x):

    x['pay_yn'] = x['pay_yn'].astype(object)
    x['most_trade_item'] = x['most_trade_item'].astype(object)
    x['most_trade_time'] = x['most_trade_time'].astype(object)

    print('There were {} columns before encoding categorical features'.format(x.shape[1]))
    combined = oneHotEncode(x, ['pay_yn','most_trade_time','most_trade_item'])
    print('There are {} columns after encoding categorical features'.format(combined.shape[1]))
    return combined

## 범주형 함수 encoding 적용
model2_test2_preprocess = encoding_cate(model2_test2_preprocess)


# In[119]:


model2_test2_preprocess.shape


# In[120]:


model2_test2_preprocess.head()


# In[121]:


model2_test2_X = model2_test2_preprocess.iloc[:,1:] # acc_id 등 불필요한 것 걸러내기


# In[122]:


model2_test2_X.head()


# ## model load

# In[123]:


import joblib
import vecstack
import xgboost as xgb
import lightgbm as lgb
import keras.backend as K


# In[124]:


def custom_loss(y_true,y_pred):
    d = y_true-y_pred
    score_max = 30
    score = 30*K.exp(-(K.square(d)/450))
    loss = score_max - score

    return K.sum(loss)

import keras.losses
keras.losses.custom_loss = custom_loss


# In[125]:


glob.glob('../model/*')


# In[126]:


# 이탈 분류 모델
churn_yn_model = joblib.load('../model\\first_churn_yn_model.pkl')

# 이탈시기 예측 모델
churn_rg_model_1 = joblib.load('../model\\first_churn_rg_model_1.sav')
churn_rg_model_2 =  joblib.load('../model/first_churn_rg_model_2.sav')

# 과금 분류 모델
spend_yn_model = joblib.load('../model\\first_spend_yn_model.pkl')
                                
# 과금액 예측 모델은 나중에 로드함


# ### churn_yn

# In[127]:


#####
model2_test2_churn_yn_pred = churn_yn_model.predict_proba(model2_test2_X)
model2_test2_churn_yn_pred = model2_test2_churn_yn_pred[:,1]


# In[128]:


pd.DataFrame(model2_test2_churn_yn_pred).head(5)


# ### churn_rg_model_1

# In[129]:


#####
model2_test2_churn_rg_pred_1 = churn_rg_model_1.predict(model2_test2_X)


# In[130]:


model2_test2_churn_rg_pred_1


# ### <font color='red'>churn_rg_model_2</font>

# In[131]:


#####
model2_test2_churn_rg_pred_2 = churn_rg_model_2.predict(model2_test2_X)


# In[132]:


model2_test2_churn_rg_pred_2 = model2_test2_churn_rg_pred_2.reshape(-1)


# In[133]:


pd.Series(model2_test2_churn_rg_pred_2).max()


# #### <font color = 'red'>survival_time_pred 생성</font>

# In[134]:


def get_model2_test2_churn_rg_pred(pred_1, pred_2):
    pred_1_int = [7*(i-1)+1 for i in pred_1]
    
    pred = []
    
    for i,j in zip(pred_1_int, pred_2):
        if (i + j) < 0 :
            pred.append(1)
        elif (i + j)/2 > 63 :
            pred.append(63)
        else :
            pred.append((i+j)/2)
    
    return pred


# In[135]:


#####
model2_test2_churn_rg_pred = get_model2_test2_churn_rg_pred(model2_test2_churn_rg_pred_1, model2_test2_churn_rg_pred_2)


# In[136]:


model2_test2_churn_rg_pred[:10]


# ### spend_yn_model

# #### <font color = 'red'>survival_time 예측값을 제일 마지막 column에 붙임</font>

# In[137]:


model2_test2_X['survival_time'] = model2_test2_churn_rg_pred


# In[138]:


model2_test2_X.head()


# In[139]:


#####
model2_test2_spend_yn_pred = spend_yn_model.predict_proba(model2_test2_X)
model2_test2_spend_yn_pred = model2_test2_spend_yn_pred[:,1]


# In[140]:


model2_test2_spend_yn_pred


# ### spend_rg_model

# In[141]:


def custom_loss_as(y_true,y_pred):
    d = y_true-y_pred
    bool_idx_1 = K.greater(d,0)
    score_max = (20*y_true) - 0.3*y_true
    score_under= (((10/9)*(y_pred/y_true)-1/9)*20*y_true) - 0.3*y_pred
    score_over = (20*y_true) - 0.3*y_pred
    loss1 = score_max - score_under
    loss2 = score_max - score_over
    loss = K.switch(bool_idx_1,loss1,loss2)
    return K.sum(loss)

import keras.losses
keras.losses.custom_loss = custom_loss_as


# In[142]:


spend_rg_model =  joblib.load('../model/spend_rg_model.sav')


# In[143]:


model2_test2_spend_rg_pred = spend_rg_model.predict(model2_test2_X)


# In[144]:


model2_test2_spend_rg_pred = model2_test2_spend_rg_pred.reshape(-1)


# In[145]:


model2_test2_spend_rg_pred[:15]


# ##### <font color='red'>음의 값이 나온 경우 0 으로 대체</font>

# In[146]:


#####
model2_test2_spend_rg_pred[model2_test2_spend_rg_pred<0] = 0


# ## <font color='red'>model2_test2 예측하기</font>

# In[147]:


model2_test2_df = pd.DataFrame({'acc_id' : model2_test2_preprocess['acc_id'],
                        'survival_time_pred' : model2_test2_churn_rg_pred,
                        'amount_spent_pred' : model2_test2_spend_rg_pred,
                        'churn_proba' : model2_test2_churn_yn_pred,
                        'spent_proba' : model2_test2_spend_yn_pred
   })



def set_threshold(df, spent_threshold, churn_threshold):
    as_st = [[k, l] if ((i>=spent_threshold) & (j>=churn_threshold)) else (0,64) for i,j,k,l in zip(df['spent_proba'], df['churn_proba'], df['amount_spent_pred'],df['survival_time_pred'])]
    df['amount_spent'] = [i[0] for i in as_st]
    df['survival_time'] = [i[1] for i in as_st]
    
    submission = pd.DataFrame({
        'acc_id': df['acc_id'],
        'survival_time': df['survival_time'],
        'amount_spent': df['amount_spent']
    })
    return submission

model2_test2_predict = set_threshold(model2_test2_df, 0.05, 0.05)
#model2_test2_predict.to_csv('./model2_test2_predict.csv')


# In[148]:


model2_test2_predict


# # 최종 예측값 test1

# In[149]:


test1_predict.shape
print(model2_test1_predict.shape)


# In[150]:


temp = test1_predict.merge(model2_test1_predict, on='acc_id', how='left')


# In[151]:


temp.head()


# In[152]:


for i in range(len(test1_predict)):
    if np.isnan(temp.iloc[i,3]) :
        temp.iloc[i,3] = temp.iloc[i,1]
        temp.iloc[i,4] = temp.iloc[i,2]

temp.head()


# In[153]:


final_test1_predict = pd.DataFrame({
    'acc_id' : temp.acc_id,
    'survival_time' : temp.survival_time_y,
    'amount_spent' : temp.amount_spent_y
})


# In[154]:


final_test1_predict.to_csv('./test1_predict.csv', index=False)


# # 최종 예측값 test2

# In[155]:


test2_predict.shape
print(model2_test2_predict.shape)


# In[156]:


temp = test2_predict.merge(model2_test2_predict, on='acc_id', how='left')


# In[157]:


temp.head()


# In[158]:


for i in range(len(test2_predict)):
    if np.isnan(temp.iloc[i,3]) :
        temp.iloc[i,3] = temp.iloc[i,1]
        temp.iloc[i,4] = temp.iloc[i,2]

temp.head()


# In[159]:


final_test2_predict = pd.DataFrame({
    'acc_id' : temp.acc_id,
    'survival_time' : temp.survival_time_y,
    'amount_spent' : temp.amount_spent_y
})


# In[160]:


final_test2_predict.to_csv('./test2_predict.csv', index=False)

