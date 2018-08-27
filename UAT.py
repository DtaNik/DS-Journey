# -*- coding: utf-8 -*-
"""
Created on Thu Apr 26 17:31:49 2018

@author: AH0667765
"""

import pandas as pd
import numpy as np


Data1 = pd.read_csv("X:\\Team IEG GGN\Abhinav\\Re-Imagine Project\\UAT Testing\\Data from HUE\\person_hm_choice_enrolled.csv")
Data2 = pd.read_csv("X:\\Team IEG GGN\Abhinav\\Re-Imagine Project\\UAT Testing\\Data from HUE\\person_hm_choice_enrolled_ja_2016_to_2018_01_01.csv")


Data2['prsn_intn_id'].describe()
Data2.describe()
Data1['client_name'] = Data1['client_name'].astype('category')
Data1['client_name'].value_counts()


### Data 1 Analysis

Data1['enrolled_total_annual_hsa_contribution'].describe()

Data1['choice_has_hsa_option'].value_counts()
Data2['choice_has_hsa_option'].value_counts()

Data1['choice_no_coverage_price_max'].describe()
Data1['choice_no_coverage_price_max'].describe()
Data1['choice_no_coverage_net_price_min'].describe()
Data1['choice_no_coverage_net_price_25_percentile'].describe()
Data1['choice_no_coverage_net_price_median'].describe()
Data1['choice_no_coverage_net_price_avg'].describe()
Data1['choice_no_coverage_net_price_75_percentile'].describe()
Data1['choice_no_coverage_net_price_max'].describe()
Data1['choice_no_coverage_net_price_max'].describe()
Data1['choice_no_coverage_net_price_max'].value_counts()
Data1['choice_participant_max_price'].value_counts()

Data1['choice_participant_max_credit'].describe()
Data1['choice_participant_max_credit'].value_counts()

Data1['choice_participant_net_price_min'].describe()

Data1['choice_participant_net_price_25_percentile'].describe()
Data1['choice_participant_net_price_median'].describe()
Data1['choice_participant_net_price_avg'].describe()
Data1['choice_participant_net_price_75_percentile'].describe()
Data1['choice_participant_net_price_max'].describe()
Data1['choice_participant_sa_max_enrollment_period_at'].describe()
Data1['choice_spouse_max_price'].describe()

Data1['choice_spouse_max_credit'].describe()
Data1['choice_spouse_max_credit'].value_counts()

Data1['choice_spouse_net_price_min'].describe()

Data1['choice_spouse_net_price_25_percentile'].describe()
Data1['choice_spouse_net_price_50_median'].describe()

Data1['choice_spouse_net_price_avg'].describe()
Data1['choice_spouse_net_price_75_percentile'].describe()

Data1['choice_spouse_net_price_max'].describe()
Data1['choice_spouse_sa_max_enrollment_period_at'].describe()






### Data 2 Analysis

Data2['act_ldsc_tx'].value_counts()
Data2['plan_smry_brnd_cd'].value_counts()
Data2['plan_brnd_cd'].value_counts()
Data2['plan_ldsc_tx'].value_counts()
Data2['enrolled_opt_brnd_cd'].value_counts()
Data2['enrolled_cv_cat_brnd_cd'].value_counts()
Data2['enrolled_cv_cat_lbl_cd'].value_counts()
Data2['enrolled_cv_cat_ldsc_tx'].value_counts()


Num_list = [ 
            'plfm_prsn_intn_id', 
            'prsn_intn_id', 
            'act_ref_nmbr_id', 
            'plan_id', 
            'act_id', 
            'act_efdt', 
            'choice_has_hsa_option', 
            'choice_no_cov_max_price', 
            'choice_no_cov_max_credit', 
            'choice_no_cov_max_enrlprd_tot_max_at', 
            'choice_part_max_price', 
            'choice_part_max_credit', 
            'choice_part_max_enrlprd_tot_max_at', 
            'choice_sps_max_price', 
            'choice_sps_max_credit', 
            'choice_sps_max_enrlprd_tot_max_at', 
            'choice_chld_max_price', 
            'choice_chld_max_credit', 
            'choice_chld_max_enrlprd_tot_max_at', 
            'choice_part_n_1_max_price', 
            'choice_part_n_1_max_credit',
            'choice_part_n_1_max_enrlprd_tot_max_at', 
            'choice_chdr_max_price', 
            'choice_chdr_max_credit', 
            'choice_chdr_max_enrlprd_tot_max_at', 
            'choice_part_n_fmly_max_price', 
            'choice_part_n_fmly_max_credit', 
            'choice_part_n_fmly_max_enrlprd_tot_max_at', 
            'choice_sps_n_fmly_max_price', 
            'choice_sps_n_fmly_max_credit', 
            'choice_sps_n_fmly_max_enrlprd_tot_max_at',
            'choice_other_max_price', 
            'choice_other_max_credit', 
            'choice_other_max_enrlprd_tot_max_at', 
            'choice_max_price', 'choice_max_credit',
            'choice_count', 
            'choice_has_ee_option', 
            'choice_has_chld_option', 
            'choice_has_sps_option', 
            'choice_has_fmly_option', 
            'choice_has_dpnd_option', 
            'choice_has_rtee_option', 
            'has_person_choice_rec', 
            'has_person_enrolled_rec', 
            'enrolled_prsn_opt_efbegdt', 
            'enrolled_prsn_opt_efenddt_fix', 
            'enrolled_prsn_opt_efenddt', 
            'enrolled_opt_id', 
            'enrolled_bndl_cv_cat_id', 
            'enrolled_tot_plan_pr_at', 
            'enrolled_low_cost_pr_at', 
            'enrolled_ptax_pr_at', 
            'enrolled_atax_pr_at', 
            'enrolled_side_fund_at', 
            'enrolled_addl_act_refnmbrid', 
            'enrolled_has_prsn_opt_plan_sa_rec', 
            'enrolled_tot_enrl_prd_src_elect', 
            'enrolled_tot_enrl_prd_src_erseed', 
            'enrolled_has_prsn_act_rec', 
            'pyrl_freq_fc', 
            'act_type_cd', 
            'enrolled_opt_lbl_cd', 
            'enrolled_sv_area_id', 
            'is_contributing_hsa', 
            'has_employer_seed', 
            'is_no_coverage_option', 
            'enrolled_opt_year', 
            'plfm_id', 
            'clnt_id', 
            'prsn_brth_dt_mm_yy', 
            'age', 'prsn_pstl_cd', 
            'prsn_marital_stat', 
            'prsn_dth_dt', 
            'efctive_ts', 
            'expirtion_ts', 
            'prsn_trmn_dt', 
            'prsn_empl_stat_star_dt', 
            'prsn_empl_stat_end_dt', 
            'prsn_expc_ann_base_slry', 
            'prsn_expc_ann_base_slry_rng_desc', 
            'prsn_orgn_prsn_intn_id', 
            'is_tba_hm_client', 
            'is_tba_db_client', 
            'is_tba_dc_client', 
            'is_plan_brand_cd_likely_in_error', 
            'planpremiumpaid']


t = [] # implicit instantiation

for i in Num_list :
    t.append(Data2[i].describe())

t


## Write output in text file
f = open("myfile.txt","wb")
f.write(str(t))


## Write output in CSV file
import csv
with open("output.csv", "wb") as f:
    writer = csv.writer(f)
    writer.writerows(t)


# Get working directory
import os
os.getcwd()



Catvar = ['act_st_cd', 
            'enrolled_opt_plan_rspn_cd', 
            'enrolled_opt_tax_cd', 
            'enrolled_prsn_sa_efbegdt', 
            'enrolled_sa_id', 
            'enrolled_hdhp_opt_rslt_cd', 
            'enrolled_carr_prcs_id', 
            'enrolled_adj_act_type_cd', 
            'enrolled_prsn_cr_count', 
            'enrolled_cr_brnd_cd_list', 
            'enrolled_prsn_cr_at', 
            'pyrl_freq_cd', 
            'act_smry_brnd_cd', 
            'act_brnd_cd', 
            'act_ldsc_tx', 
            'plan_smry_brnd_cd', 
            'plan_brnd_cd', 
            'plan_type_cd', 
            'plan_ldsc_tx', 
            'enrolled_opt_brnd_cd', 
            'enrolled_opt_ldsc_tx', 
            'enrolled_cv_cat_brnd_cd', 
            'enrolled_cv_cat_lbl_cd', 
            'enrolled_cv_cat_ldsc_tx', 
            'enrolled_cnnc_cvtier_lbl_cd', 
            'option_metallic_level', 
            'is_option_hdhp', 
            'is_option_hsa', 
            'is_option_hra', 
            'is_option_cge', 
            'is_option_byo', 
            'option_medicare_type_cd', 
            'enrolled_sa_brnd_cd', 
            'enrolled_sa_ldsc_tx', 
            'plfm_nm', 'prsn_empl_stat', 
            'prsn_empl_stat_cd', 
            'prsn_gdr', 
            'prsn_gdr_desc', 
            'prsn_pstl_rgn', 
            'prsn_pstl_rgn_desc', 
            'prsn_ctry', 
            'prsn_ctry_desc', 
            'prsn_mrtl_stat_desc', 
            'prsn_hire_dt', 
            'prsn_fltm_prtm_ee_cd', 
            'prsn_fltm_prtm_ee_desc', 
            'prsn_hce_cd', 
            'prsn_hce_desc', 
            'prsn_hrly_slry_cd', 
            'prsn_hrly_slry_desc', 
            'prsn_ee_perm_stat_cd', 
            'prsn_ee_perm_stat_desc', 
            'prsn_rsn_cd', 
            'prsn_rsn_desc', 
            'clnt_nm', 
            'ind_desc_tx', 
            'enrolled_cv_cat_brnd_cd_derived',
            'is_contributing_hsa',
            'has_employer_seed', 
            'is_no_coverage_option', 
            'plfm_id', 
            'clnt_id']


CatSummary = [] # implicit instantiation

for i in Catvar:
    Data2[i] = Data2[i].astype('category')

for i in Catvar:
    CatSummary.append(Data2[i].value_counts())

CatSummary



# Data Quaity
import pydqc
import py2nb

import openpyxl
import xlsxwriter
import seaborn
import matplotlib_venn
import sklearn

