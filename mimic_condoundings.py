# %%

import pandas as pd
import numpy as np

pd.set_option('mode.chained_assignment', None)

services = pd.read_csv("./data/SERVICES.csv", parse_dates=['TRANSFERTIME'])
transfers = pd.read_csv("./data/TRANSFERS.csv", parse_dates=['INTIME', 'OUTTIME'])
callout = pd.read_csv("./data/CALLOUT.csv", parse_dates=['OUTCOMETIME'])
patients = pd.read_csv("./data/PATIENTS.csv", parse_dates=['DOD', 'DOB', 'DOD_HOSP', 'DOD_SSN'])
admissions = pd.read_csv("./data/ADMISSIONS.csv", parse_dates=["ADMITTIME"])

oasis = pd.read_csv("./data/oasis.csv")
elixhauser = pd.read_csv("./data/elixhauser.csv")

services.columns = services.columns.str.lower()
transfers.columns = transfers.columns.str.lower()
callout = callout.rename(columns=str.lower)
patients = patients.rename(columns=str.lower)
admissions = admissions.rename(columns=str.lower)

# In[ ]:

# Load the publicly accessible version of the Services table
# and date restrict it simply to reduce the size slightly by eliminating
# entries far outside the dates of interest
services = services[services['transfertime'] > pd.Timestamp('20010101')]

# In[ ]:

# Create a 'med_service_only' dataframe: essentially a copy of the Services table that only contains entries
# related to those patients who were taken care of exclusively by the MED service during their hospital admission.
# i.e. curr_service = 'MED' and size(hadm_id) = 1
row_ids = services.groupby('hadm_id').size()
row_ids = row_ids[row_ids < 2]
one_service_only = services[services['hadm_id'].isin(row_ids.index)]
med_service_only = one_service_only[one_service_only['curr_service'] == 'MED']

# %%

# In[ ]:
# Left join transfers to med_service_only.
# This creates a dataframe 'df' where every transfer in the database is represented, but only those patients
# taken care of exclusively by the MED service during their stay have data from the Services table.
df = pd.merge(transfers, med_service_only, how='left', on='hadm_id')

# In[ ]:
# Remove transfers that are not related to an ICU stay
df2 = df[df['icustay_id'].notnull()]

# Filter to specified dates
# MICU == CC6D & CC7D after April 10th, 2006 (until end of dataset)
df3 = df2[(df2['intime'] > pd.Timestamp('20060410'))]

# Select out those patients who were under the care of either of a 'West Campus' MICU team
# MSICU is a MICU but it is on the 'East Campus' and not of interest in this study.
df4 = df3[(df3['curr_service'] == 'MED') & (df3['curr_careunit'] != 'MSICU')]

# In[ ]:

# Trim down the dataframe that we will check each MICU patient against to
# determine the presence of inboarders (non-MICU patients boarding in the MICU)

inboarders = df3[(df3['curr_service'] != 'MED') &
                 (df3['curr_careunit'] == 'MICU')]

inboarders = inboarders[['intime', 'outtime', 'curr_careunit']]

# %%

df3.groupby(["curr_careunit", "curr_service"]).size()

df3[df3.curr_service.isna()].curr_careunit.value_counts()

df3.curr_service.isna().sum()

inboarders.shape

df4.shape

# %%


# In[ ]:

# For each patient under the care of a West Campus MICU team, calculate the number of
# non-MICU patients (i.e. cared for by other ICU teams) physically occupying MICU beds

# Start with a copy of the dataframe containing all the MICU patients
df5 = df4.copy()

# Create a column that defines 1 = patient being cared for by a MICU team in a location other
# than a MICU (e.g. in the SICU). We default to 0 here, then change the value if appropriate during for loop below.
df5['boarder_status'] = 0

# Create a column that distinguishes whether the patient is on the MICU Orange or Green service
# 0 = Orange, 1 = Green
df5['micu_team'] = 0

# Create columns that specify how many non-MICU patients were occupying MICU beds at the time
# each patient was admitted/transferred to the care of a MICU team
df5['cc6d_boarder_count'] = np.nan
df5['total_boarder_count'] = np.nan

for row_index, row in df5.iterrows():

    # Determine which patients in the inboarders dataframe (non-MICU patients in MICU beds) were in
    # MICU-Orange (CC6D) and MICU-Green (CC7D) beds at the time of each MICU patient's ICU stay intime
    combined_boarders = inboarders[((inboarders['intime'] < row['intime']) &
                                    (inboarders['outtime'] > row['intime'])) &
                                   (inboarders['curr_careunit'] == 'MICU')]

    # Store the inboarder counts in their respective columns
    df5.loc[row_index, 'total_boarder_count'] = len(combined_boarders.index)

    # If this row represents a MICU patient boarding in a non-MICU ICU bed, change 'boarder_status' to 1 (default = 0)
    if (row['curr_careunit'] != 'MICU'):
        df5.loc[row_index, 'boarder_status'] = 1

    # If this row represents a MICU patient in CC7D, it is almost certainly a patient cared for by the MICU Green team
    if (row['curr_careunit'] == 'MICU'):
        df5.loc[row_index, 'micu_team'] = 1

# %%

df5.micu_team.value_counts()

df5.head()

# %%

# In[6]:

# Team census and outboarder count for the MICU team taking care of a given patient
df5['team_census'] = np.nan
df5['team_outboarders'] = np.nan
df5['team_census_same_room'] = np.nan

# For each MICU patient...
for row_index, row in df5.iterrows():

    # ... being taken care of by the MICU-Orange team ...
    if row['micu_team'] == 0:

        # Determine how many patients (boarders + non-boarders) were assigned to the MICU Orange team at that time
        # NOT INCLUSIVE OF THIS PATIENT
        census = df5[(df5['intime'] < row['intime']) &
                     (df5['outtime'] > row['intime']) &
                     (df5['micu_team'] == 0)]

        # Determine how many NON-boarders the MICU-Orange service was taking care of at that time.
        # NOT INCLUSIVE OF THIS PATIENT
        nonboarders = census[census['curr_careunit'] == 'MICU']

        # Determine how many boarders the MICU-Orange service was taking care of at that time.
        # NOT INCLUSIVE OF THIS PATIENT
        outboarders = census[census['curr_careunit'] != 'MICU']
        #         outboarders = df5[(df5['transfers.intime'] < row['transfers.intime']) &
        #                           (df5['transfers.outtime'] > row['transfers.intime']) &
        #                           (df5['micu_team'] == 0) &
        #                           (df5['curr_ward'] != 'CC6D')]

        # Determine how many patients the MICU-Orange service was taking care of at that time...
        # ...IN THE SAME ROOM AS THIS PATIENT
        # ...NOT INCLUSIVE OF THIS PATIENT
        census_same_room = census[census['curr_careunit'] == row['curr_careunit']]

    # ... being taken care of by the MICU-Green team ...
    else:

        # Determine how many patients (boarders + non-boarders) were assigned to the MICU Green team at that time
        # NOT INCLUSIVE OF THIS PATIENT
        census = df5[(df5['intime'] < row['intime']) &
                     (df5['outtime'] > row['intime']) &
                     (df5['micu_team'] == 1)]

        # Determine how many NON-boarders the MICU-Green service was taking care of at that time.
        # NOT INCLUSIVE OF THIS PATIENT
        nonboarders = census[census['curr_careunit'] == 'MICU']

        # Determine how many boarders the MICU-Green service was taking care of at that time.
        # NOT INCLUSIVE OF THIS PATIENT
        outboarders = census[census['curr_careunit'] != 'MICU']
        #         outboarders = df5[(df5['transfers.intime'] < row['transfers.intime']) &
        #                           (df5['transfers.outtime'] > row['transfers.intime']) &
        #                           (df5['micu_team'] == 1) &
        #                           (df5['curr_ward'] != 'CC7D')]

        # Determine how many patients the MICU-Orange service was taking care of at that time...
        # ...IN THE SAME ROOM AS THIS PATIENT
        # ...NOT INCLUSIVE OF THIS PATIENT
        census_same_room = census[census['curr_careunit'] == row['curr_careunit']]

    df5.loc[row_index, 'team_census'] = len(census.index)
    df5.loc[row_index, 'team_outboarders'] = len(outboarders)
    df5.loc[row_index, 'team_census_same_room'] = len(census_same_room)

df5['other_team_census'] = np.nan
df5['other_team_outboarders'] = np.nan

# For each MICU patient...
for row_index, row in df5.iterrows():

    # ... being taken care of by the MICU-Orange team ...
    if (row['micu_team'] == 0):

        # Determine how many patients (boarders + non-boarders) were assigned to the MICU Green team at that time
        census = df5[(df5['intime'] < row['intime']) &
                     (df5['outtime'] > row['intime']) &
                     (df5['micu_team'] == 1)]

        # Determine how many boarders the MICU-Green service was taking care of at that time.
        outboarders = census[census['curr_careunit'] != 'MICU']
    #         outboarders = df5[(df5['intime'] < row['intime']) &
    #                           (df5['outtime'] > row['intime']) &
    #                           (df5['micu_team'] == 1) &
    #                           (df5['curr_ward'] != 'CC7D')]

    # ... being taken care of by the MICU-Green team ...
    else:

        # Determine how many patients (boarders + non-boarders) were assigned to the MICU Orange team at that time
        census = df5[(df5['intime'] < row['intime']) &
                     (df5['outtime'] > row['intime']) &
                     (df5['micu_team'] == 0)]

        # Determine how many boarders the MICU-Orange service was taking care of at that time.
        outboarders = census[census['curr_careunit'] != 'MICU']
    #         outboarders = df5[(df5['intime'] < row['intime']) &
    #                           (df5['outtime'] > row['intime']) &
    #                           (df5['micu_team'] == 0) &
    #                           (df5['curr_ward'] != 'CC6D')]

    df5.loc[row_index, 'other_team_census'] = len(census.index)
    df5.loc[row_index, 'other_team_outboarders'] = len(outboarders)

# %%


# Location restrict to the MSICU
msicu_transfers = transfers[(transfers['curr_careunit'] == 'MSICU')]

# In[12]:

# Team census and outboarder count for the Med/Surg ICU (an ICU on the hospital's other campus)
df5['msicu_team_census'] = np.nan
# df5['msicu_team_outboarders'] = np.nan


# For each MICU patient...
for row_index, row in df5.iterrows():
    # Determine how many patients (boarders + non-boarders) were assigned to the MICU Green team at that time
    census = msicu_transfers[(msicu_transfers['intime'] < row['intime']) &
                             (msicu_transfers['outtime'] > row['intime'])]

    df5.loc[row_index, 'msicu_team_census'] = len(census.index)

# In[14]:

# Add a column that estimates the EXPECTED number of outboarders
df5['expected_team_outboarders'] = np.nan
df5.expected_team_outboarders[(df5['micu_team'] == 0)] = (df5['team_census'] - (8 - df5['total_boarder_count']))
df5.expected_team_outboarders[(df5['micu_team'] == 1)] = (df5['team_census'] - (8 - df5['total_boarder_count']))

# Add a column that estimates the EXPECTED number of remaining beds in the nominal ICU of the team caring for the patient
df5['remaining_beds'] = np.nan
df5.remaining_beds[df5['micu_team'] == 0] = (
            8 - (df5['team_census'] - df5['team_outboarders']) - df5['total_boarder_count'])
df5.remaining_beds[df5['micu_team'] == 1] = (
            8 - (df5['team_census'] - df5['team_outboarders']) - df5['total_boarder_count'])

# In[15]:

# Add a column that estimates the EXPECTED number of outboarders for the OTHER MICU team
# (the one NOT taking care of the patient)
df5['other_expected_team_outboarders'] = np.nan
df5.other_expected_team_outboarders[(df5['micu_team'] == 0)] = (
            df5['other_team_census'] - (8 - df5['total_boarder_count']))
df5.other_expected_team_outboarders[(df5['micu_team'] == 1)] = (
            df5['other_team_census'] - (8 - df5['total_boarder_count']))

# Add a column that estimates the EXPECTED number of remaining beds in the OTHER MICU
# (the one NOT taking care of the patient)
df5['other_remaining_beds'] = np.nan
df5.other_remaining_beds[(df5['micu_team'] == 0)] = (
            8 - (df5['other_team_census'] - df5['other_team_outboarders']) - df5['total_boarder_count'])
df5.other_remaining_beds[(df5['micu_team'] == 1)] = (
            8 - (df5['other_team_census'] - df5['other_team_outboarders']) - df5['total_boarder_count'])

df5['initial_remaining_beds'] = df5['remaining_beds'] + df5['other_remaining_beds'] + df5['msicu_team_census']

df5.columns

df5.icustay_id.nunique()

# %%

callout.columns = callout.columns.str.lower()
callout.columns

df6 = pd.merge(df5, callout, left_on='hadm_id', right_on="hadm_id", how='left')

## define outcome
## discharge from ICU: 0
## gg at ICU or transfer to other icu

callout.columns

callout.callout_outcome.value_counts()

patients.columns = patients.columns.str.lower()

patients.expire_flag.value_counts()

patients.subject_id.nunique()

patients.columns

patients.dod.isnull().sum()

df6.shape

# %%

df7 = pd.merge(df6, patients, left_on='subject_id_x', right_on="subject_id", how='left')

list(df7.columns.sort_values())

df7.shape

df7 = df7.loc[:, ~df7.columns.duplicated()]

indicator = pd.DataFrame(df7.groupby('subject_id_x').outtime.max())

indicator = indicator.reset_index()
indicator.columns = ['subject_id_x', 'lasttime']

indicator

indicator.lasttime.dt.date

# %%

df8 = pd.merge(df7, indicator, how='left')
df8 = df8.loc[df8.outtime == df8.lasttime, :]

df8 = df8.sort_values('outcometime').groupby("subject_id_x").tail(1)

df8.groupby("subject_id_x").size().sort_values()

# df8.loc[df8.subject_id_x == 87906,:]
# print(df8.loc[df8.subject_id_x == 87906, ['hadm_id', 'hadm_id', 'lasttime', 'intime', 'outtime', 'dod', 'updatetime', 'outcometime', 'curr_careunit_x', 'callout_status', 'callout_outcome']])

pd.DataFrame(df8.expire_flag, df8.dod.dt.date - df8.outtime.dt.date)

df8.loc[:, ["expire_flag", "dod", 'outtime', 'callout_outcome']]

df8["within_24_hours"] = df8.dod.dt.date - df8.outtime.dt.date

df8['within_24_hours'] = df8.apply(lambda x: 1 if x.within_24_hours <= pd.Timedelta('1 days') else 0, axis=1)

df8.loc[:, ['dod', 'outtime', 'expire_flag', 'within_24_hours']]

df8.groupby(['expire_flag', 'within_24_hours']).size()

df8.head()

df8.groupby(["boarder_status", "expire_flag"]).size()

df8.groupby(["boarder_status", "within_24_hours"]).size()

# %%

a = 5544
b = 5643
c = 795
d = 961
p1 = a / (a + b)
p2 = c / (c + d)
p = (a + c) / (a + b + c + d)
n1 = a + b
n2 = c + d

# %%

a = 9318
b = 1869
c = 1440
d = 316
p1 = a / (a + b)
p2 = c / (c + d)
p = (a + c) / (a + b + c + d)
n1 = a + b
n2 = c + d

# %%

p1

p2

(p1 - p2) / np.sqrt(p * (1 - p) * (1 / n1 + 1 / n2))

# %%

df7 = df7.loc[:, ~df7.columns.duplicated()]

df7.groupby("subject_id_x").icustay_id.size().sort_values().describe()

df7.loc[df7.subject_id_x == 109, 'outtime'].sort_values()

df7.loc[df7.subject_id_x == 109, ['dod']]

# %%

age = pd.merge(admissions, patients, on="subject_id")

age["age"] = (age.admittime.dt.year - age.dob.dt.year)

age.loc[age.age > 200, "age"] = 89

age = age.rename(columns=str.lower)

# %%

### WE DEFINE A FINAL DATA TO WORK ON ###

final_data = df7.copy()

### FINAL DATA

### add age

final_data = pd.merge(final_data, oasis, how="left", on=["hadm_id", "icustay_id"])

final_data = pd.merge(final_data, elixhauser,
                      how="left",
                      on=["hadm_id", "subject_id"])

final_data.columns
