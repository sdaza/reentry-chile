
# attrition exercise

# %%
import pandas as pd
import datetime
import matplotlib.pyplot as plt
import matplotlib.dates as dates
import re
from dateutil.relativedelta import relativedelta
from ggplot import *

# %%
url = 'https://www.dropbox.com/s/au959gfz1ca1hdx/20170628%20Logros.xlsx?dl=1'
file = pd.ExcelFile(url)
file.sheet_names

df = file.parse(-1, parse_dates=True, date_parser=lambda x: pd.to_datetime(x, dayfirst=True))
type(file)

# define today
today = datetime.datetime.now()
start = pd.to_datetime("01/09/2017", dayfirst=True)
print(start)

# %%
df = df.reset_index()
df.columns = map(str.lower, df.columns)
cnames = list(df.columns)

old_names = ['id folio', 'encuestadora final', 'fecha egreso', 'fecha egreso definitiva']
new_names = ['id', 'interviewer', 'release_o', 'release']

df.rename(columns=dict(zip(old_names, new_names)), inplace=True)

df['release_o']
df['release']
print(df.id.duplicated().value_counts())


df.loc[:,['release']].min()

df['release'].isnull().value_counts()
df['release_o'].isnull().value_counts()


mrelease = df.release.isnull()
mrelease.value_counts()

df.loc[mrelease, "release"] = df.loc[mrelease, "release_o"]
# s = (df.release < start) & (df.release_o >= start)
# df.loc[s,'release'] = df.loc[s, 'release_o']

df.release.min()
df.release.max()

df.loc[df.release == df.release.max(), ['id', 'release', 'release_o']]
df.loc[df.release == df.release.min(), ['id', 'release', 'release_o']]

# run these lines twice
df.loc[df.release == df.release.min(), ['release']] = df.loc[df.release == df.release.min(), ['release_o']]
df.loc[df.release == df.release.min(), ['release']] = df.loc[df.release == df.release.min(), ['release_o']]
df.loc[df.release == df.release.min(), ['id', 'release', 'release_o']]

mrelease = df.release.isnull()
mrelease.value_counts()

df['release_year'] = pd.DatetimeIndex(df['release']).year
df['release_month'] = pd.DatetimeIndex(df['release']).month
df.release_year.value_counts()
df.release_month.value_counts()

n = df.reset_index().set_index('release').resample('M', how={'id': 'count'})
n = n.rename(columns={'id': 'cases'})

n.plot(kind = "bar")
plt.show()

 print('# Baseline')

# baseline response
varname = [col for col in df.columns if 'sí: se realizó línea base' in col]
print(varname)

df = df.rename(columns={varname[0]: 'r_baseline'})
df.r_baseline.value_counts()
df['r_baseline'] = df['r_baseline'].str.contains("s|S").astype(int)
df.r_baseline.value_counts()  # only ones

varname = [col for col in df.columns if 'fecha entrevista' in col]
print(varname)

df = df.rename(columns={varname[0]: 'dbaseline'})
df.dbaseline.describe()

# function to clean dates
def clean_dates(text):
   if (isinstance(text, str)):
      text = str(text)
      pp = re.compile("-|\.")
      r = re.sub(pp, "/", text)
      pp = re.compile("([0-9]+/[0-9]+/[0-9]+)|([0-9]+-[0-9]+-[0-9]+)")
      r = re.search(pp, r)
      if (r is not None):
         r = r.group()
      else:
         r = ''
   else:
      r = text
   return pd.to_datetime(r, dayfirst=True)

df['cdbaseline'] = [clean_dates(i) for i in df['dbaseline']]

m = df['cdbaseline'].isnull() & df['dbaseline'].notnull()
df.loc[m, ['cdbaseline', 'dbaseline']]

# replace errors in data entry
errors = ['V16/12716', 'V14 y M18/10']
new_values = ['16/12/16', '18/10/16']

for i in range(0,len(errors)):
   df.loc[df['dbaseline'] == errors[i], 'cdbaseline'] = pd.to_datetime(new_values[i], format='%d/%m/%y')

m = df['cdbaseline'].isnull()
m.value_counts()
df.loc[m, ['cdbaseline', 'dbaseline']]


# %% identify inconsistent cases

df = df.assign(release_baseline = (df.cdbaseline - df.release).dt.days)
df.release_baseline.describe()
df.release_baseline.hist(bins = 40)
plt.show()

s = ((df.release_baseline > 0) | (df.cdbaseline.isnull()))
s.value_counts()
df.loc[s, ['id', 'r_baseline', 'release_baseline', 'release', 'cdbaseline', 'dbaseline']].sort('id')


print('# First week')

varname = [col for col in df.columns if 'participa' in col]
print(varname)

df = df.rename(columns={varname[1]: 'r_week'})
df.r_week.value_counts()
df['r_week'] = df['r_week'].str.contains("s|S").astype(float)
df['r_week'] = df.r_week.fillna(0)
df.r_week.isnull().value_counts()
df.r_week.value_counts()

# df.loc[df.r_week==1, ['id']].sort('id')

# date week
varname = [col for col in df.columns if 'fecha entrevista' in col]
print(varname)

df = df.rename(columns={varname[0]: 'dweek'})
df.dweek.describe()

df['cdweek'] = [clean_dates(i) for i in df['dweek']]
df.cdweek.describe()

m = df.cdweek.isnull() & df.dweek.notnull()
df.loc[m, ['id', 'cdweek', 'dweek']]


# replace errors in data entry
def excel_datetime(xldate, datemode = 0):
    # datemode: 0 for 1900-based, 1 for 1904-based
    return (
        datetime.datetime(1899, 12, 30)
        + datetime.timedelta(days=xldate + 1462 * datemode)
        )


errors = ['42671', 'S29/10 y W02/11']
new_values = [excel_datetime(42671, 0), '11/02/2016']

for i in range(0,len(errors)):
   df.loc[df['dweek'] == errors[i], 'cdweek'] = pd.to_datetime(new_values[i], dayfirst=True)

s = (df.r_week == 1)
df.loc[s, 'cdweek'].isnull().value_counts()
#df.loc[s,['id', 'r_week', 'release', 'week_dealine', 'cdweek']]

df = df.assign(release_week = (df.cdweek - df.release).dt.days)
df.release_week.describe()

df.loc[df.release_week < -5000, ['id', 'r_week', 'release', 'week', 'release_week']]
df.release_week.hist()
plt.show()


df['week_deadline'] = pd.DatetimeIndex( df['release'] ) + pd.DateOffset(weeks = 5)

df.loc[df.r_week == 1, 'cdweek'].isnull().value_counts()

s = ((df.r_week == 1) & (today > df.week_deadline)) & ( (df.release_week <= 0) | (df.cdweek.isnull()) )


df.loc[s,['id', 'r_week', 'release', 'cdweek', 'week_deadline']].sort('id')


tab  = df.loc[ (today > df.week_deadline), 'r_week'].value_counts()
[i / sum(list(tab)) for i in l]

# two months

# cnames
varname = [col for col in df.columns if 'participa ' in col]
print(varname)

df = df.rename(columns={varname[0]: 'r_2months'})
df.r_2months.value_counts()
df['r_2months'] = df['r_2months'].str.contains("s|S").astype(float)
df['r_2months'] = df.r_2months.fillna(0)
df.r_2months.isnull().value_counts()
df.r_2months.value_counts()

# df.loc[df.r_2months==1, ['id']].sort('id')

# date 2months
varname = [col for col in df.columns if 'fecha entrevista' in col]
print(varname)

df = df.rename(columns={varname[0]: 'd2months'})
df.d2months.describe()

df['cd2months'] = [clean_dates(i) for i in df['d2months']]
df.cd2months.describe()

m = df.cd2months.isnull() & df.d2months.notnull()
df.loc[m, ['id', 'release', 'cd2months', 'd2months']]


# replace errors in data entry
errors = ['27/2']
new_values = ['27/02/2017']

for i in range(0,len(errors)):
   df.loc[df['d2months'] == errors[i], 'cd2months'] = pd.to_datetime(new_values[i])

s = (df.r_2months == 1)
df.loc[s, 'cd2months'].isnull().value_counts()
#df.loc[s,['id', 'r_2months', 'release', '2months_dealine', 'cd2months']]

df = df.assign(release_2months = (df.cd2months - df.release).dt.days)
df.release_2months.describe()
df.release_2months.hist()
plt.show()


df['two_months_deadline'] = pd.DatetimeIndex( df['release'] ) + pd.DateOffset(months = 5)

df.loc[df.r_2months == 1, 'cd2months'].isnull().value_counts()

s = ((df.r_2months == 1) & (today > df.two_months_deadline)) & ( (df.release_2months <= 0) | (df.cd2months.isnull()) )


df.loc[s,['id', 'r_2months', 'release', 'cd2months', '2months_deadline']].sort('id')


tab  = df.loc[ (today > df.2months_deadline), 'r_2months'].value_counts()
[i / sum(list(tab)) for i in l]
