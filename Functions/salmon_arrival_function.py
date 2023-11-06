# replicate the salmon part in python

import pandas as pd
import scipy as sp
import matplotlib.pyplot as plt

df_sockeye = pd.read_csv("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Data/BallardDailyCounts2012-2022.csv")
df_sockeye.head()

# create the day of year column
days_of_study = list(range(163,276)) * 11
# print(days_of_study)
# len(days_of_study)
df_sockeye["day_of_study"] = days_of_study

# calculate average per day of study
df_sockeye["DailyCount"].mean()

#pivot table first attempt
daily_sockeye = df_sockeye.pivot_table(index = "day_of_study", aggfunc = "mean", values = "DailyCount")
daily_sockeye["day_of_study"] = list(range(163, 276))

yr_day_sockeye = df_sockeye.pivot(index = "day_of_study", columns = "Year", values = "DailyCount")

# plot the average daily counts
daily_sockeye.plot(kind = "line")
plt.show()

# plot each year separately
yr_day_sockeye.plot(kind = "line")
plt.show()
from scipy.stats import norm
y_hat = np.repeat((-1), len(df_sockeye))
def fit_to_fish(params, data):
  for t in range(len(data)):
    y_hat[t] = norm.pdf(x = t, loc = params[1], scale = params[2]) * params[0]
  print(y_hat)

# test function
params_1 = [73500, 23.6, 14]
fit_to_fish(params = params_1, data = df_sockeye)

