#Importing libraries
import pandas as pd
import os
import glob
import numpy as np

#Variables set up
input_folder = (r'C:\Users\inesr\OneDrive\Documentos\Gapminder_data_project\\')
# input_files = glob.glob(os.path.join(input_folder, "*.csv"))
# extension = 'csv'
# os.chdir(input_folder)
input_files = glob.glob('*.csv')
output_folder = (r'C:\Users\inesr\OneDrive\Documentos\Gapminder_data_project\output\\')
output_file = 'gapminder_data.csv'

#%% IMPORT FILES
#Import all files - unpivot - add a new column for the metric using the file name. Then replace any na with 0

df = pd.DataFrame()
# for i in input_files:
#     df[i] = pd.read_csv(i).melt(id_vars=['country'], var_name='year', value_name=i).set_index(['country', 'year']).dropna()

for i in input_files:
    df[i] = pd.read_csv(input_folder+i).melt(id_vars=['country'], var_name='year', value_name=i).set_index(['country', 'year']).dropna()

df = df.rename(columns = lambda x : str(x)[:-4])  # remove the ".csv" from the column name
df = df.fillna(0)           #using pandas
# df = df.replace(np.nan,0) #using numpy

#%%
#Convert k, m, b to units
#Define function to remove the symbols and multiply to obtain units
def value_to_float(x):
    if type(x) == float or type(x) == int:
        return x
    if 'K' in x:
        if len(x) > 1:
            return float(x.replace('K', '')) * 1000
        return 1000.0
    if 'k' in x:
        if len(x) > 1:
            return float(x.replace('k', '')) * 1000
        return 1000.0
    if 'M' in x:
        if len(x) > 1:
            return float(x.replace('M', '')) * 1000000
        return 1000000.0
    if 'm' in x:
        if len(x) > 1:
            return float(x.replace('m', '')) * 1000000
        return 1000000.0
    if 'B' in x:
        return float(x.replace('B', '')) * 1000000000
    return 0.0
    if 'b' in x:
        return float(x.replace('b', '')) * 1000000000
    if 'TR' in x:
        return float(x.replace('b', '')) * 1000000000000
    return 0.0


#Apply function to a loop for all the columns in the dataframe
lenght = len(df.columns)

for i in range(lenght):
    df.iloc[:,i]= df.iloc[:,i].apply(value_to_float)
    df.iloc[:,i]


type(df.iloc[15000,3])
    
#%% Asign a region for each country

#to be run just once - to get a summary of all the countries needed
# countries = df.reset_index(level=['country', 'year']).country.unique()
# countries = pd.DataFrame(countries)
# countries.to_csv( output_folder + 'gapminder_data.csv', index=False)  

#load country to region mapping file; taken the gapminder_data file and using our world in data mappings created in excel
region_map = pd.read_csv(output_folder + 'country_mappping.csv')

#move indexes (country and year) as columns
df = df.reset_index(level=['country', 'year'])

#map country to region and add as a new variable
df = df.merge(region_map, on='country', how='left')


#%% create a summary of all the columns to understand their distribution
# send to csv
summaries = df.describe()
summaries.to_csv(output_folder + 'summary.csv')

#%% Export data to csv
  
# saving the csv
df.to_csv(output_folder + output_file)

