import pandas as pd
from sklearn.impute import KNNImputer
import time
#timing the ting
st = time.time()

# Read the dataset from a CSV file
df = pd.read_csv('/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/imputation/df_all_numeric.csv')
# Detect columns with missing values
missing_cols = df.columns[df.isnull().any()].tolist()

# Separate input and output features
input_cols = [col for col in df.columns if col not in missing_cols]
output_cols = missing_cols

# Apply KNN imputation
imputer = KNNImputer()
df_imputed = pd.DataFrame(imputer.fit_transform(df[output_cols + input_cols]), columns=output_cols + input_cols)

# Output the imputed dataset
df_imputed.to_csv('/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/imputation/df_all_numeric_imputed.csv', index=False)
#end the timer
et = time.time()
elapsed_time = et - st
print('Execution time:', elapsed_time, 'seconds')