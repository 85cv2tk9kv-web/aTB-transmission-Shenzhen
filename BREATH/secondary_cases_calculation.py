import pandas as pd

# 读取 WIW 文件（tab 分隔）
df = pd.read_csv("wiw.csv", sep="\t", index_col=0)

# 统计每列中非零值的个数（即继发病例数）
secondary_cases = (df > 0.3).sum(axis=0)

# 整理为 DataFrame 并保存
result = secondary_cases.reset_index()
result.columns = ['StrainID', 'SecondaryCases']
result.to_csv("secondary_cases_2.csv", index=False)

print("Done!Save as secondary_cases.csv")
