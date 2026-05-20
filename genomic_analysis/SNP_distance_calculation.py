# 提取 list_1248 中菌株的两两 SNP 距离
# 输入文件: distance_new4560.txt, list_175.txt
# 输出文件: distance_175.txt

input_file = "distance_new4560.txt"
list_file = "list_175.txt"
output_file = "distance_175.txt"


with open(list_file, "r") as f:
    strains = set(line.strip() for line in f)

print(f"List size: {len(strains)}")


with open(input_file, "r") as fin, open(output_file, "w") as fout:
    for line in fin:
        parts = line.strip().split()
        if len(parts) < 3:
            continue
        id1, id2, distance = parts[0], parts[1], parts[2]
        if id1 in strains and id2 in strains:
            fout.write(line)

print("Done! Save as", output_file)
