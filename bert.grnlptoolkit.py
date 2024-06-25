from gr_nlp_toolkit import Pipeline
import pandas as pd
import numpy as np
import csv
nlp = Pipeline("ner")
data = pd.read_csv("/home/stathis/Desktop/res/ref/data/2016/q2016.unique.per.w.csv")
i = 200000
v = list()
s = list()
while i < len(data.index):
	text = data.loc[i][5]
	doc = nlp(text)
	for token in doc.tokens:
		print((i+1)/2000)
#		print(token.text)
#		print(token.ner)
		if token.ner !="O":
			v.append(i)
			v.append(token.text)
			v.append(token.ner)
	s.append(v)
	v = list()
	i += 1



with open('/home/stathis/Desktop/s2016.200.to.len.csv', 'w') as f:
	write = csv.writer(f)
	i = 0
	while i < len(s):
		write.writerow(s[i])
		i += 1