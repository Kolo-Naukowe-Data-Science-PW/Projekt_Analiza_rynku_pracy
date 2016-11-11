# -*- coding: utf-8 -*-

import sqlite3
import re
from collections import Counter
import fim  # Frequent Itemset Mining

import json
import math

import os
os.chdir('/home/michal/Projekty/Projekt_Analiza_rynku_pracy/reguly_asocjacyjne')


db = sqlite3.connect('../oferty_segmenty.db')
cursor = db.cursor()

cursor.execute("select wymagania from oferty")
wymagania = cursor.fetchall()

db.close()

wymagania_copy = wymagania


wymagania = wymagania_copy
wymagania = [w[0].lower() for w in wymagania]

wymagania = [
    w.replace("c/c++", " c_c++ ")
    .replace("c++", " c_c++ ")
    .replace("data mining", " data_mining ")
    .replace("machine learning", " machine_learning ")
    .replace("big data", " big_data ")
    .replace("bigdata", " big_data ")
    .replace("t-sql", " tsql ")
    .replace("power point", " powerpoint ")
    .replace("visual studio", " visual_studio ")
    .replace(".net", " dot_net ")
    for w in wymagania
]


def replace(wymagania, regexp, repl):
    regex = re.compile(regexp)
    return([
        regex.sub(repl, w)
        for w in wymagania
    ])

wymagania = replace(wymagania, 
                    r'uczeni\w+ maszyn\w+', 
                    ' machine_learning ')
wymagania = replace(wymagania, 
                    r'matematy\w+', 
                    ' matematyka ')
wymagania = replace(wymagania, 
                    r'statysty\w+', 
                    ' statystyka ')
wymagania = replace(wymagania, 
                    r'informaty\w+', 
                    ' informatyka ')
wymagania = replace(wymagania, 
                    r'python\w+', 
                    ' python ')
wymagania = replace(wymagania, 
                    r'(ms sql server)|(ms sql)|(sql server)|(microsoft sql)', 
                    ' ms_sql_server ')        
wymagania = replace(wymagania, 
                    r'postgre\w*', 
                    ' postgresql ')
wymagania = replace(wymagania, 
                    r'\Wr\W', 
                    ' r ')
wymagania = replace(wymagania, 
                    r'.js\W', 
                    'js ')
wymagania = replace(wymagania, 
                    r'\Wjs\W', 
                    ' javascript ')
               


podzbior = set((
    # ogólne zagadnienia
    "machine_learning", "data_mining", "big_data",
    
    # studia
    "matematyka", "statystyka", "informatyka", "fizyka",
                
    # jezyki programowania
    "c_c++",  "java", "scala", "python", "sas", "sql", "bash", "tsql", "r", 
    "c#", "vba", "matlab", "shell", "spss", "statistica", "html", "css", 
    "javascript", "perl",
    
               
    # technologie / programy / biblioteki 
    "kafka", "spark", "hadoop", "eclipse", "git", "jenkins", "hbase", "hdfs",
    "pig", "mapreduce", "hive", "splunk", "nosql", "elasticsearch", "flume",
    ".net", "sqoop", "ms_sql_server", "mysql", "postgresql", "mongodb", "linux"
    "docker", "kuberntes", "mesos",
    "excel", "powerpoint", "xml",
    "bootstrap", "angularjs", "nodejs", "d3js",
    "grep", "sed", "awk"
))

regex = re.compile(r'[\w+#]+')
wymagania = [regex.findall(w) for w in wymagania]
wymagania = [
    [w for w in wym if w in podzbior]
    for wym in wymagania
]


word_counts = Counter()
for w in wymagania:
    word_counts.update(w)

word_counts.most_common(100)

wymagania = [ w for w in wymagania if w ]


itemsets = fim.fpgrowth(wymagania, 
                        target = 'r',
                        zmin = 2, zmax = 2,
                        supp = 0.40,
                        conf = 20,
                        eval = 'l', 
                        report = '(acl')

'''
nodes = [
    {"id": id, "group":1} for id, count in word_counts.items()
]

links = [
    {"source":left, "target":right[0], "value": math.log(numbers[2] + 1) }
    for left, right, numbers in itemsets
]

graph = {
    "nodes": nodes,
    "links": links
}


with open("rules.json", "w") as file:
    file.write(json.dumps(graph, indent = 1))
'''

'''
with open("nodes.csv", "w") as file:
    file.write("title\n")
    file.write("\n".join(word_counts.keys()))
'''

links = [
    left + "," + right[0] + "," + str(numbers[2])
    for left, right, numbers in itemsets
]

with open("links.csv", "w") as file:
    file.write("source,target,lift\n")
    file.write("\n".join(links))






