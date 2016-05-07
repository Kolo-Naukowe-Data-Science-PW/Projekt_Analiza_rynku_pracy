# -*- coding: utf-8 -*-
"""
Created on Sun Apr 10 21:32:37 2016

@author: mic
"""

import pandas as pd
import sqlite3
import lxml.html
import re
import numpy as np

from sklearn.feature_extraction.text import CountVectorizer
from sklearn.naive_bayes import BernoulliNB


dbpath = "oferty_lang.db"
conn = sqlite3.connect(dbpath)
o = pd.read_sql_query("""SELECT nazwa,firma, miejsce,opis FROM oferty\
                         WHERE lang=="pl" """,conn)
conn.close()

print("Database loaded with {0} entries", len(o))

html = o["opis"]

text_offers = []

for html_content in html:
    offer = lxml.html.fromstring(html_content)
    #print(html_content)    
    
    for br in offer.xpath("*//br"):
        br.tail = "\n" + br.tail if br.tail else "\n"
        
    #for br in offer.xpath("*//p"):
    #    br.tail = "\n" + br.tail if br.tail else "\n"
    for para in offer.xpath("*//p"):
        if para.text:
            para.text = "\n%s\n" % para.text
        else:
            para.text = "\n"
        
    for li in offer.xpath("*//li"):
        li.tail = "\n" + li.tail if li.tail else "\n"
    			
    for li in offer.xpath("*//li"):
        if li.text:
            li.text = "*" + li.text
        else:
            li.text = "*"
            
    for ul in offer.xpath("*//ul"):
        if ul.xpath("ancestor::ul"):
            continue
        ul.tail = "\n" + ul.tail if ul.tail else "\n"
        if ul.text:
            ul.text = ul.text + "\n"
        else:
            ul.text = "\n"
    
    
    text = offer.text_content().strip()
    #print(text)
    text_offers.append(text)
    


o["text"] = text_offers
print("Parsing html to text done")



#o = o.iloc[0:1000,]    
#Wygląda na to, że wydłużanie tabeli w pandas jest
#OKROPNIE wolne!

#lists = pd.DataFrame(columns=("offer_id", "list"))
#re_all_lists = re.compile(r"^(.*\n+(\W*[\*-].*\n)(.+\n)*)",re.I | re.M)
##for i, row in o.itertuples():
#for row in o.itertuples():
#    i = row[0] 
#    t = row[-1]
#    print(i)
#    matches = re_all_lists.findall(t)
#    for m in matches:
#        lists.loc[ len(lists) ] = [i,m[0].strip()]
        
#UWAGA -> przyjmuje brak zmian w numerowaniu indexow        
l = []
re_all_lists = re.compile(r"^(.*\n+(\W*[\*-].*\n)(.+\n)*)",re.I | re.M)
for i,t in enumerate(o.loc[:, "text"]):
    matches = re_all_lists.findall(t)
    for m in matches:
        l.append([i,m[0].strip("\n\t ")])
lists = pd.DataFrame(l, columns=("offer_id", "list"))        
    
print("Extracting lists done") 
   
def bayes_one(parts, start, attr):
    good_parts_ids = []
    bad_parts = []
    good_parts = []
    for i, row in parts.iterrows():
        #if r.match(lists.loc[0,"list"].split('\n',1)[0]):
        if row["list"].split("\n", 1)[0].strip(":\t ").lower() in start:
            good_parts_ids.append(i)
            good_parts.append(row["list"])
    
    for gi in good_parts_ids: 
        bad = parts[ parts["offer_id"] == parts.loc[gi,"offer_id" ] ]     
        bad = bad[ bad.index != gi ]

        if len(bad) > 0:
            for i, row in bad.iterrows():
                bad_parts.append(row["list"])
    
#    for g in good_parts:
#        print(g)
#
#    for b in bad_parts:
#        print(b)
        
        
    print("Sizeof good parts")
    print(len(good_parts))
    print("Sizeof bad parts")
    print(len(bad_parts))   
        
    inp = good_parts + bad_parts
    out = [1]*len(good_parts) + [0]*len(bad_parts)
    
    count_vectorizer = CountVectorizer(binary=True)
    #counts = count_vectorizer.fit_transform(o['oferujemy'].dropna().values)
    counts = count_vectorizer.fit_transform(inp)

    classifier = BernoulliNB()
    classifier.fit(counts, out)
    
    cls_counts = count_vectorizer.transform(parts["list"])
    predictions = classifier.predict(cls_counts)
    parts[attr] = predictions
            

#bayes_one(lists, r"(Ofer.*\n+(\W*[\*-].*\n)(.+\n)*)"  , "oferta" )
#bayes_one(lists, r"(Wymag.*\n+(\W*[\*-].*\n)(.+\n)*)"  , "wymagania" )
#bayes_one(lists, r"(Obowi.*\n+(\W*[\*-].*\n)(.+\n)*)"  , "obowiazki" )

bayes_one(lists, ["oferta", "oferujemy", "oferujemy",
                  "zapewniamy", "kandydatom oferujemy",
                  "firma oferuje"]  , "oferta" )
print("bayes: oferta - done")
bayes_one(lists, ["wymagania", "wymagamy",
                  "oczekiwania", "oczekujemy",
                  "nasze oczekiwania"]  , "wymagania" )
print("bayes: wymagania - done")
bayes_one(lists, ["zakres obowiązków", "obowiązki",
                  "będziesz odpowiedzialny za",
                  "zakres odpowiedzialności",
                  "twoje zadania",
                  "główne zadania",
                  "zakres zadań", "zadania", "główne obowiązki"],
                  "obowiazki" )
print("bayes: obowiazki - done")

#print(lists)
#lists[ lists["oferta"] == 1]


lists[ lists["oferta"] == 1 ].loc[:,"list"].\
    apply(lambda x: x.split("\n", 1)[0]).value_counts()

lists[ lists["wymagania"] == 1 ].loc[:,"list"].\
    apply(lambda x: x.split("\n", 1)[0]).value_counts()

lists[ lists["obowiazki"] == 1 ].loc[:,"list"].\
    apply(lambda x: x.split("\n", 1)[0]).value_counts()


#lists["oferta"].apply( lambda x: x)

#Empty
empty = lists[ lists.iloc[:,2:].sum(axis=1) == 0  ]
print(len(empty))
print(empty)

#Many
many = lists[ lists.iloc[:,2:].sum(axis=1) > 1  ]
print(len(many))
print(many)
 


#lists.iloc[(18,:]










