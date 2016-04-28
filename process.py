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
o = pd.read_sql_query("SELECT nazwa,firma, miejsce,opis FROM oferty",conn)
conn.close()


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
    

#for offer in text_offers:
o["text"] = text_offers
o = o.iloc[0:1000,]    


offer_lists = pd.DataFrame(columns=("offer_id", "list"), dtypes=)
re_all_lists = re.compile(r"^(.*\n+(\W*[\*-].*\n)(.+\n)*)",re.I | re.M)
for i, row in o.iterrows():
    print(i)
    print(type(i))
    t = row["text"]
    matches = re_all_lists.findall(t)
    for m in matches:
        offer_lists.loc[ len(offer_lists) ] = [i,m[0]]
        
        

def bayes_one(list_table, reg, attr):
    r = re.compile(reg)
    good_parts_ids = []
    bad_parts = []
    good_parts = []
    for i, row in list_table.iterrows():
        if r.match(row["list"]):
            good_parts_ids.append(i)
            good_parts.append(row["list"])
    
    for gi in good_parts_ids:
        #take bad parts
        offer_id = 
        bad = list_table[ list_table["offer_id"] == gi ]
        print(bad)
        bad = bad[ bad.index != gi ]
        #print(bad)
        if len(bad) > 0:
            for i, row in bad.iterrows():
                bad_parts.append(row["list"])
            
    #for g in good_parts:
    #    print(g)

    #for b in bad_parts:
    #    print(b)
            
#    inp = good_parts + bad_parts
#    out = [1]*len(good_parts) + [0]*len(bad_parts)
#    
#    count_vectorizer = CountVectorizer(binary=True)
#    #counts = count_vectorizer.fit_transform(o['oferujemy'].dropna().values)
#    counts = count_vectorizer.fit_transform(inp)
#
#    classifier = BernoulliNB()
#    classifier.fit(counts, out)
#    
#    cls_counts = count_vectorizer.transform(list_table["list"])
#    predictions = classifier.predict(cls_counts)
#    list_table[attr] = predictions
            

bayes_one(offer_lists, r"(Ofer.*\n+(\W*[\*-].*\n)(.+\n)*)"  , "oferujemy" )











