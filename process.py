# -*- coding: utf-8 -*-
"""
Created on Sun Apr 10 21:32:37 2016

@author: mic
"""

import pandas as pd
import sqlite3
import lxml.html
import re





dbpath = "oferty.db"
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



    
oferujemy = []
r = re.compile(r"(Ofer.*\n+(\W*[\*-].*\n)(.+\n)*)",re.I | re.M)
for i,t in enumerate(text_offers):
    m = r.search(t)
    if m:
        #o[i].oferta = m.group(0)
        oferujemy.append(m.group(0))
    else:
        #oferujemy.append('') #or...None?
        oferujemy.append(None)
        

        
o["oferujemy"] = oferujemy   


print( "Number of offer rows: {0}".format(len(o)))
print( "Number of oferujemy rows: {0}".format(
    o["oferujemy"].isnull().sum()))


other_lists = []        
r_all_lists = re.compile(r"^(.*\n+(\W*[\*-].*\n)(.+\n)*)",re.I | re.M)
for t in o["text"][o["oferujemy"].notnull()].values:
    matches = r_all_lists.findall(t)
    #print(matches)
    for m in matches:
        m = m[0]
        #print(m)
        if not r.match(m):
            other_lists.append(m)
        
offer_list = [x for x in oferujemy if x is not None ]

to_classify = []
to_cls_id = []        
for i,t in enumerate(o["text"].values):
    if not o["oferujemy"][i]:
        matches = r_all_lists.findall(t)
        for m in matches:
            m = m[0]
            to_classify.append(m)
            to_cls_id.append(i)

        
        
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.naive_bayes import BernoulliNB

inp = offer_list + other_lists
out = [1]*len(offer_list) + [0]*len(other_lists)

count_vectorizer = CountVectorizer(binary=True)
#counts = count_vectorizer.fit_transform(o['oferujemy'].dropna().values)
counts = count_vectorizer.fit_transform(inp)

classifier = BernoulliNB()

classifier.fit(counts, out)

cls_counts = count_vectorizer.transform(to_classify)

predictions = classifier.predict(cls_counts)
           

for x in range(40):
    print(to_cls_id[x])
    print(predictions[x])
    print(to_classify[x])










