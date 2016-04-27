# -*- coding: utf-8 -*-
"""
Created on Wed Apr 27 22:34:40 2016

@author: mic
"""

import langdetect



import pandas as pd
import sqlite3
import lxml.html


dbpath = "oferty.db"
conn = sqlite3.connect(dbpath)
o = pd.read_sql_query("SELECT * FROM oferty",conn)
conn.close()


o["lang"] = ""
for idx, row in o.iterrows():
    offer = lxml.html.fromstring(row.opis)
    text = offer.text_content().strip()
    if text and len(text) > 0:
        lang = langdetect.detect(text)
    else:
        lang = ""
    
    o.loc[idx, "lang"] = lang
    
conn = sqlite3.connect("oferty_lang.db")
o.to_sql("oferty", conn, if_exists="replace" )
conn.close()    
    
    



