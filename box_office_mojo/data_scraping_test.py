from lxml import etree
import urllib.request
import pandas as pd
import time
import socket
import telnetlib
import requests
import random
import json
import threading
import urllib.error
import logging
from multiprocessing import Pool
from requests.exceptions import ConnectionError
from fake_useragent import UserAgent


url = 'https://www.boxofficemojo.com/year/?area='
abb_br = ['CN','IT','RU','ID','TR','SA','PL','TH','IR','AT','AE','NG','ZA','SG','MY','PH','PK','CL','BD','EG','VN','PT','CZ','RO','PE','NZ','GR','IQ','DZ','QA','KZ','HU','AO','KW','SD','UA','MA','EC','CU','SK','LK','ET','KE','DO','OM','MM','LU','PA','GH','BG','CR','UY','HR','BY','LB','TZ','UZ','SI','LT','CS','AZ','TN','BO','BH','CM','YE','LV','EE','UG','NP','SV','KH','TT','CY','ZW','SN','AF','BA','LA','ML','GE','GA','JM','NA','AL','MZ','MT','GQ','BN','AM','MG','MN','GN','TD','BJ','RW','MD','NE','KG','TJ','FJ','MR','MV','ME','TG','BB','SL','GY','LR','BI','SR','TL','LS','AG','SC','GM','SB','GD','KM','VU','WS','DM','TO','KI']
urls = []

for i in abb_br:
    new_url = url + i
    urls.append(new_url)


ua = UserAgent()
error_file = 'd:/work/github/imdb_br/box_office_mojo/error.txt'
headers = {'User-Agent':ua.random}
count =1
max_count = 2
p = Pool(20) 


def main(url,i,count):   
    def get_one_page(url,count):
        print('Crawing',url)
        print('Trying count',count)
        if count >= max_count:
            print("Tried too many counts")
            return None
        try:
            response = requests.get(url = url, headers = headers, timeout = 60 ,allow_redirects = False)
            if response.status_code == 200:
                return response.text
            if response.status_code == 302:
                print('302')
        except ConnectionError as e:
            print('Error Occurred',e.args)
            count += 1
            return get_one_page(url,count)

    html = get_one_page(url,count) 
    try:
        html_file = 'd:/work/github/imdb_br/box_office_mojo/html/%s.html' % tt_list[i]
        with open (html_file,'w+',encoding = 'utf8') as f :
            f.write(html)
            print(i,'write success %s' % html_file)
    except:
        with open (error_file,'a+',encoding = 'utf8') as f :
            f.write(url)
            f.write('\r\n')
            print(i,'write not success %s' % url)
    finally:
        f.close()    

for i in range(0,len(urls)):
    url = urls[i]
    p.apply_async(main, args=(url,i,count))

p.close()
p.join()



