import requests
from multiprocessing import Pool
import os

url = 'https://www.boxofficemojo.com/year/?area='
abb_br = ['CN','IT','RU','ID','TR','SA','PL','TH','IR','AT','AE','NG','ZA','SG','MY','PH','PK','CL','BD','EG','VN','PT','CZ','RO','PE','NZ','GR','IQ','DZ','QA','KZ','HU','AO','KW','SD','UA','MA','EC','CU','SK','LK','ET','KE','DO','OM','MM','LU','PA','GH','BG','CR','UY','HR','BY','LB','TZ','UZ','SI','LT','CS','AZ','TN','BO','BH','CM','YE','LV','EE','UG','NP','SV','KH','TT','CY','ZW','SN','AF','BA','LA','ML','GE','GA','JM','NA','AL','MZ','MT','GQ','BN','AM','MG','MN','GN','TD','BJ','RW','MD','NE','KG','TJ','FJ','MR','MV','ME','TG','BB','SL','GY','LR','BI','SR','TL','LS','AG','SC','GM','SB','GD','KM','VU','WS','DM','TO','KI']
abb_non_br = ['US','JP','DE','IN','GB','FR','BR','CA','KP','AU','ES','MX','NL','CH','AR','SE','BE','NO','IL','HK','IE','DK','CO','FI','GT','MO','JO','PY','TM','CI','IS','HN','PG','BW','NI','MU','BF','BS','CG','HT','MW','GU','AD','AW','BT','CF','BZ','LC','SM','MP','AS','PW','MH','TV']

abb = abb_br + abb_non_br

urls = []

for i in abb:
    new_url = url + i
    urls.append(new_url)

# r = requests.get(urls[0])
# print(r.content)


for i in range(0,len(urls)):
    html = requests.get(urls[i]).text
    
    html_file = 'd:/work/github/imdb_br/box_office_mojo/html/%s.html' % abb_br[i]
    with open (html_file,'w+',encoding = 'utf8') as f :
        f.write(html)

