from bs4 import BeautifulSoup
import requests

test_html = r'D:/work/github/imdb_br/box_office_mojo/html_br/CN.html'


with open(test_html, "r") as f:
    page = f.read()

soup = BeautifulSoup(page, 'lxml')
