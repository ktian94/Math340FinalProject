import csv
import logging
import requests
from lxml import html
from zipfile import ZipFile
from StringIO import StringIO
import re

with open('dataSources.csv') as csvfile:
	reader = csv.DictReader(csvfile)
	dataSources = [{'url':row['url'],'targDir':row['targetDirectory'],'tableName':row['tableName']} for row in reader]

# modified from 
# http://docs.python-guide.org/en/latest/scenarios/scrape/
# http://stackoverflow.com/questions/9419162/python-download-returned-zip-file-from-url
for source in dataSources:
	page = requests.get(source['url'])
	tree = html.fromstring(page.text)
	a = tree.xpath('//li[@class="download-csv last"]/a')
	downloadLink = a[0].attrib['href']
	r = requests.get(downloadLink)
	z = ZipFile(StringIO(r.content))
	z.extractall(source['targDir'])
	pattern = re.compile('Meta')
	for fname in z.namelist():
		if not pattern.match(fname) and fname[-3:] == 'csv':
			with open('extract.R','a') as rScript:
				rScript.write(source['tableName'] + ' <- read.csv("' + source['targDir'] + '/' + fname + '", header=TRUE)\n')



	

