#!/bin/env python
import sys, httplib
from xml.dom import minidom
from preprocessor import preprocess

try:
  # Python 2.6
  import json
except:
  # Prior to 2.6 requires simplejson
  import simplejson as json

# Bulk process a xml document
def addXml(fileName):
  xml = minidom.parse(fileName)
  docs = []
  for item in xml.childNodes[0].childNodes:
    if item.nodeName == u'result':
      doi = item.childNodes[0].childNodes[0].wholeText
      for node in item.childNodes[1:]:
        source = node.childNodes[0].wholeText
        content = preprocess("\\begin{document}"+source+"\\end{document}")
        doc = {'doi': doi, 'source': source, 'content': content}
        docs.append(doc)

  conn = httplib.HTTPConnection("localhost:5984")
  headers = {"Content-type": "application/json"}
  conn.request("POST", "/documents/_bulk_docs", json.dumps({'all-or-nothing':True, 'docs':docs}), headers)
  response = conn.getresponse()
  if response.status != 201:
    # What status codes are acceptable here?
    raise IOError
  conn.close()

if __name__ == '__main__':
    addXml("exs.xml")