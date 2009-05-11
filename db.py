#!/bin/env python
import sys, httplib
from xml.dom import minidom
from preprocessor import preprocess
import simplejson as json

def postDocs(docs):
  conn = httplib.HTTPConnection("localhost:5984")
  headers = {"Content-type": "application/json"}
  conn.request("POST", "/documents/_bulk_docs", json.dumps({'all-or-nothing':True, 'docs':docs}), headers)
  response = conn.getresponse()
  if response.status != 201:
    # What status codes are acceptable here?
    raise IOError
  conn.close()

def by_doi(dois):
  conn = httplib.HTTPConnection("localhost:5984")
  headers = {"Content-type": "application/json"}
  conn.request("POST", "/documents/_design/demo/_view/by_doi", json.dumps({'keys':dois}), headers)
  response = conn.getresponse()
  if response.status != 200:
    # What status codes are acceptable here?
    raise IOError
  result = json.loads(response.read())
  ids = []
  for row in result['rows']:
    ids.append(row['value'])
  conn.close()
  return ids

# Bulk process a xml document
def addXml(fileName):
  xml = minidom.parse(fileName)

  # Collect docs
  docs = []
  for item in xml.childNodes[0].childNodes:
    if item.nodeName == u'result':
      doi = item.childNodes[0].childNodes[0].wholeText
      print ("Parsing %s" % doi)
      for node in item.childNodes[1:]:
        source = node.childNodes[0].wholeText
        content = preprocess("\\begin{document}"+source+"\\end{document}")
        doc = {'doi': doi, 'source': source, 'content': content}
        docs.append(doc)

  # Add docs
  print "Adding..."
  postDocs(docs)

def delXml(fileName):
  xml = minidom.parse(fileName)

  # Collect dois
  dois = []
  for item in xml.childNodes[0].childNodes:
    if item.nodeName == u'result':
      doi = item.childNodes[0].childNodes[0].wholeText
      print ("Parsing %s" % doi)
      dois.append(doi)

  # Retrieve docs by doi
  docs = by_doi(dois)

  # Delete docs
  print "Deleting..."
  for doc in docs:
    doc['_deleted'] = True
  postDocs(docs)

def usage():
  print "Usage: 'db --add=docs1.xml --add=docs2.xml --del=docs3.xml'"

import getopt

if __name__ == '__main__':
  try:
    opts, args = getopt.getopt(sys.argv[1:], "", ["add=", "del="])
    for opt, arg in opts:
      if opt == "--add":
        addXml(arg)
      if opt == "--del":
        delXml(arg)
    print "OK!"
  except getopt.GetoptError:
    usage()
    sys.exit(2)