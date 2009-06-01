#!/bin/env python
import sys, httplib, urllib
from xml.dom import minidom
from preprocessor import preprocess
import simplejson as json
from util import expectResponse, encodeDoi

def initDB():
  # Warn user
  response = raw_input("This will erase the texsearch database. Are you sure? (y/n):")
  if response != 'y':
    print "Ok, nothing was done"
    sys.exit(0)

  conn = httplib.HTTPConnection("localhost:5984")

  print "Deleting existing databases"
  conn.request("DELETE", "/documents")
  conn.getresponse().read()

  print "Creating new databases"
  conn.request("PUT", "/documents")
  expectResponse(conn,201)

  conn.close()

def postDocs(docs):
  conn = httplib.HTTPConnection("localhost:5984")
  headers = {"Content-type": "application/json"}
  conn.request("POST", "/documents/_bulk_docs", json.dumps({'all-or-nothing':True, 'docs':docs}), headers)
  expectResponse(conn,201)
  conn.close()

def stuffDocs(docs):
  for i in xrange(1,100):
    for doc in docs:
      doc['_id'] = doc['_id'][:-1] + str(i)
    postDocs(docs)

# Bulk process a xml document
def addXml(fileName):
  xml = minidom.parse(fileName)

  # Collect docs
  docs = []
  for item in xml.childNodes[0].childNodes:
    if item.nodeName == u'result':
      doi = item.childNodes[0].childNodes[0].wholeText
      print ("Parsing %s" % doi)
      source = {}
      content = {}
      for i in xrange(1,len(item.childNodes)):
        latex = item.childNodes[i].childNodes[0].wholeText
        source[str(i)] = latex
        content[str(i)] = preprocess(latex)
      doc = {'_id': encodeDoi(doi), 'source': source, 'content': content}
      docs.append(doc)

  # Add docs
  print "Adding..."
  postDocs(docs)

def delXml(fileName):
  xml = minidom.parse(fileName)

  # Collect dois
  docs = []
  for item in xml.childNodes[0].childNodes:
    if item.nodeName == u'result':
      doi = item.childNodes[0].childNodes[0].wholeText
      print ("Parsing %s" % doi)
      docs.append({'_id': encodeDoi(doi), '_deleted':True})

  # Delete docs
  print "Deleting..."
  postDocs(docs)

def usage():
  print "Usage: --init, --add=docs.xml, --del=docs.xml"

import getopt

if __name__ == '__main__':
  try:
    opts, args = getopt.getopt(sys.argv[1:], "", ["add=", "del=", "init"])
    for opt, arg in opts:
      if opt == "--init":
        initDB()
      if opt == "--add":
        addXml(arg)
      if opt == "--del":
        delXml(arg)
    print "Ok"
  except getopt.GetoptError:
    usage()
    sys.exit(2)