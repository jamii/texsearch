#!/bin/env python
import sys, httplib, urllib
from xml.dom import minidom
from preprocessor import preprocess, render, JsonRenderer
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

def putDocs(docs):
  conn = httplib.HTTPConnection("localhost:5984")

  for doc in docs:
    conn.request("GET", "/documents/%s" % doc['_id'])

    response = conn.getresponse()
    if response.status == 200:
      doc['_rev'] = json.loads(response.read())['_rev']
    elif response.status == 404:
      response.read() # Clear the response
    else:
      raise UnexpectedResponse(200,response.status)

    conn.request("PUT", "/documents/%s" % doc['_id'], json.dumps(doc))
    expectResponse(conn,201)

  conn.close()

def delDocs(docs):
  conn = httplib.HTTPConnection("localhost:5984")

  for doc in docs:
    conn.request("GET", "/documents/%s" % doc['_id'])

    revision = json.loads(expectResponse(conn,200))['_rev']
    conn.request("DELETE", "/documents/%s?rev=%s" % (doc['_id'], revision))

  conn.close()

# Bulk process a xml document
def addXml(fileName):
  print "Reading file %s" % fileName
  xml = minidom.parse(fileName)

  # Collect docs
  docs = []
  for article in xml.getElementsByTagName("Article"):
    doi = article.getElementsByTagName("ArticleDOI")[0].childNodes[0].wholeText
    print ("Parsing article %s" % doi)
    source = {}
    content = {}

    for eqn in article.getElementsByTagName("Equation") + article.getElementsByTagName("InlineEquation"):
      eqnID = eqn.attributes.get('ID').value

      latex = eqn.getElementsByTagName("EquationSource")[0].childNodes[0].wholeText
      latex = latex.replace("\n","")
      source[eqnID] = latex

      try:
        renderer = JsonRenderer()
        render(preprocess("\\begin{document}" + latex + "\\end{document}"),renderer)
        content[eqnID] = renderer.dumps()
      except Exception:
        print "Could not parse equation %s" % eqnID

    doc = {'_id': encodeDoi(doi), 'source': source, 'content': content}
    docs.append(doc)

  # Add docs
  print "Adding..."
  putDocs(docs)

def delXml(fileName):
  print "Reading file %s" % fileName
  xml = minidom.parse(fileName)

  # Collect dois
  docs = []
  for article in xml.getElementsByTagName("Article"):
    doi = article.getElementsByTagName("ArticleDOI")[0].childNodes[0].wholeText
    print ("Parsing %s" % doi)
    docs.append({'_id': encodeDoi(doi), '_deleted':True})

  # Delete docs
  print "Deleting..."
  delDocs(docs)

def usage():
  print "Usage: --init, --add=docs.xml, --del=docs.xml"

import os, os.path, getopt

if __name__ == '__main__':
  try:
    opts, args = getopt.getopt(sys.argv[1:], "", ["add=", "del=", "init"])
    for opt, arg in opts:
      if opt == "--init":
        initDB()
      if opt == "--add":
        for root, _, files in os.walk(arg):
          for fi in files:
            if fi.endswith(".xml"):
              addXml(os.path.join(root,fi))
      if opt == "--del":
        for root, _, files in os.walk(arg):
          for fi in files:
            if fi.endswith(".xml"):
              delXml(os.path.join(root,fi))
    print "Ok"
  except getopt.GetoptError:
    usage()
    sys.exit(2)
