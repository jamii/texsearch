#!/bin/env python
import sys, os, os.path, httplib, urllib
from xml.dom import minidom
import simplejson as json
from util import expectResponse
import random
from preprocessor import *

# Bulk process a xml document
def loadXml(fileName):
  # Collect docs
  docs = []

  for root, _, files in os.walk(arg):
    for fi in files:
      if fi.endswith(".xml"):
        xml = minidom.parse(os.path.join(root,fi))

        for article in xml.getElementsByTagName("Article"):
          doi = article.getElementsByTagName("ArticleDOI")[0].childNodes[0].wholeText

          source = []
          for eqn in article.getElementsByTagName("Equation") + article.getElementsByTagName("InlineEquation"):
            source.append(eqn.getElementsByTagName("EquationSource")[0].childNodes[0].wholeText)
          
          if source:
            doc = {'doi': doi, 'source': source}
            docs.append(doc)

  return docs

def testIndex(docs,n,server):
  successes = 0
  empties = 0
  failures = 0
  errors = 0

  for i in xrange(0,n):
    searchDoc = random.choice(docs)
    searchDoi = searchDoc['doi']
    searchTerm = random.choice(searchDoc['source'])
    try:
      conn = httplib.HTTPConnection(server)
      conn.request("GET", "/documents/_external/index?format=xml&searchTerm=\"%s\"" % urllib.quote(searchTerm))
      result = expectResponse(conn,200)
      xml = minidom.parseString(result)
      results = xml.childNodes[0]
      if len(results.childNodes) > 1:
        topResult = results.childNodes[1]
        doi = topResult.attributes['doi'].childNodes[0].wholeText
        if doi == searchDoi:
          print "Test %d: success (%d results)" % (i, len(results.childNodes))
          successes += 1
        else:
          failures += 1
          print "Test %d: wrong result" % i
          #print searchDoi, doi
          #print searchTerm
      else:
        empties += 1
        print "Test %d: no results" % i
        print searchDoi
        print searchTerm
        print result
    except Exception, e:
      errors += 1
      print "Test %d: %s" % (i,e)
      print searchDoi
      print searchTerm
      print result

  print "Successes %d, empties %d, failures %d, errors %d" % (successes,empties,failures,errors)

def callPreprocessor(source,server):
  conn = httplib.HTTPConnection(server)
  conn.request("GET", "/documents/_external/preprocess?format=json&latex=%s" % urllib.quote(source))
  return json.loads(expectResponse(conn,200))

def testPreprocessor(docs,n,server):
  successes = 0
  failures = 0
  errors = 0

  sources = []
  for doc in docs:
    sources.extend(doc['source'])
  for source in random.sample(sources,n):
    try:
      result1 = callPreprocessor(source,server)
      result2 = callPreprocessor(source,server)
      if (result1 == result2):
        successes += 1
        print "Test succeeded"
      else:
        failures += 1
        print "Test failed: inconsistent results"
        print source
        print result1
        print result2
    except Exception, e:
      errors += 1
      print "Test failed: error"
      print e

  print "Successes %d, failures %d, errors %d" % (successes,failures,errors)

def previewPreprocessor(docs,n):
  sources = []
  for doc in docs:
    sources.extend(doc['source'])
  for source in random.sample(sources,n):
    print source
    tex = preprocess(source)
    plainRenderer = PlainRenderer()
    render(tex,plainRenderer)
    print plainRenderer.dumps()
    print

import getopt

if __name__ == '__main__':
  opts, args = getopt.getopt(sys.argv[1:], "", ["file=","n=","server="])
  docs = []
  n = 100
  server = "localhost:5984"
  for opt, arg in opts:
    if opt == "--file":
      docs = loadXml(arg)
    if opt == "--n":
      n = int(arg)
    if opt == "--server":
      server = arg
  testIndex(docs,n,server)
  #testPreprocessor(docs,n,server)
  #previewPreprocessor(docs,n)
  print "Ok"
