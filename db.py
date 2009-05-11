#!/bin/env python
import sys, httplib
from xml.dom import minidom
from preprocessor import preprocess
import simplejson as json
from util import expectResponse

def initDB():
  try :
    # Print warning

    conn = httplib.HTTPConnection("localhost:5984")
    headers = {"Content-type": "application/json"}

    # Delete existing databases
    conn.request("DELETE", "/documents")
    expectResponse(conn,200)
    conn.request("DELETE", "/store")
    expectResponse(conn,200)

    # Create new databases
    conn.request("PUT", "/documents")
    expectResponse(conn,201)
    conn.request("PUT", "/store")
    expectResponse(conn,201)

    # Setup views
    try:
      design = open('design.json','r')
      conn.request("PUT", "/documents/_design/search", design.read())
      expectResponse(conn,201)
    except IOError:
      print "Could not find the design document (design.json)"
      sys.exit(1)
    finally:
      design.close()

  except Exception, e:
    print "Error contacting database:"
    raise e
    sys.exit(1)
  finally:
    conn.close()

def postDocs(docs):
  try:
    conn = httplib.HTTPConnection("localhost:5984")
    headers = {"Content-type": "application/json"}
    conn.request("POST", "/documents/_bulk_docs", json.dumps({'all-or-nothing':True, 'docs':docs}), headers)
    expectResponse(conn,201)
  except Exception, e:
    print "Error contacting database:"
    raise e
  finally:
    conn.close()

def by_doi(dois):
  try:
    conn = httplib.HTTPConnection("localhost:5984")
    headers = {"Content-type": "application/json"}
    conn.request("POST", "/documents/_design/search/_view/by_doi", json.dumps({'keys':dois}), headers)
    result = json.loads(expectResponse(conn,200))
    ids = []
    for row in result['rows']:
      ids.append(row['value'])
    return ids
  except Exception, e:
    print "Error contacting database:"
    raise e
  finally:
    conn.close()

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
  print "Usage: --init, --add docs.xml, --del docs.xml"

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
    print "OK!"
  except getopt.GetoptError:
    usage()
    sys.exit(2)