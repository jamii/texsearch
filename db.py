#!/usr/bin/env python
import sys, httplib, urllib
from xml.dom import minidom
from preprocessor import preprocess, render, JsonRenderer
import simplejson as json
from util import expectResponse, encodeDoi

couchdb = "localhost:5984"

def initDB():
  # Warn user
  response = raw_input("This will erase the texsearch database. Are you sure? (y/n):")
  if response != 'y':
    print "Ok, nothing was done"
    sys.exit(0)

  conn = httplib.HTTPConnection(couchdb)

  print "Deleting existing databases"
  conn.request("DELETE", "/documents")
  conn.getresponse().read()

  print "Creating new databases"
  conn.request("PUT", "/documents")
  expectResponse(conn,201)

  conn.close()

def addDocs(docs):
  conn = httplib.HTTPConnection(couchdb)

  for doc in docs:
    conn.request("GET", "/documents/%s" % doc['_id'])

    response = conn.getresponse()

    if response.status == 200: 
      # Entry already exists
      oldDoc = json.loads(response.read())
      if (doc['type'] == 'xml.meta') and (oldDoc['type'] == 'xml'):
        # Full document has stricly more information than the meta 
        print "Full entry already exists, not overwriting with meta"
      else:
        # Safe to overwrite
        print "Overwriting existing entry"
        doc['_rev'] = oldDoc['_rev']
        conn.request("PUT", "/documents/%s" % doc['_id'], json.dumps(doc))
        expectResponse(conn,201)
    elif response.status == 404:
      # No existing entry
      print "Adding new entry"
      response.read() # Clear the response
      conn.request("PUT", "/documents/%s" % doc['_id'], json.dumps(doc))
      expectResponse(conn,201)
    else:
      raise UnexpectedResponse(200,response.status)

  conn.close()

def delDocs(docs):
  conn = httplib.HTTPConnection(couchdb)

  for doc in docs:
    conn.request("GET", "/documents/%s" % doc['_id'])

    revision = json.loads(expectResponse(conn,200))['_rev']
    conn.request("DELETE", "/documents/%s?rev=%s" % (doc['_id'], revision))

  conn.close()

# Bulk process a xml document
def addXml(fileName, type):
  conn = httplib.HTTPConnection(couchdb)

  print "Reading file %s" % fileName
  xml = minidom.parse(fileName)

  journalID = xml.getElementsByTagName("JournalID")[0].childNodes[0].wholeText

  publicationDate = xml.getElementsByTagName("PrintDate") or xml.getElementsByTagName("CoverDate") or xml.getElementsByTagName("OnlineDate")
  if publicationDate:
    publicationYear = publicationDate[0].getElementsByTagName("Year")[0].childNodes[0].wholeText
  else:
    print "Note: no publication year"
    publicationYear = None

  # Collect docs
  docs = []
  for article in xml.getElementsByTagName("Article"):
    doi = article.getElementsByTagName("ArticleDOI")[0].childNodes[0].wholeText
    print ("Parsing article %s" % doi)
    
    # Collect equation sources, ignoring duplicates
    equations = {}
    for eqn in article.getElementsByTagName("Equation") + article.getElementsByTagName("InlineEquation"):
      eqnID = eqn.attributes.get('ID').value
      try:
        eqnSource = eqn.getElementsByTagName("EquationSource")[0]
        if eqnSource.attributes.get('Format').value == "TEX":
          latex = eqnSource.childNodes[0].wholeText
          equations[latex] = eqnID
      except IndexError:
        print ("Note: no equation source for eqn %s" % eqnID)
      except AttributeError:
        print ("Note: missing format attribute for eqn %s" % eqnID)

    source = {}
    content = {}
    for latex, eqnID in equations.items():
      try:
        source[eqnID] = latex 

        preprocessed = preprocess("\\begin{document} " + latex + " \\end{document}")
        renderer = JsonRenderer()
        render(preprocessed,renderer)
        content[eqnID] = renderer.dumps()
      except KeyboardInterrupt, e:
        raise e
      except Exception, e:
        print "Note: Preprocessor failed on equation %s : %s" % (eqnID, e)

    doc = {'_id': encodeDoi(doi), 'journalID': journalID, 'publicationYear': publicationYear, 'type':type, 'source': source, 'content': content}
    docs.append(doc)

  # Add docs
  addDocs(docs)

def delXml(fileName):
  print "Reading file %s" % fileName
  xml = minidom.parse(fileName)

  # Collect dois
  docs = []
  for article in xml.getElementsByTagName("Article"):
    doi = article.getElementsByTagName("ArticleDOI")[0].childNodes[0].wholeText
    print ("Parsing %s" % doi)
    docs.append({'_id': encodeDoi(doi)})

  # Delete docs
  print "Deleting..."
  delDocs(docs)

def usage():
  print "Usage: --init, --add=docs.xml, --del=docs.xml"

import os, os.path, getopt

if __name__ == '__main__':
  try:
    opts, args = getopt.getopt(sys.argv[1:], "", ["add=", "del=", "init"])
    errors = []

    for opt, arg in opts:
      if opt == "--init":
        initDB()
      if opt == "--add":
        for root, _, files in os.walk(arg):
          for fi in files:
            fileType = ""
            if fi.lower().endswith(".xml"):
              fileType = "xml"
            elif fi.lower().endswith(".xml.meta"):
              fileType = "xml.meta"
            if fileType:
              try:
                addXml(os.path.join(root,fi),fileType)      
              except KeyboardInterrupt, e:
                raise e
              except Exception, exc:
                print exc
                errors.append((os.path.join(root,fi),exc))
      if opt == "--del":
        for root, _, files in os.walk(arg):
          for fi in files:
            if fi.endswith(".xml"):
              delXml(os.path.join(root,fi))
    if errors:
      print "Errors occurred whilst processing the following files:"
      for (fi,exc) in errors:
        print fi
        print exc
    else:
      print "Ok"

  except getopt.GetoptError:
    usage()
    sys.exit(2)
