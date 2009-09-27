#!/usr/bin/env python
import re
import sys, httplib, urllib
from xml.dom import minidom
import preprocessor
from util import encodeDoi
import couchdb.client

# Find the couchdb server
conf = open("./db.ini")
port = re.compile(r"port *= *(\d+)").search(conf.read()).group(1)
conf.close()

print 'couchdb is at http://localhost:%s/' % port

couchdb = couchdb.client.Server('http://localhost:%s/' % port)
db = couchdb['documents']

def preprocess(latex):
  preprocessed = preprocessor.preprocess("\\begin{document} " + latex + " \\end{document}")
  renderer = preprocessor.JsonRenderer()
  preprocessor.render(preprocessed,renderer)
  return renderer.dumps()

def confirm(prompt):
  response = raw_input(prompt + " (y/n):")
  if response != 'y':
    print "Ok, nothing was done"
    sys.exit(0)

# Initial configuration of the database

def initDB():
  confirm("This will erase the texsearch database. Are you sure?")

  print "Deleting existing databases"
  del couchdb['documents']

  print "Creating new databases"
  couchdb.create['documents']

# Parsing and preprocessing xml articles

def parseEquation(eqn):
  eqnID = eqn.attributes.get('ID').value
  try:
    eqnSource = eqn.getElementsByTagName("EquationSource")[0]
    if eqnSource.attributes.get('Format').value == "TEX":
      latex = eqnSource.childNodes[0].wholeText
      equations[latex] = eqnID
  except IndexError:
    print ("Note: no equation source for eqn %s" % eqnID)
    eqnSource = None
  except AttributeError:
    print ("Note: missing format attribute for eqn %s" % eqnID)
    eqnSource = None
  return (eqnSource, eqnID)

def parseArticle(article):
  doi = article.getElementsByTagName("ArticleDOI")[0].childNodes[0].wholeText
  print ("Parsing article %s" % doi)
    
  equations = [parseEquation(eqn) for eqn in article.getElementsByTagName("Equation") + article.getElementsByTagName("InlineEquation")]
  # Eliminate duplicate equations (key is eqnSource)
  equations = dict(equations).items()

  source = {}
  content = {}
  for latex, eqnID in equations.items():
    try:
      source[eqnID] = latex 
      content[eqnID] = preprocess(latex)
    except KeyboardInterrupt, e:
      raise e
    except Exception, e:
      print "Note: Preprocessor failed on equation %s : %s" % (eqnID, e)

  return {'_id': encodeDoi(doi), 'source': source, 'content': content}

def parseFile(fileName):
  xml = minidom.parse(fileName)

  journalID = xml.getElementsByTagName("JournalID")[0].childNodes[0].wholeText

  publicationDate = xml.getElementsByTagName("PrintDate") or xml.getElementsByTagName("CoverDate") or xml.getElementsByTagName("OnlineDate")
  if publicationDate:
    publicationYear = publicationDate[0].getElementsByTagName("Year")[0].childNodes[0].wholeText
  else:
    print "Note: no publication year"
    publicationYear = None

  docs = [parseArticle(article) for article in xml.getElementsByTagName("Article")]

  for doc in docs:
    doc['journalID'] = journalID
    doc['publicationYear'] = publicationYear

  return docs

# Adding and deleting articles from the database

def addFile(fileName, type):
  print "Reading file %s" % fileName
  docs = parseFile(fileName)

  for doc in docs:
    doc['type'] = type

    oldDoc = db[doc._id]
    if not oldDoc:
      print "Adding new entry"
      db[doc._id] = doc
    elif (doc['type'] == 'xml.meta') and (oldDoc['type'] == 'xml'):
      print "Full entry already exists, not overwriting with meta"
    else:
      print "Overwriting existing entry"
      db[doc._id] = doc

def delFile(fileName, type):
  print "Reading file %s" % fileName
  xml = minidom.parse(fileName)

  for article in xml.getElementsByTagName("Article"):
    doi = article.getElementsByTagName("ArticleDOI")[0].childNodes[0].wholeText

    oldDoc = db[doi]
    if not oldDoc:
      print "No entry to delete"
    elif (type == 'xml.meta') and (oldDoc['type'] == 'xml'):
      print "Full entry exists, not deleting meta"
    else:
      print "Deleting entry"
      del db[doc._id]

# Reprocess all latex sources in the database, handy when changing the preprocessor
def reprocess():
  confirm("Ensure that no other processes are currently modifying the database. Continue?")
  print "Reprocessing latex sources"    
  for doi in db:
    print "Reprocessing %s" % doi
    doc = db[doi]
    doc['content'] = [eqnID, preprocess(eqnSource) for eqnID, eqnSource in doc['source']]
    db[doi] = doc

# Command line interaction

def walk(path):
  for root, _, files in os.walk(arg):
    for file in files:
      yield os.path.join(root,file)

def usage():
  print "Usage: --init, --reprocess, --add=/docs/addme, --del=/docs/delme"

import os, os.path, getopt

if __name__ == '__main__':
  try:
    opts, args = getopt.getopt(sys.argv[1:], "", ["init", "reprocess", "add=", "del="])
    errors = []

    for opt, arg in opts:
      if opt == "--init":
        initDB()
      elif opt == "--reprocess":
        reprocess()
      elif opt == "--add":
        for file in walk(arg):
          try:
            if file.lower().endswith(".xml"):
              addFile(os.path.join(root,fi),"xml")
            elif file.lower().endswith(".xml.meta"):
              addFile(os.path.join(root,fi),"xml.meta")
          except KeyboardInterrupt, e:
            raise e
          except Exception, exc:
            print exc
            errors.append((os.path.join(root,fi),exc))
      elif opt == "--del":
          try:            
            if file.lower().endswith(".xml"):
              delFile(os.path.join(root,fi),"xml")
            elif file.lower().endswith(".xml.meta"):
              delFile(os.path.join(root,fi),"xml.meta")
          except KeyboardInterrupt, e:
            raise e
          except Exception, exc:
            print exc
            errors.append((os.path.join(root,fi),exc))
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
