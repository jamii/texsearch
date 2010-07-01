#!/usr/bin/env python
import re
import sys, httplib, urllib
from xml.dom import minidom
from preprocessor import JsonProcessor, parseLaTeX
from util import encodeDoi, decodeDoi
import couchdb.client

# Find the couchdb server
conf = open("./db.ini")
port = re.compile(r"port *= *(\d+)").search(conf.read()).group(1)
conf.close()

couchdb_server = couchdb.client.Server('http://localhost:%s/' % port)

def confirm(prompt):
  response = raw_input(prompt + " (y/n):")
  if response != 'y':
    print "Ok, nothing was done"
    sys.exit(0)

### Initial configuration of the database ###

def initDB():
  confirm("This will erase the texsearch database. Are you sure?")

  print "Deleting existing databases"
  try:
    del couchdb_server['documents']
  except couchdb.client.ResourceNotFound:
    # db doesnt exist yet
    pass

  print "Creating new databases"
  couchdb_server.create('documents')

### Parsing and preprocessing xml articles ###

# Wrap the JsonProcessor in some error handling, since plasTeX often fails in weird ways
def preprocess(eqnID, latex):
  try:  
    result = JsonProcessor().process(parseLaTeX("\\begin{document} " + latex + " \\end{document}")).dumps()
    return (eqnID, result)
  except KeyboardInterrupt, e:
    raise e
  except Exception, e:
   print "Note: Preprocessor failed on equation %s : %s" % (eqnID, e)
   return None

def parseEquation(eqn):
  eqnID = eqn.attributes.get('ID').value
  try:
    for eqnSource in eqn.getElementsByTagName("EquationSource"):
      if eqnSource.attributes.get('Format').value == "TEX":
        latex = eqnSource.childNodes[0].wholeText
        return (latex, eqnID)
    return None
  except IndexError:
    print ("Note: no equation source for eqn %s" % eqnID)
  except AttributeError:
    print ("Note: missing format attribute for eqn %s" % eqnID)
    return None

def filterNone(xs):
  return [x for x in xs if x is not None]

def parseEquations(item):
  equations = filterNone([parseEquation(eqn) for eqn in item.getElementsByTagName("Equation") + item.getElementsByTagName("InlineEquation")])
  # Eliminate duplicate equations (key is latex)
  equations = dict(equations).items()

  source = dict([(eqnID, latex) for (latex, eqnID) in equations])
  content = dict(filterNone([preprocess(eqnID, latex) for (latex, eqnID) in equations]))

  return (source, content)
  
def parseArticle(article):
  doi = article.getElementsByTagName("ArticleDOI")[0].childNodes[0].wholeText
  print ("Parsing article %s" % doi)
  journalID = xml.getElementsByTagName("JournalID")[0].childNodes[0].wholeText
  (source, content) = parseEquations(article)
  return {'_id': encodeDoi(doi), 'source': source, 'content': content, 'format': 'Article', 'containerID': journalID}

def parseChapter(chapter):
  doi = chapter.getElementsByTagName("ChapterDOI")[0].childNodes[0].wholeText
  print ("Parsing chapter %s" % doi)
  (source, content) = parseEquations(chapter)
  return {'_id': encodeDoi(doi), 'source': source, 'content': content, 'format':'Chapter'}

def parseBook(book):
  bookDOI = book.getElementsByTagName("BookDOI")[0].childNodes[0].wholeText
  chapters = []
  for chapter in book.getElementsByTagName("Chapter"):
    chapter = parseChapter(chapter)
    chapter['containerID'] = bookDOI
    chapters.append(chapter)
  return chapters

def parseFile(fileName):
  xml = minidom.parse(fileName)

  publicationDate = xml.getElementsByTagName("PrintDate") or xml.getElementsByTagName("CoverDate") or xml.getElementsByTagName("OnlineDate")
  if publicationDate:
    publicationYear = publicationDate[0].getElementsByTagName("Year")[0].childNodes[0].wholeText
  else:
    print "Note: no publication year"
    publicationYear = None

  articles = [parseArticle(article) for article in xml.getElementsByTagName("Article")]
  chapters = []
  for book in xml.getElementsByTagName("Book"):
    chapters.extend(parseBook(book))
  docs = articles + chapters

  for doc in docs:
    doc['publicationYear'] = publicationYear

  return docs

### Adding and deleting articles from the database ###

def addFile(fileName, type):
  db = couchdb_server['documents']

  print "Reading file %s" % fileName
  docs = parseFile(fileName)

  for doc in docs:
    doc['type'] = type

    oldDoc = db.get(doc['_id'],None)
    if not oldDoc:
      print "Adding new entry"
      db[doc['_id']] = doc
    elif (doc['type'] == 'xml.meta') and (oldDoc['type'] == 'xml'):
      print "Full entry already exists, not overwriting with meta"
    else:
      print "Overwriting existing entry"
      doc['_rev'] = oldDoc['_rev']
      db[doc['_id']] = doc

def delFile(fileName, type):
  db = couchdb_server['documents']

  print "Reading file %s" % fileName
  xml = minidom.parse(fileName)

  for article in xml.getElementsByTagName("Article"):
    doi = encodeDoi(article.getElementsByTagName("ArticleDOI")[0].childNodes[0].wholeText)

    oldDoc = db.get(doi,None)
    if not oldDoc:
      print "No entry to delete"
    elif (type == 'xml.meta') and (oldDoc['type'] == 'xml'):
      print "Full entry exists, not deleting meta"
    else:
      print "Deleting entry"
      del db[doi]

# Reprocess all latex sources in the database, handy when changing the preprocessor
def reprocess():
  db = couchdb_server['documents']

  print "Reprocessing latex sources"    
  for doi in db:
    print "Reprocessing %s" % decodeDoi(doi)
    doc = db[doi]
    doc['content'] = dict(filterNone([(preprocess(eqnID, latex)) for (eqnID, latex) in doc['source'].items()]))
    db[doi] = doc

def convert_journalID_containerID():
  db = couchdb_server['documents']

  print "Converting"    
  for doi in db:
    print "Converting %s" % decodeDoi(doi)
    doc = db[doi]
    if 'journalID' in doc:
      doc['containerID'] = doc['journalID']
      del doc['journalID']
    db[doi] = doc

# Repair this server by copying content from targetServer
def repair(targetServer):
  db = couchdb_server['documents']
  targetdb = couchdb.client.Server(targetServer)['documents']

  print "Copying from %s" % target_server  

  for doi in db:
    targetDoc = targetdb.get(doi,None)
    if targetDoc:
      db[doi] = targetDoc 

### Command line interaction ###

def walk(path):
  for root, _, files in os.walk(arg):
    for file in files:
      yield os.path.join(root,file)

def usage():
  print "Usage: --init, --reprocess, --add=/docs/addme, --del=/docs/delme"

import os, os.path, getopt

if __name__ == '__main__':
  try:
    opts, args = getopt.getopt(sys.argv[1:], "", ["init", "reprocess", "add=", "del=", "convert"])
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
              addFile(file,"xml")
            elif file.lower().endswith(".xml.meta"):
              addFile(file,"xml.meta")
          except KeyboardInterrupt, e:
            raise e
          except Exception, exc:
           print exc
           errors.append((file,exc))
      elif opt == "--del":
        for file in walk(arg):
          try:            
            if file.lower().endswith(".xml"):
              delFile(file,"xml")
            elif file.lower().endswith(".xml.meta"):
              delFile(file,"xml.meta")
          except KeyboardInterrupt, e:
            raise e
          except Exception, exc:
            print exc
            errors.append((file,exc))
      elif opt == "--convert":
        convert_journalID_containerID()
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
