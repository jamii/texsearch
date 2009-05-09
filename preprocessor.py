#!/bin/env python

import string, re
from plasTeX import TeXFragment
from plasTeX.DOM import Node

# Ignore useless tags
ignoreSet = frozenset([
 'displaymath'
,'bgroup'
,'math'
,'text'
,'nulldelimiterspace'
,'vphantom'
,'hphantom'
,'hfill'
,'vfill'
,'hbox'
,'align'
,'aligned'
,'gathered'
,'active::&'
,'\\'
,'#document'
,'document'
,'left'
,'right'
,'langle'
,'rangle'])

def clean(node,output):
  # Short circuit text nodes
  if node.nodeType == Node.TEXT_NODE:
    text = re.sub("\s+", "", unicode(node))
    if text:
      output.append(text)
  elif node.nodeName in ignoreSet:
    # Ignore node and move on to children
    for child in node.childNodes:
      clean(child,output)
  else:
    """ Rendering method for all non-text nodes """
    children = []

    # See if we have any attributes to clean
    if node.hasAttributes():
      for key, value in node.attributes.items():
        # If the key is 'self' these nodes are the same as the child nodes
        # If the key is '*modifier*' we dont care about it
        if key == 'self' or key == '*modifier*':
          continue
        if value.__class__ is TeXFragment:
          clean(value,children)
        else:
          children.append(unicode(value))

    # Invoke cleaning on child nodes
    for child in node.childNodes:
      clean(child,children)

    output.append({node.nodeName : children})

from plasTeX.TeX import TeX

def preprocess(string):
  # Instantiate a TeX processor and parse the input text
  tex = TeX()
  tex.disableLogging()
  tex.input(string)
  result = []
  clean(tex.parse(),result)
  return result

import sys, httplib
from xml.dom import minidom
try:
  # Python 2.6
  import json
except:
  # Prior to 2.6 requires simplejson
  import simplejson as json

# Bulk process a xml document
def preprocessXml(fileName):
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
    preprocessXml("exs.xml")