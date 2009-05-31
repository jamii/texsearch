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
,'rm'
,'par'])

def cleanup(node):
  # Short circuit text nodes
  if node.nodeType == Node.TEXT_NODE:
    return [unicode(node)]
  elif node.nodeName in ignoreSet:
    # Ignore node and move on to children
    return cleanupChildren(node)
  else:
    return [{ node.nodeName : cleanupChildren(node) }]

def cleanupChildren(node):
    """ Rendering method for all non-text nodes """
    children = []

    # See if we have any attributes to cleanup
    if node.hasAttributes():
      for key, value in node.attributes.items():
        # If the key is 'self' these nodes are the same as the child nodes
        # If the key is '*modifier*' we dont care about it
        if key == 'self' or key == '*modifier*':
          continue
        if value.__class__ is TeXFragment:
          for child in value.childNodes:
            children.extend(cleanup(child))
        else:
          children.append(unicode(value))

    # Dump child nodes
    for child in node.childNodes:
      children.extend(cleanup(child))

    return children

from plasTeX.TeX import TeX

def preprocess(string):
  # Instantiate a TeX processor and parse the input text
  tex = TeX()
  tex.disableLogging()
  tex.input(string)
  return cleanup(tex.parse())

import sys
import simplejson as json

def requests():
  line = sys.stdin.readline()
  while line:
    yield json.loads(line)
    line = sys.stdin.readline()

def main():
  for request in requests():
    try:
      query = request['query']
      response = preprocess("\\begin{document}$$"+query['latex']+"$$\\end{document}")
      code = 200 # OK
    except KeyError, e:
      response = 'Error: ' + str(e)
      code = 400 # Bad request
    except Exception, e:
      response = 'Error: ' + str(e)
      code = 500 # Internal server error
    sys.stdout.write("%s\n" % json.dumps({'code':code, 'json':response}))
    sys.stdout.flush()

if __name__ == "__main__":
    main()