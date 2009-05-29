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
          for child in value.childNodes:
            clean(child,children)
        else:
          children.append(unicode(value))

    # Invoke cleaning on child nodes
    for child in node.childNodes:
      clean(child,children)

    output.append({node.nodeName : children})

from plasTeX.TeX import TeX

class PreprocessorError(Exception):
  pass

def preprocess(string):
  # Instantiate a TeX processor and parse the input text
  try:
    tex = TeX()
    tex.disableLogging()
    tex.input(string)
    result = []
    clean(tex.parse(),result)
    return result
  except Exception:
    raise PreprocessorError()