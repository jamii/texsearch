#!/bin/env python

""" Parses and preprocesses LaTeX formulae using PlasTeX """ 

import string, re
from plasTeX import TeXFragment, TeXDocument
import plasTeX.Context
from plasTeX.DOM import Node
from plasTeX.TeX import TeX
from plasTeX.Base.TeX.Primitives import MathShift

### LaTeX preprocessing ###

# Ignore useless nodes
# There are probably more nodes that could be ignored but these are the most common
ignoreSet = frozenset([
 'displaymath'
,'bgroup'
,'egroup'
,'math'
,'text'
,'nulldelimiterspace'
,'kern'
,'vphantom'
,'hphantom'
,'hfill'
,'vfill'
,'hbox'
,'align'
,'aligned'
,'gathered'
,'active::&'
,'#document'
,'document'
,'rm'
,'par'
,'None'
,'mathord'
,'array'
])

class BadProcess(Exception):
  pass

class Processor:
  def __init__(self):
    self.textNode = False
  
  def process(self,node):
    if node.nodeName.startswith('text'):
      self.textNode = True
    if node.nodeType == Node.TEXT_NODE:
      # Short circuit text nodes
      text = unicode(node)
      # Unfortunately plasTeX does not place \text node arguments under text nodes
      if self.textNode:
        self.addText(text)
        self.textNode = False
      else:
        for char in text:
          if char != ' ':
            self.addText(char)
    elif node.nodeName in ignoreSet:
      # Ignore node and move on to children
      for child in node.childNodes:
        self.process(child)
    else:
      self.pushMacro(unicode(node.nodeName))
      self.processChildren(node)
      self.popMacro(unicode(node.nodeName))

    return self

  def processChildren(self,node):
    # See if we have any attributes to process
    if node.hasAttributes():
      for key, value in node.attributes.items():
        # If the key is 'self' these nodes are the same as the child nodes
        # If the key is '*modifier*' we dont care about it
        if key == 'self' or key == '*modifier*':
          continue
        elif value.__class__ is TeXFragment:
          self.openBracket()
          for child in value.childNodes:
            self.process(child)
          self.closeBracket()
        elif value.__class__ is Node:
          self.openBracket()
          self.process(value)
          self.closeBracket()
        else:
          continue 

    # Process child nodes
    if node.childNodes:
      self.openBracket()
      for child in node.childNodes:
        self.process(child)
      self.closeBracket()
  
    return self

# Converts a plasTeX DOM tree into a json tree #
class JsonProcessor(Processor):
  def __init__(self):
    self.textNode = False
    self.text = [[]]
    self.macros = []

  def dumps(self):
    if len(self.text) != 1:
      raise BadProcess()
    return self.text[0]

  def addText(self,text):
    self.text[-1].append(text)

  def pushMacro(self,macro):
    self.text.append([])
    self.macros.append(macro)

  def popMacro(self,macro):
    currentMacro = self.macros.pop()
    if currentMacro != macro:
      raise BadProcess()
    currentText = self.text.pop()
    self.text[-1].append({currentMacro : currentText})

  def openBracket(self):
    pass

  def closeBracket(self):
    pass

# Converts a plasTeX DOM tree back into plain LaTeX
class PlainProcessor(Processor):
  def __init__(self):
    self.textNode = False
    self.text = []
    self.macros = 0

  def dumps(self):
    return " ".join(self.text)

  def addText(self,text):
    self.text.append(text)

  def pushMacro(self,macro):
    self.macros += 1
    if macro.startswith("active::"):
      self.text.append(macro.lstrip("active::"))
    else:
      self.text.append("\\" + macro)

  def popMacro(self,macro):
    self.macros -= 1

  def openBracket(self):
    self.text.append("{")

  def closeBracket(self):
    self.text.append("}")

# Override plasTeX's buggy handling of mathmode, since we dont need textmode
plasTeX.Context.Context.isMathMode = property(lambda obj: True)

def parseLaTeX(string):
    # PlasTeX bug - this variable doent get reinitialised
    MathShift.inEnv = []

    # Instantiate a TeX processor and parse the input text
    tex = TeX()
    tex.disableLogging()

    # Parse the LaTeX
    tex.input(string)
    return tex.parse()

### Making the preprocessor available as a couchdb _external  ###

import sys
import simplejson as json

def requests():
  line = sys.stdin.readline()
  while line:
    yield json.loads(line)
    line = sys.stdin.readline()

import signal

class Timeout(Exception):
  def __str__(self):
    return "Timed out"

def handleTimeout(signum,frame):
  raise Timeout()

def main():
  # Work around the lack of real threading by using an alarm signal for timeouts
  signal.signal(signal.SIGALRM, handleTimeout)

  for request in requests():
    try:
      try: # Nested try because older versions of python cant handle except/finally
        query = request['query']

        format = query['format']

        try:
          timeout = int(float(query['timeout']))
        except ValueError, e:
          timeout = 5
        except KeyError, e:
          timeout = 5
        signal.alarm(timeout)    
   
        dom = parseLaTeX("\\begin{document} $$" + query['latex'] + "$$ \\end{document}")

        if format == 'json-plain':
          jsonResponse = JsonProcessor().process(dom).dumps()
          plainResponse = PlainProcessor().process(dom).dumps()
          response = {'code':200, 'json':{'json':jsonResponse, 'plain':plainResponse}}
        elif format == 'json':
          jsonResponse = JsonProcessor().process(dom).dumps()
          response = {'code':200, 'json':jsonResponse}
        elif format == 'plain':
          plainResponse = PlainProcessor().process(dom).dumps()
          response = {'code':200, 'body':plainResponse, 'headers':{'Content-type':'text/plain'}}
        else:
          response = {'code':400, 'body':('Error: bad format argument'), 'headers':{'Content-type':'text/plain'}} # Bad request

      except KeyError, e:
        response = {'code':400, 'body':('Error: ' + str(e)), 'headers':{'Content-type':'text/plain'}} # Bad request
      except Timeout, e:
        response = {'code':500, 'body':('Error: ' + str(e)), 'headers':{'Content-type':'text/plain'}} # Internal server error
      except Exception, e:
        response = {'code':500, 'body':('Error: ' + str(e)), 'headers':{'Content-type':'text/plain'}} # Internal server error
    finally:
      # Deactivate the timeout
      signal.alarm(0)

    sys.stdout.write("%s\n" % json.dumps(response))
    sys.stdout.flush()

def dumps(latex):
  return JsonProcessor().process(parseLaTeX("\\begin{document} $$" + latex + "$$ \\end{document}")).dumps()

if __name__ == "__main__":
    main()
