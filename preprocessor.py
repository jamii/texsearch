#!/bin/env python

import string, re
from plasTeX import TeXFragment, TeXDocument
from plasTeX.DOM import Node

class BadRender(Exception):
  pass

class JsonRenderer:
  def __init__(self):
    self.text = [[]]
    self.macros = []

  def dumps(self):
    if len(self.text) != 1:
      raise BadRender()
    # Dont convert to string - the response goes out through json.dumps anyway
    return self.text[0]

  def addText(self,text):
    self.text[-1].append(text)

  def pushMacro(self,macro):
    self.text.append([])
    self.macros.append(macro)

  def popMacro(self,macro):
    try:
      currentMacro = self.macros.pop()
      if currentMacro != macro:
        raise BadRender()
      currentText = self.text.pop()
      self.text[-1].append({currentMacro : currentText})
    except Exception:
      raise BadRender()

  def openBracket(self):
    pass

  def closeBracket(self):
    pass

class PlainRenderer:
  def __init__(self):
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

# Ignore useless tags
ignoreSet = frozenset([
 'displaymath'
,'bgroup'
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
,'left'
,'right'
,'rm'
,'par'
,'None'
,'mathord'
,'array'
])

def render(node,renderer):
  if node.nodeType == Node.TEXT_NODE:
    # Short circuit text nodes
    text = unicode(node)
    if (re.sub("\s+","",text) != "") & (not (text in ignoreSet)):
      renderer.addText(text)
  elif node.nodeName in ignoreSet:
    # Ignore node and move on to children
    for child in node.childNodes:
      render(child,renderer)
  else:
    renderer.pushMacro(node.nodeName)
    renderChildren(node,renderer)
    renderer.popMacro(node.nodeName)

def renderChildren(node,renderer):
    # See if we have any attributes to render
    if node.hasAttributes():
      for key, value in node.attributes.items():
        # If the key is 'self' these nodes are the same as the child nodes
        # If the key is '*modifier*' we dont care about it
        if key == 'self' or key == '*modifier*':
          continue
        elif value.__class__ is TeXFragment:
          renderer.openBracket()
          for child in value.childNodes:
            render(child,renderer)
          renderer.closeBracket()
        elif value.__class__ is Node:
          renderer.openBracket()
          render(value,renderer)
          renderer.closeBracket()
        else:
          continue 

    # Render child nodes
    if node.childNodes:
      renderer.openBracket()
      for child in node.childNodes:
        render(child,renderer)
      renderer.closeBracket()

from plasTeX.TeX import TeX

def preprocess(string):
  # Instantiate a TeX processor and parse the input text
  tex = TeX()
  tex.disableLogging()
  tex.input(string)
  return tex.parse()

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
          timeout = int(query['timeout'])
        except ValueError, e:
          timeout = 1
        except KeyError, e:
          timeout = 1
        signal.alarm(timeout)    
   
        result = preprocess("\\begin{document} $$ " + query['latex'] + " $$ \\end{document}")

        if format == 'json-plain':
          jsonRenderer = JsonRenderer()
          render(result,jsonRenderer)
          plainRenderer = PlainRenderer()
          render(result,plainRenderer)
          response = {'code':200, 'json':{'json':jsonRenderer.dumps(), 'plain':plainRenderer.dumps()}}
        elif format == 'json':
          renderer = JsonRenderer()
          render(result,renderer)
          response = {'code':200, 'json':renderer.dumps()}
        elif format == 'xml':
          renderer = XmlRenderer()
          render(result,renderer)
          response = {'code':200, 'body':renderer.dumps(), 'headers':{'Content-type':'text/xml'}}
        elif format == 'plain':
          renderer = PlainRenderer()
          render(result,renderer)
          response = {'code':200, 'body':renderer.dumps(), 'headers':{'Content-type':'text/plain'}}
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
    

if __name__ == "__main__":
    main()
