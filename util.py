class UnexpectedResponse(Exception):
  def __init__(self,expected,got):
    self.expected = expected
    self.got = got

  def __str__(self):
    return ("Expected code %s, got code %s" % (self.expected,self.got))

def expectResponse(conn,code):
  response = conn.getresponse()
  if response.status != code:
    raise UnexpectedResponse(code,response.status)
  return response.read()