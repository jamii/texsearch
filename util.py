def encodeDoi(doi):
  return doi.replace("/","_",1)

def decodeDoi(doi):
  return doi.replace("_","/",1)
