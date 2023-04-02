from _ast import AST
from ast import parse
import codecs
import ast
import json
from pprint import pprint

### copied from https://github.com/YoloSwagTeam/ast2json 

BUILTIN_PURE = (int, float, bool)
BUILTIN_BYTES = (bytearray, bytes)
BUILTIN_STR = (str)

def decode_str(value):
    return value

def decode_bytes(value):
    try:
        return value.decode('utf-8')
    except:
        return codecs.getencoder('hex_codec')(value)[0].decode('utf-8')

def ast2json(node):
    assert isinstance(node, AST)
    to_return = dict()
    to_return['_type'] = node.__class__.__name__
    for attr in dir(node):
        if attr.startswith("_"):
            continue
        to_return[attr] = get_value(getattr(node, attr))
    return to_return

def str2json(string):
    return ast2json(parse(string))

def get_value(attr_value):
    if attr_value is None:
        return attr_value
    if isinstance(attr_value, BUILTIN_PURE):
        return attr_value
    if isinstance(attr_value, BUILTIN_BYTES):
        return decode_bytes(attr_value)
    if isinstance(attr_value, BUILTIN_STR):
        return decode_str(attr_value)
    if isinstance(attr_value, complex):
        return str(attr_value)
    if isinstance(attr_value, list):
        return [get_value(x) for x in attr_value]
    if isinstance(attr_value, AST):
        return ast2json(attr_value)
    if isinstance(attr_value, type(Ellipsis)):
        return '!!!..._encoding_of_ellipsis_as_string_...!!!'
    else:
        raise Exception("unknown case for '%s' of type '%s'" % (attr_value, type(attr_value)))

import sys

def main():
    filename = sys.argv[1]
    with open(filename, "r") as source:
        tree = ast.parse(source.read())
    
    j = ast2json(tree)
    print (json.dumps(j, indent=4))
    #print(ast.dump(tree))
    #print(ast.unparse(tree))


if __name__ == "__main__":
    main()