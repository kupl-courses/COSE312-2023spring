import sys
import ast

def main():
  filename = sys.argv[1]
  with open(filename, "r") as source:
    tree = ast.parse(source.read())
    
  print(ast.unparse(tree))
  
if __name__ == "__main__":
    main()