type ASTIsh enum:
  TInt
  TFloat
  TString
  TNamed:
    name String
  TFunc:
    arg ASTIsh
    ret ASTIsh

fn printEnum(a ASTIsh):
    print(String(a))

fn main():
  let a = TNamed{
    name: "some name",
  }
  print(String(a))
  print("\n")
  printEnum(a)
  print("\n")
