type LinkedList enum:
  End
  Link:
    value String
    next LinkedList

fn main():
  let a = End{}
  match a:
    End():
        pass
    Link(x, Link(x, _)):
        pass
