module prelude

type Result<a, e> enum {
  OK(a)
  Err(e)
}

type World interface {
  fn open(path String) Result<File, IOError>
}



pub fn print(world World, text T) where T: Streamer<Char> {
  with(world.open("/dev/stdin"), fn(stdin) {
    text.iter().each(stdin.write_char)
  })
}
