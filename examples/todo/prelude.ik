module prelude

type Result<a, e> enum:
  OK:
    val A
  Err:
    err E

type World interface:
  fn open(path String) Result<File, IOError>

pub fn print(world World, text t) where t: Streamer<Char>:
  with(world.open("/dev/stdin"), fn(stdin) =>
    text.iter().each(stdin.write_char)
  )
