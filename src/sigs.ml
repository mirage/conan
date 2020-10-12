type ('a, 's) io

type 's scheduler = {
  bind : 'a 'b. ('a, 's) io -> ('a -> ('b, 's) io) -> ('b, 's) io;
  return : 'a. 'a -> ('a, 's) io;
}

type where = SET | CUR | END

type ('fd, 'error, 's) syscall = {
  seek : 'fd -> int64 -> where -> ((unit, 'error) result, 's) io;
  read : 'fd -> int -> ((string, 'error) result, 's) io;
  line : 'fd -> ((string, 'error) result, 's) io;
  read_int8 : 'fd -> ((int, 'error) result, 's) io;
  read_int16_ne : 'fd -> ((int, 'error) result, 's) io;
  read_int32_ne : 'fd -> ((int32, 'error) result, 's) io;
  read_int64_ne : 'fd -> ((int64, 'error) result, 's) io;
}
