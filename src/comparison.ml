let invalid_arg fmt = Format.kasprintf invalid_arg fmt

type 'a t =
  | Equal of 'a
  | Different of 'a
  | Greater of 'a
  | Lower of 'a
  | And of 'a
  | Xor of 'a

let serialize pp ppf = function
  | Equal v -> Format.fprintf ppf "@[<2>Conan.Comparison.equal_to@ %a@]" pp v
  | Different v ->
      Format.fprintf ppf "@[<2>Conan.Comparison.different_to@ %a@]" pp v
  | Greater v ->
      Format.fprintf ppf "@[<2>Conan.Comparison.greater_than@ %a@]" pp v
  | Lower v -> Format.fprintf ppf "@[<2>Conan.Comparison.lower_than@ %a@]" pp v
  | And v -> Format.fprintf ppf "@[<2>Conan.Comparison.bitwise_and@ %a@]" pp v
  | Xor v -> Format.fprintf ppf "@[<2>Conan.Comparison.bitwise_xor@ %a@]" pp v

let equal_to v = Equal v
let different_to v = Different v
let greater_than v = Greater v
let lower_than v = Lower v
let bitwise_and v = And v
let bitwise_xor v = Xor v

let of_string ~with_val = function
  | "=" -> Equal with_val
  | "!" -> Different with_val
  | "<" -> Lower with_val
  | ">" -> Greater with_val
  | "&" -> And with_val
  | "^" -> Xor with_val
  | v -> invalid_arg "Invalid comparison operation: %S" v

let map ~f = function
  | Equal v -> Equal (f v)
  | Different v -> Different (f v)
  | Greater v -> Greater (f v)
  | Lower v -> Lower (f v)
  | And v -> And (f v)
  | Xor v -> Xor (f v)

let value = function
  | Equal v -> v
  | Different v -> v
  | Greater v -> v
  | Lower v -> v
  | And v -> v
  | Xor v -> v

let pf = Format.fprintf

let pp pp_val ppf = function
  | Equal v -> pf ppf "=%a" pp_val v
  | Different v -> pf ppf "!%a" pp_val v
  | Greater v -> pf ppf "<%a" pp_val v
  | Lower v -> pf ppf ">%a" pp_val v
  | And v -> pf ppf "&%a" pp_val v
  | Xor v -> pf ppf "^%a" pp_val v

let is = function '=' | '!' | '<' | '>' | '&' | '^' -> true | _ -> false

let process : type a. a Integer.t -> a -> a t -> bool =
 fun w a -> function
  | Equal b -> Integer.equal w a b
  | Different b -> Integer.different w a b
  | Greater b -> Integer.greater w a b
  | Lower b -> Integer.lower w a b
  | And b ->
      let v = Integer.bitwise_and w a b in
      Integer.equal w v b
      (* XXX(dinosaure): to specify that the value from the file must have set all of the bits
       * that are set in the specified value. *)
  | Xor b ->
      let v = Integer.bitwise_xor w a b in
      Integer.different w v (Integer.zero w)

let process_float a = function
  | Equal b -> Float.equal a b
  | Different b -> not (Float.equal a b)
  | Greater b -> a > b
  | Lower b -> a < b
  | And _ | Xor _ -> invalid_arg "Invalid bitwise operator on float"

let process_string a = function
  | Equal b -> a = b
  | Different b -> a <> b
  | Greater b -> a > b
  | Lower b -> a < b
  | And _ | Xor _ -> invalid_arg "Invalid bitwise operation on string"

let process_ptime a c =
  let a = Ptime.Span.to_float_s a in
  let c = map ~f:Ptime.Span.to_float_s c in
  process_float a c
