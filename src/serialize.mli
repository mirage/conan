type 'a t = Format.formatter -> 'a -> unit

val cut : _ t

val fmt : ('a, Format.formatter, unit) format -> Format.formatter -> 'a

val char : char t

val int : int t

val int32 : int32 t

val int64 : int64 t

val float : float t

val string : string t

val pair : 'a t -> 'b t -> ('a * 'b) t

val option : 'a t -> 'a option t

val parens : 'a t -> 'a t

val ptime_span : Ptime.Span.t t

val re : Re.t t

val list : 'a t -> 'a list t
