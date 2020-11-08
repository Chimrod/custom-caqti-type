include Caqti_type

let tup5 a b c d e : ('a * 'b * 'c  * 'd * 'e) Caqti_type.t =
  let encode (a, b, c, d, e) =
    Ok ((a, b, c, d), e)
  and decode ((a, b, c, d), e) =
    Ok (a, b, c, d, e)
  and rep =
    tup2 (tup4 a b c d) e
  in
  custom ~encode ~decode rep

let tup6 a b c d e f : ('a * 'b * 'c  * 'd * 'e * 'f) Caqti_type.t =
  let encode (a, b, c, d, e, f) =
    Ok ((a, b, c, d), (e, f))
  and decode ((a, b, c, d), (e, f)) =
    Ok (a, b, c, d, e, f)
  and rep =
    tup2 (tup4 a b c d) (tup2 e f)
  in
  custom ~encode ~decode rep

let tup7 a b c d e f g : ('a * 'b * 'c  * 'd * 'e * 'f * 'g) Caqti_type.t =
  let encode (a, b, c, d, e, f, g) =
    Ok ((a, b, c, d), (e, f, g))
  and decode ((a, b, c, d), (e, f, g)) =
    Ok (a, b, c, d, e, f, g)
  and rep =
    tup2 (tup4 a b c d) (tup3 e f g)
  in
  custom ~encode ~decode rep

let tup8 a b c d e f g h : ('a * 'b * 'c  * 'd * 'e * 'f * 'g * 'h) Caqti_type.t =
  let encode (a, b, c, d, e, f, g, h) =
    Ok ((a, b, c, d), (e, f, g, h))
  and decode ((a, b, c, d), (e, f, g, h)) =
    Ok (a, b, c, d, e, f, g, h)
  and rep =
    tup2 (tup4 a b c d) (tup4 e f g h)
  in
  custom ~encode ~decode rep

let tup9 a b c d e f g h i : ('a * 'b * 'c  * 'd * 'e * 'f * 'g * 'h * 'i) Caqti_type.t =
  let encode (a, b, c, d, e, f, g, h, i) =
    Ok ((a, b, c, d), (e, f, g, h), i)
  and decode ((a, b, c, d), (e, f, g, h), i) =
    Ok (a, b, c, d, e, f, g, h, i)
  and rep =
    tup3 (tup4 a b c d) (tup4 e f g h) i
  in
  custom ~encode ~decode rep

let tup10 a b c d e f g h i j : ('a * 'b * 'c  * 'd * 'e * 'f * 'g * 'h * 'i * 'j) Caqti_type.t =
  let encode (a, b, c, d, e, f, g, h, i, j) =
    Ok ((a, b, c, d), (e, f, g, h), (i, j))
  and decode ((a, b, c, d), (e, f, g, h), (i, j)) =
    Ok (a, b, c, d, e, f, g, h, i, j)
  and rep =
    tup3 (tup4 a b c d) (tup4 e f g h) (tup2 i j)
  in
  custom ~encode ~decode rep
