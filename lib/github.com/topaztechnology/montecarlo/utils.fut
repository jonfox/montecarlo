-- Find the first index in a sequence that matches a predicate given a starting index
let first_index_where [n] (f: i64 -> bool) (start_index: i64) (xs: [n]i64): i64 =
  let i = start_index
  let x = xs[i]
  let (_, i) = loop (x, i) while !(f x) && i < n do
    let next_i = i + 1
    let next_x = if next_i == n then -1 else xs[next_i]
    in (next_x, next_i)
  in i
