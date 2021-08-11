import "brownians"
import "../../diku-dk/cpprandom/random"

module brownians_rng = mk_brownians_rng f64 pcg32

let main [m] (times: [m]f64) (n: i64): [n][m]f64 =
  brownians_rng.build times n 123
