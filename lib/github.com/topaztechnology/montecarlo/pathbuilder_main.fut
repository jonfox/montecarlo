import "brownians"
import "pathbuilder"
import "../../diku-dk/cpprandom/random"

module brownians_rng = mk_brownians_rng f64 pcg32
module pathbuilder = mk_pathbuilder f64

let main [m] (prices: [m]f64) (vols: [m]f64) (times: [m]f64) (n: i64): [n][m]f64 =
  let brownians = brownians_rng.build times n 123
  in pathbuilder.build brownians prices vols times
