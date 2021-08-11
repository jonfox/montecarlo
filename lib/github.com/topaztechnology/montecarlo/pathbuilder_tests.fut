-- | ignore

import "brownians"
import "pathbuilder"
import "../../diku-dk/cpprandom/random"
import "../../diku-dk/statistics/statistics"

module brownians_rng = mk_brownians_rng f64 pcg32
module pathbuilder = mk_pathbuilder f64
module stats = mk_statistics f64

-- ==
-- entry: test_path_moments
-- input { [100.0, 100.0, 100.0] [20.0, 20.0, 20.0] [1.0, 2.0, 3.0] 10000i64 }
-- output { [[100.0, 10.0], [100.0, 10.0], [100.0, 10.0]] }

entry test_path_moments [m] (prices: [m]f64) (vols: [m]f64) (times: [m]f64) (n: i64): [m][2]f64 =
  let steps = 
    let brownians = brownians_rng.build times n 123
    let paths = pathbuilder.build brownians prices vols times
    in transpose paths

  in steps |> map(\paths ->
      -- let logxs = map (\x -> f64.log(x)) paths
      let mu = stats.mean paths
      let sigma = stats.stddev paths
      in [mu, sigma]
    )
