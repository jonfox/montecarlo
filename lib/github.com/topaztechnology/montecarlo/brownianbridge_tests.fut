-- | ignore

import "brownianbridge"
module bridge = mk_brownian_bridge f64

import "../../diku-dk/cpprandom/random"
module rand_dist = normal_distribution f64 pcg32
type rng = rand_dist.engine.rng
local let std_moments = { mean = 0.0f64, stddev = 1.0f64}

import "../../diku-dk/statistics/statistics"
module stats = mk_statistics f64


-- | Two step tests

-- ==
-- entry: test_brownian_bridge2_left_index
-- input { [1.0, 2.0] }
-- output { [0i64, 0i64] }

entry test_brownian_bridge2_left_index [n] (times: [n]f64) = (bridge.build times).left_index

-- ==
-- entry: test_brownian_bridge2_right_index
-- input { [1.0, 2.0] }
-- output { [0i64, 1i64] }

entry test_brownian_bridge2_right_index [n] (times: [n]f64) = (bridge.build times).right_index

-- ==
-- entry: test_brownian_bridge2_bridge_index
-- input { [1.0, 2.0] }
-- output { [1i64, 0i64] }

entry test_brownian_bridge2_bridge_index [n] (times: [n]f64) = (bridge.build times).bridge_index

-- ==
-- entry: test_brownian_bridge2_left_weight
-- input { [1.0, 2.0] }
-- output { [0.0, 0.5] }

entry test_brownian_bridge2_left_weight [n] (times: [n]f64) = (bridge.build times).left_weight

-- ==
-- entry: test_brownian_bridge2_right_weight
-- input { [1.0, 2.0] }
-- output { [0.0, 0.5] }

entry test_brownian_bridge2_right_weight [n] (times: [n]f64) = (bridge.build times).right_weight

-- ==
-- entry: test_brownian_bridge2_std_dev
-- input { [1.0, 2.0] }
-- output { [1.4142135623730951, 0.7071067811865476] }

entry test_brownian_bridge2_std_dev [n] (times: [n]f64) = (bridge.build times).std_dev


-- | Five step tests

-- ==
-- entry: test_brownian_bridge5_left_index
-- input { [1.0, 2.0, 3.0, 4.0, 5.0] }
-- output { [0i64, 0i64, 0i64, 2i64, 3i64] }

entry test_brownian_bridge5_left_index [n] (times: [n]f64) = (bridge.build times).left_index

-- ==
-- entry: test_brownian_bridge5_right_index
-- input { [1.0, 2.0, 3.0, 4.0, 5.0] }
-- output { [0i64, 4i64, 1i64, 4i64, 4i64] }

entry test_brownian_bridge5_right_index [n] (times: [n]f64) = (bridge.build times).right_index

-- ==
-- entry: test_brownian_bridge5_bridge_index
-- input { [1.0, 2.0, 3.0, 4.0, 5.0] }
-- output { [4i64, 1i64, 0i64, 2i64, 3i64] }

entry test_brownian_bridge5_bridge_index [n] (times: [n]f64) = (bridge.build times).bridge_index

-- ==
-- entry: test_brownian_bridge5_left_weight
-- input { [1.0, 2.0, 3.0, 4.0, 5.0] }
-- output { [0.0, 0.6, 0.5, 0.6666666666666666, 0.5] }

entry test_brownian_bridge5_left_weight [n] (times: [n]f64) = (bridge.build times).left_weight

-- ==
-- entry: test_brownian_bridge5_right_weight
-- input { [1.0, 2.0, 3.0, 4.0, 5.0] }
-- output { [0.0, 0.4, 0.5, 0.33333333333333337, 0.5] }

entry test_brownian_bridge5_right_weight [n] (times: [n]f64) = (bridge.build times).right_weight

-- ==
-- entry: test_brownian_bridge5_std_dev
-- input { [1.0, 2.0, 3.0, 4.0, 5.0] }
-- output { [2.23606797749979, 1.0954451150103321, 0.7071067811865476, 0.816496580927726, 0.7071067811865476] }

entry test_brownian_bridge5_std_dev [n] (times: [n]f64) = (bridge.build times).std_dev

-- Bridge transform tests

-- ==
-- entry: test_distribution
-- input { [1.0, 2.0, 5.0, 10.0] 1000000i64 }
-- output { [[0.0, 1.0], [0.0, 2.0], [0.0, 5.0], [0.0, 10.0]] }

entry test_distribution [m] (times: [m]f64) (n: i64): [m][2]f64 =
  let bridge_params = bridge.build times

  let generate_path (start_rng: rng): ([m]f64, rng) =
    let rngs = rand_dist.engine.split_rng m start_rng
    let (rngs, xs) = unzip (map (rand_dist.rand std_moments) rngs)
    let end_rng = rand_dist.engine.join_rng rngs
    let bridged = bridge.transform bridge_params xs
    in (xs, end_rng)

  let rng_state = rand_dist.engine.rng_from_seed [123]

  let paths = replicate n (replicate m 0.0f64)
  let (paths, _) = loop (paths, rng_state) = (paths, rng_state) for i < n do
    let (path, rng_state) = generate_path rng_state
    in (paths with [i] = path, rng_state)

  let round_dp (dp: i8) (x: f64): f64 =
    let scale: f64 = 10.0 ** f64.i8 dp
    in f64.round(x * scale) / scale

  -- somehow mean totally broken, even without bridge..
  in transpose paths |> map (\sims ->
    let mu = stats.mean sims
    let sigma = stats.variance sims |> round_dp 3
    in [mu, sigma]
  )
