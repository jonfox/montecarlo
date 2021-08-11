
import "../../diku-dk/cpprandom/random"

module mk_brownians_rng(T: float)(E: rng_engine) = {
  type t = T.t

  module rand_dist = normal_distribution T E
  type rng = rand_dist.engine.rng
  local let std_moments = { mean = (T.f32 0.0), stddev = (T.f32 1.0)}
  
  import "brownianbridge"
  module bridge = mk_brownian_bridge T

  let build [m] (times: [m]t) (n: i64) (seed: i32): [n][m]t =
    let generate_variates (start_rng: rng): ([m]t, rng) =
      let rngs = rand_dist.engine.split_rng m start_rng
      let (rngs, xs) = unzip (map (rand_dist.rand std_moments) rngs)
      let end_rng = rand_dist.engine.join_rng rngs
      in (xs, end_rng)

    let bridge_params = bridge.build times
    let rng_state = rand_dist.engine.rng_from_seed [seed]

    let paths = replicate n (replicate m (T.f32 0.0))
    let (tensor, _) = loop (paths, rng_state) = (paths, rng_state) for i < n do
      let (variates, rng_state) = generate_variates rng_state
      let bridged = bridge.transform bridge_params variates
      in (paths with [i] = bridged, rng_state)
    
    in tensor
}

module mk_brownians_sobol (T: float) = {
  type t = T.t

  import "../../diku-dk/statistics/statistics"
  module stats = mk_statistics T
  let std_gaussian = stats.mk_normal {mu = (T.f32 0.0), sigma = (T.f32 1.0)}
  local let uniform_to_gaussian (x: t): t = stats.sample std_gaussian x

  import "../../diku-dk/sobol/sobol"
  import "../../diku-dk/sobol/sobol-dir-21201"

  import "brownianbridge"
  module bridge = mk_brownian_bridge T

  -- let build [m] (times: [m]t) (n: i64): [n][m]t =
  --   -- TODO build browians with Sobol
}