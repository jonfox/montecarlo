-- | Browian bridge implementation
-- a pinned Brownian motion as described in Mark Joshi's More Mathematical Finance, pp 175-183

module type brownian_bridge = {
  -- | The type of scalars that the functions operate on.
  type t

  -- | the Brownian bridge parameters
  type BridgeParams [n] = {
    left_index: [n]i64,
    right_index: [n]i64,
    bridge_index: [n]i64,
    left_weight: [n]t,
    right_weight: [n]t,
    std_dev: [n]t
  }

  -- | Builds the bridge parameters
  val build [n]: (times: [n]t) -> BridgeParams [n]

  -- | Transforms a set of variates using the bridge parameters
  val transform [n]: BridgeParams [n] -> [n]t -> [n]t
}

module mk_brownian_bridge(T: float): brownian_bridge with t = T.t = {
  type t = T.t

  local let zero = (T.f32 0.0)
  local let one = (T.f32 1.0)

  import "utils"

  type BridgeParams [n] = {
    left_index: [n]i64,
    right_index: [n]i64,
    bridge_index: [n]i64,
    left_weight: [n]t,
    right_weight: [n]t,
    std_dev: [n]t
  }

  let build [n] (times: [n]t): BridgeParams [n] =
    let (left_index, right_index, bridge_index, left_weight, right_weight, std_dev, _, _) =
      let left_index = replicate n 0
      let right_index = replicate n 0
      let bridge_index = tabulate n (\i -> if i == 0 then n - 1 else 0)
      let left_weight = replicate n zero
      let right_weight = replicate n zero
      let std_dev = tabulate n (\i -> if i == 0 then T.sqrt(times[n - 1]) else zero)
      let step_map = tabulate n (\i -> if i == n - 1 then 1 else 0)
      let j = 0

      in loop (left_index, right_index, bridge_index, left_weight, right_weight, std_dev, step_map, j) for i in 1..<n do
          let j = first_index_where (\x -> x == 0) j step_map
          let k = first_index_where (\x -> x != 0) j step_map
          let l = j + ((k - 1 - j) >> 1)

          let (lw, sd) = if (j > 0) then
            let lw_i = if k == j - 1 then one else (times[k] T.- times[l]) T./ (times[k] T.- times[j - 1])
            let sd_i = T.sqrt((times[l] T.- times[j - 1]) T.* lw_i) 
            in (lw_i, sd_i)
          else
            let lw_i = (times[k] T.- times[l]) T./ times[k]
            let sd_i = T.sqrt(times[l] T.* lw_i)
            in (lw_i, sd_i)

          let rw = one T.- lw
          
          let updated_j = if k + 1 >= n then 0 else k + 1

          in (
            left_index with [i] = j,
            right_index with [i] = k,
            bridge_index with [i] = l,
            left_weight with [i] = lw,
            right_weight with [i] = rw,
            std_dev with [i] = sd,
            step_map with [l] = i,
            updated_j
            )
    in {
      left_index,
      right_index,
      bridge_index,
      left_weight,
      right_weight,
      std_dev
    }

  let transform [n] (params: BridgeParams [n]) (variates: [n]t): [n]t =
    let path = tabulate n (\i ->
      if i == n - 1 then params.std_dev[0] T.* variates[0] else zero)
    in loop (path) for i in 1..<n do
      let j = params.left_index[i]
      let k = params.right_index[i]
      let l = params.bridge_index[i]
      let right_step = params.right_weight[i] T.* path[k] T.+ params.std_dev[i] T.* variates[i]
      let path_l = if j > 0 then
          params.left_weight[i] T.* path[j - 1] T.+ right_step
        else
          right_step
      in path with [l] = copy(path_l)
}
