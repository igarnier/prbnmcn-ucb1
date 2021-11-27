open Ucb1

module Arm : Arm_sig with type t = int = struct
  type t = int

  let compare = Int.compare

  let pp = Format.pp_print_int
end

module Bandit = Ucb1.Make (Arm)

let test_zero_rewards () =
  let bandit = Bandit.create [| 0; 1; 2 |] in

  let (_arm, bandit) = Bandit.next_action bandit in

  let bandit = Bandit.set_reward bandit 0.0 in
  ignore (Format.printf "%a" Bandit.pp_stats bandit)

let () =
  let open Alcotest in
  run
    "Ucb1"
    [("zero rewards", [test_case "no assert false" `Quick test_zero_rewards])]
