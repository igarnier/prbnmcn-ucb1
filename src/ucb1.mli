(** The UCB1 module is parameterised by a finite set of actions
    presented as an array of abstract "arms", each arm corresponding
    to an action. *)
module type Arm_sig = sig
  type t

  val compare : t -> t -> int

  val pp : Format.formatter -> t -> unit
end

(** Phantom types used to tag the state of the bandit. *)

(** [awaiting_rewards] tags bandits from which an arm was selected and are awaiting
    the reward associated to this arm. *)
type awaiting_reward

(** [ready_to_move] tags bandits that are ready to perform another action. *)
type ready_to_move

(** Statistics of a given arm. *)
type arm_statistics =
  { number_of_activations : float;
        (** Number of times this arm was activated. *)
    cumulative_reward : float;  (** Total reward gathered by this arm. *)
    empirical_reward : float  (** Average reward gathered by this arm. *)
  }

module type S = sig
  (** The type of arms (i.e. actions) *)
  type arm

  (** The state of a bandit. *)
  type 'state t

  (** Create a fresh bandit with given arms. *)
  val create : arm array -> ready_to_move t

  (** Select the UCB1-optimal action to play. The bandit expects a reward. *)
  val next_action : ready_to_move t -> arm * awaiting_reward t

  (** Assign a reward to the bandit.

      @raises Invalid_argument if [reward] is not in the unit interval. *)
  val set_reward : awaiting_reward t -> float -> ready_to_move t

  (** Total rewards obtained by the bandit. *)
  val total_rewards : ready_to_move t -> float

  (** [find_best_arm bandit f] returns the arm that maximizes [f], together with the
      maximizing value. *)
  val find_best_arm : 'state t -> (arm_statistics -> float) -> arm * float

  (** Pretty-print useful statistics on the bandit, for debugging purposes.*)
  val pp_stats : Format.formatter -> 'state t -> unit
end

module Make : functor (Arm : Arm_sig) -> S with type arm = Arm.t
