structure Subst : sig

  val subst    : string * ULC.term * ULC.term -> ULC.term
  val freeVars : ULC.term -> string list

end = struct

  structure U = ULC

(* Note: variables, in this compiler, are just strings.
 * You do not need to implement a fast set.
 * Quadratic-time and/or quadratic-space operations are acceptable.
 * In a production setting, they wouldn't be.
 *)
  structure VarSet : sig
    type set
    val empty     : set
    val isEmpty   : set -> bool
    val size      : set -> int
    val singleton : string -> set
    val member    : string * set -> bool
    val union     : set * set -> set
    val remove    : set * string -> set
    val toList    : set -> string list
  end = struct
    type set = string list (* todo: replace this with a workable type *)
    val empty = []  (* todo: replace this with a workable value *)
    fun isEmpty s : bool = if s = [] then true else false
    fun size s : int = case s of [] => 0 | (t :: ts) => 1 + (size ts)
    fun singleton s = s :: []
    fun member (str, ns) : bool =
    (case ns
      of [] => false
      | (str' :: ns) => if str = str' then true else member (str, ns))
    fun union (s1, s2) : set = (case s1 of [] => s2 | (s :: s1') => s :: (union (s1', s2)))
    fun remove (s, str) = (List.filter (fn str' => str' <> str) s)
    fun toList s = s
  end

  val counter = ref 0
  fun freshVarName () =
    let
      val n = "@" ^ Int.toString (!counter)
      val _ = (counter := (1 + !counter))
    in
      n
    end

(* Follow the definition of FV as in the text and the README. *)
  fun FV (t : U.term) : VarSet.set =
    (case t
      of (U.Var x) => VarSet.singleton(x)
      | (U.Abs (x, t1)) => VarSet.remove((FV t1), x)
      | (U.App (t1, t2)) => VarSet.union(FV t1, FV t2))

  val freeVars = VarSet.toList o FV

(* Sub x for s in term y *)
  fun subst (x, s, y) : U.term = (case y
    of (U.Var str) => if x = str then s else y
    | (U.App (t1, t2)) => (U.App ((subst (x, s, t1)), (subst (x, s, t2))))
    | (U.Abs (x', t1)) => if x = x' then y
      else if VarSet.member(x', (FV s)) then
        let
          val y' = freshVarName()
        in
          subst (x, (U.Abs (y', (subst (x', t1, (U.Var y'))))), s)
        end
      else
        (U.Abs (x', subst (x, t1, s))))

end
