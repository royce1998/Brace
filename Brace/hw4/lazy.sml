structure Lazy : REDUCTION_SYSTEM = struct

  structure U = ULC

  fun strip t = (case t
    of SOME t' => t'
    | NONE => raise Fail "strip fail")

  fun step (t : ULC.term) : (ULC.term option) = if (U.isNormalForm t) then NONE
    else (case t
      of (U.App (t1, t2)) => if not (U.isNormalForm t) then (SOME (U.App(strip(step t1), t2)))
        else (case t1
          of (U.Abs (x, y)) => SOME(Subst.subst(x, t2, y))
          | _ => NONE)
      | _ => NONE)
  fun reduce t = (case step t of NONE => t | SOME t' => reduce t')
  fun steps t  = (case step t of NONE => t :: [] | SOME t' => (t :: steps t'))

end
