structure Eval : sig

  val step : AST.term -> AST.term option
  val eval : AST.term -> AST.term

end = struct

  structure A = AST

  fun step (A.If (A.True, t2, t3)) = SOME t2
    | step (A.If (A.False, t2, t3)) = SOME t3
    | step (A.If (t1, t2, t3)) =
      let
        val t1' = step t1
      in
        (case t1'
          of SOME t1' => SOME(A.If (t1', t2, t3))
          | NONE => raise Fail "If fail")
      end
    | step A.Zero = NONE
    | step A.True = NONE
    | step A.False = NONE
    | step (A.Succ(t)) = (if A.isNumericValue(t) then NONE else
      let
        val t' = step t
      in
        (case t'
          of SOME t' => SOME(A.Succ(t'))
          | NONE => raise Fail "Succ fail")
      end)
    | step (A.Pred(A.Succ(t))) = (if A.isNumericValue(t) then SOME t else
      let
        val t' = step t
      in
        (case t'
          of SOME t' => SOME(A.Pred(A.Succ(t')))
          | NONE => raise Fail "Pred fail")
      end)
    | step (A.Pred(A.Zero)) = SOME(A.Zero)
    | step (A.IsZero(A.Zero)) = SOME(A.True)
    | step (A.IsZero(A.Succ(t))) = SOME(A.False)
    | step (A.IsZero(t)) =
      let
        val t' = step t
      in
        (case t'
          of SOME t' => SOME(A.IsZero(t'))
          | NONE => raise Fail "Not numerical value in IsZero")
      end
    | step (A.And(A.False, _)) = SOME(A.False)
    | step (A.And(A.True, t)) = SOME(t)
    | step (A.And(t1, t2)) =
      let
        val t1' = step t1
      in
        (case t1'
          of SOME t1' => SOME(A.And(t1', t2))
          | NONE => raise Fail "And fail")
      end
    | step (A.Or(A.True, _)) = SOME(A.True)
    | step (A.Or(A.False, t)) = SOME(t)
    | step (A.Or(t1, t2)) =
      let
        val t1' = step t1
      in
        (case t1'
          of SOME t1' => SOME(A.Or(t1', t2))
          | NONE => raise Fail "Or fail")
      end
    | step (A.Equal(A.Succ(t1), A.Succ(t2))) = SOME(A.Equal(t1, t2))
    | step (A.Equal(A.True, A.True)) = SOME(A.True)
    | step (A.Equal(A.False, A.False)) = SOME(A.True)
    | step (A.Equal(A.True, A.False)) = SOME(A.False)
    | step (A.Equal(A.False, A.True)) = SOME(A.False)
    | step (A.Equal(A.Zero, A.Zero)) = SOME(A.True)
    | step (A.Equal(A.Succ(t), Zero)) = SOME(A.False)
    | step (A.Equal(Zero, A.Succ(t))) = SOME(A.False)
    | step (A.Equal(t1, t2)) = if A.isValue(t1) then (
        let
          val t2' = step t2
        in
          (case t2'
            of SOME t2' => SOME(A.Equal(t1, t2'))
            | NONE => raise Fail "Equal fail")
        end)
      else (
        let
          val t1' = step t1
        in
          (case t1'
            of SOME t1' => SOME(A.Equal(t1', t2))
            | NONE => raise Fail "Equal fail")
        end)
    | step _ = NONE

  fun eval (t : A.term) : A.term =
    let
      fun lp t = (case (step t)
        of NONE => t
        | SOME t => lp t)
    in
      lp t
    end

    (* in eval, take a step at a time until you can step no more *)

end
