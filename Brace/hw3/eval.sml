structure Eval : sig

  val step  : AST.term -> AST.term option
  val subst : string * AST.term * AST.term -> AST.term
  val eval  : AST.term -> AST.term

  val isNormalForm : AST.term -> bool
  val isStuck      : AST.term -> bool

end = struct

  structure A = AST

(* read subst as "rewrite x to v in t" *)
  fun subst (x, v, t) = (case t
    of (A.If (t1, t2, t3)) => (A.If(subst (x, v, t1), subst (x, v, t2), subst (x, v, t3)))
    | A.Succ t => A.Succ (subst(x, v, t))
    | A.Pred t => A.Pred (subst(x, v, t))
    | A.IsZero t => A.IsZero (subst(x, v, t))
    | A.And (t1, t2) => (A.And (subst(x, v, t1), subst(x, v, t2)))
    | A.Or (t1, t2) => (A.Or (subst(x, v, t1), subst(x, v, t2)))
    | A.Not t => A.Not (subst(x, v, t))
    | A.Equal (t1, t2) => (A.Equal (subst(x, v, t1), subst(x, v, t2)))
    | A.Var str => if (str = x) then v else t
    | A.Let (str, t1, t2) => subst(x, v, subst(str, t1, t2))
    | A.Pair (t1, t2) => A.Pair(subst(x, v, t1), subst(x, v, t2))
    | A.Select1 t => A.Select1 (subst(x, v, t))
    | A.Select2 t => A.Select2 (subst(x, v, t))
    | A.Some t => A.Some (subst(x, v, t))
    | A.OptCase (t1, (str, t2), t3) => A.OptCase (subst (x, v, t1), (str, (subst (x, v, t2))), subst(x, v, t3))
    | _ => raise Fail "cannot subst")

(* please note that most of step can be copied from hw2 *)
(* step should not raise an error on stuck terms, it should just return NONE *)
  fun step (A.If (A.True, t2, t3)) = SOME t2
    | step (A.If (A.False, t2, t3)) = SOME t3
    | step (A.If (t1, t2, t3)) =
      let
        val t1' = step t1
      in
        (case t1'
          of SOME t1' => SOME(A.If (t1', t2, t3))
          | NONE => NONE)
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
          | NONE => NONE)
      end)
    | step (A.Pred(A.Succ(t))) = (if A.isNumericValue(t) then SOME t else
      let
        val t' = step t
      in
        (case t'
          of SOME t' => SOME(A.Pred(A.Succ(t')))
          | NONE => NONE)
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
          | NONE => NONE)
      end
    | step (A.And(A.False, _)) = SOME(A.False)
    | step (A.And(A.True, t)) = SOME(t)
    | step (A.And(t1, t2)) =
      let
        val t1' = step t1
      in
        (case t1'
          of SOME t1' => SOME(A.And(t1', t2))
          | NONE => NONE)
      end
    | step (A.Or(A.True, _)) = SOME(A.True)
    | step (A.Or(A.False, t)) = SOME(t)
    | step (A.Or(t1, t2)) =
      let
        val t1' = step t1
      in
        (case t1'
          of SOME t1' => SOME(A.Or(t1', t2))
          | NONE => NONE)
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
            | NONE => NONE)
        end)
      else (
        let
          val t1' = step t1
        in
          (case t1'
            of SOME t1' => SOME(A.Equal(t1', t2))
            | NONE => NONE)
        end)
    | step (A.Not A.True) = SOME(A.False)
    | step (A.Not A.False) = SOME(A.True)
    | step (A.Not t) =
      let
        val t' = step t
      in
        (case t'
          of SOME t' => SOME(A.Not t')
          | NONE => NONE)
      end
    | step (A.Unit) = NONE
    | step (A.Var _) = NONE
    | step (A.Let(str, t1, t2)) = SOME(subst(str, t1, t2))
    | step (A.Pair(t1, t2)) = if (not (A.isValue t1)) andalso (case step t1 of NONE => true | _ => false) then NONE
      else if (A.isValue t1) andalso (case step t1 of NONE => true | _ => false) then
        let
          val t2' = step t2
        in
          (case t2'
            of SOME t2' => SOME(A.Pair (t1, t2'))
            | NONE => NONE)
        end
      else
        let
          val t1' = step t1
        in
          (case t1'
            of SOME t1' => SOME(A.Pair (t1', t2))
            | NONE => NONE)
        end
    | step (A.Select1 (A.Pair (t1, t2))) = SOME(t1)
    | step (A.Select1 t) =
      let
        val t' = step t
      in
        (case t'
          of SOME t' => SOME(A.Select1 t')
          | NONE => NONE)
      end
    | step (A.Select2 (A.Pair (t1, t2))) = SOME(t2)
    | step (A.Select2 t) =
      let
        val t' = step t
      in
        (case t'
          of SOME t' => SOME(A.Select2 t')
          | NONE => NONE)
      end
    | step (A.Some t) =
      let
        val t' = step t
      in
        (case t'
          of SOME t' => SOME(A.Some t')
          | NONE => NONE)
      end
    | step (A.None _) = NONE
    | step (A.OptCase(t1, (str, t2), t3)) =
      let
        val t1' = step t1
      in
        (case t1'
          of SOME t1 => SOME(A.OptCase(t1, (str, t2), t3))
          | NONE => (case t1
            of (A.Some v) =>
              let
                val t2' = step (subst (str, v, t2))
              in
                (case t2'
                  of SOME t2' => SOME(t2')
                  | NONE => SOME(t2))
              end
            | A.None _ => SOME(t3)
            | _ => NONE))
      end
    | step _ = NONE

(* note: isNormalForm and isStuck have one-line implementations *)

  fun isNormalForm t = (A.isValue t) andalso (case step t of NONE => true | _ => false)

  fun isStuck t = (not (A.isValue t)) andalso (case step t of NONE => true | _ => false)

  fun eval t =
   (case step t
      of NONE => t
       | SOME t' => eval t')

end
