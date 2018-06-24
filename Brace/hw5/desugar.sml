structure Desugar : sig

  val term : AST.term -> MULC.term

end = struct

  fun term t = (case t
    of (AST.Var s) => (MULC.Var s)
    | (AST.Abs (ls, t)) => (MULC.Abs (ls, term t))
    | (AST.App ts) =>
      let
        fun termList ts = (case ts
          of [] => []
          | (t :: ts) => ((term t) :: (termList ts)))
      in
        (MULC.App (termList ts))
      end
    | (AST.Let (s, t1, t2)) => MULC.App([MULC.Abs([s], term t2), term t1])
    | (AST.LetRec (s, t1, t2)) =>
      let
        val fix = MULC.Abs (["f"], MULC.App [(MULC.Abs (["x"], (MULC.App [MULC.Var "f", (MULC.App [MULC.Var "x", MULC.Var "x", MULC.Var "y"])]))), (MULC.Abs (["x"], (MULC.App [MULC.Var "f", (MULC.App [MULC.Var "x", MULC.Var "x", MULC.Var "y"])])))])
      in
        MULC.App([MULC.Abs([s], term t2), (MULC.App [fix, MULC.Abs([s], term t1)])])
      end
    | (AST.True) => MULC.Abs(["t", "f"], MULC.Var("t"))
    | (AST.False) => MULC.Abs(["t", "f"], MULC.Var("f"))
    | (AST.If (t1, t2, t3)) => (MULC.App (term t1 :: term t2 :: term t2 :: []))
    | (AST.Not t) => MULC.App(MULC.Abs(["b"], MULC.App([MULC.Var("b"), term AST.False, term AST.True])) :: term t :: [])
    | (AST.And (t1, t2)) => MULC.App([(MULC.Abs(["b1", "b2"],  MULC.App([MULC.Var("b1"), MULC.Var("b2"), term AST.False]))), term t1, term t2])
    | (AST.Or (t1, t2)) => MULC.App([(MULC.Abs(["b1", "b2"],  MULC.App([MULC.Var("b1"), term AST.True, MULC.Var("b2")]))), term t1, term t2])
    | (AST.Nat i) =>
      let
        fun lp i = (if i = 0 then MULC.Var("z")
          else MULC.App([MULC.Var("s"), lp (i - 1)]))
      in
        MULC.Abs(["s", "z"], lp i)
      end
    | (AST.Add (t1, t2)) => MULC.App [term t1, (MULC.Abs (["n", "s", "z"], (MULC.App [MULC.Var "s", (MULC.App [MULC.Var "n", MULC.Var "s", MULC.Var "z"])]))), term t2]
    | (AST.Mult (t1, t2)) => (MULC.App [MULC.Abs(["x", "y", "z"], MULC.App [MULC.Var("x"), MULC.App [MULC.Var("y"), MULC.Var("z")]]), term t1, term t2])
    | (AST.Pair (t1, t2)) => MULC.Abs(["b"], MULC.App([MULC.Var("b"),term t1, term t2]))
    | (AST.Select1 t) => (case (term t)
      of (MULC.Abs(_, MULC.App([_, t1, t2]))) => MULC.App([term AST.True, t1, t2])
      | _ => raise Fail "Not a pair in selector")
    | (AST.Select2 t) => (case (term t)
      of (MULC.Abs(_, MULC.App([_, t1, t2]))) => MULC.App([term AST.False, t1, t2])
      | _ => raise Fail "Not a pair in selector")
    | (AST.ID s) => MULC.Abs([s], MULC.Var(s)))

end
