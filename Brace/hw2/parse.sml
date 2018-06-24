structure Parse : sig

  val parse : Scan.token list -> AST.term

end = struct

  structure S = Scan
  structure A = AST

  fun nextTerm (tokens : S.token list) : (A.term * S.token list) option =
    let
      fun lp [] = NONE
      | lp (S.T :: toks) = SOME(A.True, toks)
      | lp (S.F :: toks) = SOME(A.False, toks)
      | lp (S.Zero :: toks) = SOME(A.Zero, toks)
      | lp (S.LBrace :: S.Bang :: toks) =
        (case lp toks
          of SOME(t1, S.RBrace :: toks) => SOME(A.Not t1, toks)
          | SOME _ => raise Fail "nextTerm fail"
          | NONE => raise Fail "nextTerm fail")
      | lp (S.LBrace :: S.If :: toks) =
        (case lp toks
          of SOME(t1, S.Then :: toks) =>
          (case lp toks
            of SOME(t2, S.Else :: toks) =>
            (case lp toks
              of SOME(t3, S.RBrace :: toks) => SOME(A.If(t1, t2, t3), toks)
              | _ => raise Fail "invalid If format")
            | _ => raise Fail "invalid If format")
          | _ => raise Fail "invalid If format")
      | lp (S.LBrace :: S.PlusOne :: toks) =
        (case lp toks
          of SOME(t1, S.RBrace :: toks) => SOME(A.Succ t1, toks)
          | SOME _ => raise Fail "PlusOne fail"
          | NONE => raise Fail "PlusOne fail")
      | lp (S.LBrace :: S.MinusOne :: toks) =
        (case lp toks
          of SOME(t1, S.RBrace :: toks) => SOME(A.Pred t1, toks)
          | SOME _ => raise Fail "MinusOne fail"
          | NONE => raise Fail "MinusOne fail")
      | lp (S.LBrace :: S.IsZ :: toks) =
        (case lp toks
          of SOME(t1, S.RBrace :: toks) => SOME(A.IsZero t1, toks)
          | SOME _ => raise Fail "IsZ fail"
          | NONE => raise Fail "IsZ fail")
      | lp (S.LBrace :: S.DoubleAmpersand :: toks) =
        (case lp toks
          of SOME(t1, toks) =>
          (case lp toks
            of SOME(t2, S.RBrace :: toks2) => SOME(A.And(t1, t2), toks2)
            | _ => raise Fail "And fail")
          | NONE => raise Fail "And fail")
      | lp (S.LBrace :: S.DoublePipe :: toks) =
        (case lp toks
          of SOME(t1, toks) =>
          (case lp toks
            of SOME(t2, S.RBrace :: toks2) => SOME(A.Or(t1, t2), toks2)
            | _ => raise Fail "Or fail")
          | NONE => raise Fail "Or fail")
      | lp (S.LBrace :: S.DoubleEqual :: toks) =
        (case lp toks
          of SOME(t1, toks) =>
          (case lp toks
            of SOME(t2, S.RBrace :: toks2) => SOME(A.Equal(t1, t2), toks2)
            | _ => raise Fail "Equal fail")
          | NONE => raise Fail "Equal fail")
      | lp _ = raise Fail "invalid parse"
    in
      lp tokens
    end

  fun parse tokens =
    (case nextTerm(tokens)
      of SOME(t, []) => t
      | SOME(t, _) => raise Fail "Some tokens unparsed"
      | _ => raise Fail "parse fail")
    (* parsing the whole token sequence should result in exactly one term *)

end
