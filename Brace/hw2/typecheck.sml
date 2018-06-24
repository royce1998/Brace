structure TypeCheck : sig

  datatype ty
    = Nat
    | Bool

  val typeof : AST.term -> ty
  val check  : AST.term -> unit

end = struct

  structure A = AST

  datatype ty = Nat | Bool

  fun typeof (t : A.term) : ty =
    (case t
      of A.True => Bool
      | A.False => Bool
      | A.Zero => Nat
      | A.If(t1, t2, t3) =>
        (case typeof(t1)
          of Nat => raise Fail "Boolean expected in If"
          | Bool =>
            (case typeof(t2)
              of Nat => (case typeof(t3)
                of Nat => Nat
                | Bool => raise Fail "Incompatible types in If")
              | Bool => (case typeof(t3)
                of Bool => Bool
                | Nat => raise Fail "Incompatible types in If")))
      | A.Succ(t') =>
        (case typeof(t')
          of Nat => Nat
          | Bool => raise Fail "Invalid type in Succ")
      | A.Pred(t') =>
        (case typeof(t')
          of Nat => Nat
          | Bool => raise Fail "Invalid type in Pred")
      | A.IsZero(t') =>
        (case typeof(t')
          of Nat => Nat
          | Bool => raise Fail "Invalid type in IsZero")
      | A.And(t1, t2) =>
        (case typeof(t1)
          of Nat => raise Fail "Bool expected in And"
          | Bool =>
            (case typeof(t2)
              of Bool => Bool
              | Nat => raise Fail "Bool expected in And"))
      | A.Or(t1, t2) =>
        (case typeof(t1)
          of Nat => raise Fail "Bool expected in Or"
          | Bool =>
            (case typeof(t2)
              of Bool => Bool
              | Nat => raise Fail "Bool expected in Or"))
      | A.Not(t') =>
        (case typeof(t')
          of Bool => Bool
          | Nat => raise Fail "Invalid type in Not")
      | A.Equal(t1, t2) =>
        (case typeof(t1)
          of Nat =>
            (case typeof(t2)
              of Nat => Nat
              | Bool => raise Fail "Incompatible types in Equal")
          | Bool =>
            (case typeof(t2)
              of Bool => Bool
              | Nat => raise Fail "Incompatible types in Equal")))

  fun check t =
    let
      val _ = typeof t
    in
      ()
    end

end
