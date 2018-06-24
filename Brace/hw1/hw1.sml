structure HW1 = struct

  (* === Problem 1 === *)

  (* Here are inductively defined natural numbers. *)

  datatype nat
    = Zero
    | Succ of nat

  (* In natToInt, replace the raise expression with an
   * implementation of the function.
   *)

  fun natToInt (n : nat) : int =
    (case n
			of Zero => 0
			| Succ n' => (1 + natToInt(n')))

  (* Having written natToInt, uncomment the following tests. *)


  val _ = Check.expect (natToInt Zero, 0, "natToInt Zero")
  val _ = Check.expect (natToInt (Succ Zero), 1, "natToInt (Succ Zero)")


  (* Continue by implementing intToNat and uncommenting the tests immediately below. *)

  fun intToNat (i : int) : nat =
    if i = 0 then Zero else (Succ(intToNat(i - 1)))


  val _ = Check.expect (intToNat 0, Zero, "intToNat 0")
  val _ = Check.expect (intToNat 1, Succ Zero, "intToNat 1")


  fun natAdd (m : nat, n : nat) : nat =
    intToNat(natToInt(m) + natToInt(n))

  (* Write some of your own Check.expect tests here, and continue
   * doing so throughout this set of exercises.
   *)
   val _ = Check.expect (natAdd(Zero, Zero), Zero, "natAdd(Zero, Zero)")
   val _ = Check.expect (natAdd(Succ(Zero), Succ(Succ(Zero))), Succ(Succ(Succ(Zero))), "natAdd(Succ(Zero), Succ(Succ(Zero)))")

  (* Write natEq without calling natToInt or intToNat. *)

  fun natEq (m : nat, n : nat) : bool =
    (case m
      of Zero => (case n of Zero => true | _ => false)
      | Succ m' => (case n of Zero => false | Succ n' => natEq(m',n')))

  val _ = Check.expect (natEq(Zero, Zero), true, "natEq(Zero,Zero)")
  val _ = Check.expect (natEq(Zero, Succ(Zero)), false, "natEq(Zero,Succ(Zero))")
  val _ = Check.expect (natEq(Succ(Zero), Succ(Zero)), true, "natEq(Succ(Zero),Succ(Zero))")
  val _ = Check.expect (natEq(Succ(Succ(Zero)), Succ(Zero)), false, "natEq(Succ(Succ(Zero)),Succ(Zero))")

  (* natGT checks if m is strictly greater than n. *)

  (* Write natGT without calling natToInt or intToNat. *)

  fun natGT (m : nat, n : nat) : bool =
    (case m
      of Zero => false
      | Succ m' => (case n of Zero => true | Succ n' => natGT(m',n')))

  val _ = Check.expect (natGT(Zero, Zero), false, "natGT(Zero, Zero)")
  val _ = Check.expect (natGT(Succ(Zero), Zero), true, "natGT(Succ(Zero), Zero)")
  val _ = Check.expect (natGT(Succ(Zero), Succ(Succ(Zero))), false, "natGT(Succ(Zero), Succ(Succ(Zero))), Zero)")

  (* natToString should build a strings like "Zero",
   * "Succ(Zero)","Succ(Succ(Zero))", etc.
   * The string concatenation operator is ^, as in "a" ^ "b".
   *)

  fun natToString (n : nat) : string =
    (case n
      of Zero => "Zero"
      | Succ n' => "Succ("^(natToString(n'))^")")

  val _ = Check.expect (natToString(Zero), "Zero", "natToString Zero")
  val _ = Check.expect (natToString(Succ(Zero)), "Succ(Zero)", "natToString Succ(Zero)")

  (* === Problem 2 === *)

  datatype filesize
    = B of int
    | KB of real
    | MB of real
    | GB of real
    | TB of real

  fun toMB (s : filesize) : real =
    (case s
      of B s' => real(s') / 1000.0 / 1000.0
      | KB s' => s' / 1000.0
      | MB s' => s'
      | GB s' => s' * 1000.0
      | TB s' => s' * 1000.0 * 1000.0)

    val _ = Check.expectBy (Real.==, toMB(MB(100.0)), 100.0, "toMB(MB(100))")
    val _ = Check.expectBy (Real.==, toMB(KB(100.0)), 0.1, "toMB(MB(100))")
    val _ = Check.expectBy (Real.==, toMB(GB(1.0)), 1000.0, "toMB(MB(100))")


  (* === Problem 3 === *)

  (* Here is a useful type synonym. *)
  type 'a pred = 'a -> bool

  (* The infix directive instructs the parser that the
   * given identifier is an infix operator.
   *)

  infix \/
  infix /\

  (* \/ is a "disjunctive composition" operator for tests.
   * Assuming you have tests isPrime and isOdd, then
   * the test (isPrime \/ isOdd) identifies primes and/or odds.
   *)

  val isBig = (fn (x) => x > 10)
  val isReallyBig = (fn (x) => x > 100)


  fun (p : 'a pred) \/ (q : 'a pred) : 'a pred =
    (fn (x) => p(x) orelse q(x))

  val _ = Check.expect((isBig \/ isReallyBig)13, true, "Test1.13")
  val _ = Check.expect((isBig \/ isReallyBig)5, false, "Test1.5")

  (* /\ is a "conjunctive composition" operator for tests.
   * Assuming you have tests isPrime and isOdd, then
   * the test (isPrime /\ isOdd) identifies odd primes.
   *)

  fun (p : 'a pred) /\ (q : 'a pred) : 'a pred =
    (fn (x) => p(x) andalso q(x))

  val _ = Check.expect((isBig /\ isReallyBig)13, false, "Test2.13")
  val _ = Check.expect((isBig /\ isReallyBig)1300, true, "Test2.1300")

  (* === Problem 4 === *)

  (* Here is a mutually recursive datatype for trees
   * that alternate between having 2 and 3 children at
   * each level, and furthermore alternate between
   * having 'a and 'b data at each level.
   *)

  (* E2 is an empty tree of type t2; E3 likewise for t3. *)

  datatype ('a, 'b) t2
    = E2
    | Nd2 of 'a * ('a, 'b) t3 * ('a, 'b) t3
  and ('a, 'b) t3
    = E3
    | Nd3 of 'b * ('a, 'b) t2 * ('a, 'b) t2 * ('a, 'b) t2

  (* Count the number of nodes in a given tree. Nodes to be
   * counted are Nd2 and Nd3 values, not E2 or E3.
   *)

  val testTree = Nd3 (0, E2, E2, Nd2(1, E3, E3))
  val emptyTree = E2

  fun numNodes2 (t : ('a, 'b) t2) : int =
    (case t
      of E2 => 0
      | Nd2 (_,t',t'') => 1 + numNodes3(t') + numNodes3(t''))
  and numNodes3 (t : ('a, 'b) t3) : int =
      (case t
        of E3 => 0
        | Nd3 (_,t',t'',t''') => 1 + numNodes2(t') + numNodes2(t'') + numNodes2(t'''))

  val _ = Check.expect(numNodes2(emptyTree), 0, "numNodes2")
  val _ = Check.expect(numNodes3(testTree), 2, "numNodes3")

  (* === Problem 5 === *)

  datatype rank
    = Ace | Two | Three | Four | Five | Six | Seven
      | Eight | Nine | Ten | Jack | Queen | King

  datatype suit
    = Diamond | Clubs | Hearts | Spades

  datatype card
    = Card of rank * suit

  val card1 = Card (Two, Hearts)
  val card2 = Card (Two, Spades)
  val card3 = Card (Jack, Hearts)
  val card4 = Card (Queen, Clubs)

  fun sameSuit (c1 : card, c2 : card) : bool =
    (*)(case c1
      of Card (_,suit) => (case suit
        of Diamond => (case c2
          of Card (_,suit) => (case suit
            of Diamond => true | _ => false))
        | Clubs => (case c2
          of Card (_,suit) => (case suit
            of Clubs => true | _ => false))
        | Hearts => (case c2
          of Card (_,suit) => (case suit
            of Hearts => true | _ => false))
        | Spades => (case c2
          of Card (_,suit) => (case suit
            of Spades => true | _ => false))))*)
    (case c1
      of Card (_,suit1) => (case c2
        of Card (_,suit2) => (suit1 = suit2)))

  val _ = Check.expect(sameSuit(card1,card3), true, "sameSuit(card1,card3)")
  val _ = Check.expect(sameSuit(card1,card2), false, "sameSuit(card1,card2)")

  fun differentRank (c1 : card, c2 : card) : bool =
    (*)(case c1
      of Card (rank,_) => (case rank
        of Ace => (case c2
          of Card (rank,_) => (case rank
            of Ace => false | _ => true))
        | Two => (case c2
          of Card (rank,_) => (case rank
            of Two => false | _ => true))
        | Three => (case c2
          of Card (rank,_) => (case rank
            of Three => false | _ => true))
        | Four => (case c2
          of Card (rank,_) => (case rank
            of Four => false | _ => true))
        | Five => (case c2
          of Card (rank,_) => (case rank
            of Five => false | _ => true))
        | Six => (case c2
          of Card (rank,_) => (case rank
            of Six => false | _ => true))
        | Seven => (case c2
          of Card (rank,_) => (case rank
            of Seven => false | _ => true))
        | Eight => (case c2
          of Card (rank,_) => (case rank
            of Eight => false | _ => true))
        | Nine => (case c2
          of Card (rank,_) => (case rank
            of Nine => false | _ => true))
        | Ten => (case c2
          of Card (rank,_) => (case rank
            of Ten => false | _ => true))
        | Jack => (case c2
          of Card (rank,_) => (case rank
            of Jack => false | _ => true))
        | Queen => (case c2
          of Card (rank,_) => (case rank
            of Queen => false | _ => true))
        | King => (case c2
          of Card (rank,_) => (case rank
            of King => false | _ => true))))*)
    (case c1
      of Card (rank1,_) => (case c2
        of Card (rank2,_) => (rank1 <> rank2)))

  val _ = Check.expect(differentRank(card1,card2), false, "differentRank(card1,card2)")
  val _ = Check.expect(differentRank(card3,card4), true, "differentRank(card3,card4)")

  fun sameColor (c1 : card, c2 : card) : bool =
    (*)(case c1
      of Card (_,suit) => (case suit
        of Diamond => (case c2
          of Card (_,suit) => (case suit
            of Diamond => true | Hearts => true | _ => false))
        | Clubs => (case c2
          of Card (_,suit) => (case suit
            of Clubs => true | Spades => true | _ => false))
        | Hearts => (case c2
          of Card (_,suit) => (case suit
            of Hearts => true | Diamond => true | _ => false))
        | Spades => (case c2
          of Card (_,suit) => (case suit
            of Spades => true | Clubs => true | _ => false))))*)
    (case c1
      of Card (_,suit1) => (case c2
        of Card (_,suit2) => (if (suit1 = Diamond orelse suit1 = Hearts)
            andalso (suit2 = Diamond orelse suit2 = Hearts) then true
          else if (suit1 = Spades orelse suit1 = Clubs)
            andalso (suit2 = Spades orelse suit2 = Clubs) then true
          else false)))

  val _ = Check.expect(sameColor(card2,card4), true, "sameColor(card2,card4)")
  val _ = Check.expect(sameColor(card1,card4), false, "sameColor(card1,card4)")


end
