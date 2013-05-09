fun seq a b = if a = b then [b] else a :: seq (a + 1) b

fun find [x]     _ = x
  | find (x::xs) p = if p x then x else find xs p
val findNat = find (seq 1 100)

fun select s p k = let val v = s p in k v end
fun selNat k = select findNat k

val example1 = selNat (fn x => x = 3) (fn v => v)

fun predicate1 x y = x + y = 10
val example2 = selNat (fn x => selNat (predicate1 x) (fn y => predicate1 x y)) (fn x => selNat (predicate1 x) (fn y => (x, y)))

(* Przydałyby się RankNTypes... *)
fun composeSel sel1 sel2 sel2' pred =
  sel1 (fn x => sel2 (pred x) (fn y => pred x y)) (fn x => sel2' (pred x) (fn y => [x, y]))

fun compNat pred k = k (selNat (fn x => selNat (pred x) (fn y => pred x y)) (fn x => selNat (pred x) (fn y => [x,y])))
fun compNat' pred k = k (composeSel selNat selNat selNat pred)
val example3 = compNat predicate1 (fn v => v)
val example3' = compNat' predicate1 (fn v => v)
