open SMLofNJ.Cont

signature ESCAPE =
  sig
    type void
    val coerce : void -> 'a
    val escape : (('1a -> void) -> '1a) -> '1a
  end;

structure Escape : ESCAPE = 
  struct
    datatype void = VOID of void
    fun coerce (VOID v) = coerce v
    fun escape f = callcc (fn k => f (fn x => throw k x))
  end;

signature CONTROL =
  sig
    type ans
    val reset : (unit -> ans) -> ans
    val shift : (('1a -> ans) -> ans) -> '1a
  end;

functor Control (type ans) : CONTROL = 
  struct
    open Escape
    exception MissingReset
    val mk : (ans -> void) ref = ref (fn _ => raise MissingReset)
    fun abort x = coerce (!mk x)

    type ans = ans
    fun reset t = escape (fn k => let val m = !mk in 
      mk := (fn r => (mk := m; k r)); abort (t ()) end)
    fun shift h = escape (fn k => 
      abort (h (fn v => reset (fn () => coerce (k v)))))
  end;

structure IntCtrl  = Control (type ans = int);
structure ListCtrl = Control (type ans = int list);
structure BoolCtrl = Control (type ans = bool);

fun find [x] _     = x
  | find (x::xs) p = if p x then x else find xs p

val sel = fn () => BoolCtrl.shift (fn c => c (find [1,2,3,4,5,6,7,8,9] c))
fun sel_prod s1 s2 = fn () => BoolCtrl.shift (fn c => c (s1 (), s2 ()))

fun select f = fn () => BoolCtrl.shift (fn c => c (f c))

fun reverse []      = []
  | reverse (x::xs) = ListCtrl.shift (fn c => x :: c (reverse xs))

val seln = select (find [0,1,2,3,4,5,6,7,8,9])

fun example1 () = BoolCtrl.reset (fn () => let val a = seln () in (print
  (Int.toString a); a = 4) end)
