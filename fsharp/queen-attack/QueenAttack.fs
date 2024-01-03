module QueenAttack

let valid x = x > 0 && x < 8

let create (position: int * int) = 
    match position with 
    | x, y when valid(x) && valid(y) -> true
    | _ -> false

let canAttack (queen1: int * int) (queen2: int * int) = 
    fst queen1 = fst queen2 || snd queen1 = snd queen2 || 
    fst queen1 + snd queen1 = fst queen2 + snd queen2 || 
    fst queen1 = snd queen1 && fst queen2 = snd queen2 ||
    snd queen1 - fst queen1 = snd queen2 - fst queen2