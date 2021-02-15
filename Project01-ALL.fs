#light

module Project01

let length L =
    let rec length L tot =               // define a recursive function inside the called function
       match L with
       | [] -> tot
       | e::tail -> length tail (tot+1)  // for every element in the list call length and add 1 to tot
    length L 0                           //  call the default function

let max L =
    let rec loop L c = 
        match L with
        | [] -> c
        | e::rest -> if e>c then loop rest e else loop rest c
    loop L L.Head

let min L =
    let rec loop L c = 
        match L with
        | [] -> c
        | e::rest -> if e<c then loop rest e else loop rest c
    loop L L.Head

let nth L n =
    let rec loop L n res ret =
        match L with
        | [] -> ret
        | x::rest -> if res = n then x else loop rest n (res+1) ret
    loop L n 0 L.Head

let map F L =
   let rec loop F L = 
     match L with
     | [] -> []
     | x::rest -> (F x):: (loop F rest)
   loop F L

let iter F L =
    let rec loop F L =
        match L with
        | [] -> []
        | x::rest -> (F x) :: (loop F rest)
    loop F L

let reduce F L =
    let rec loop F L =
        match L with
        | [e] -> e
        | x::y::rest -> loop F ((F x y)::rest)
    loop F L

let fold F start L = 
    let rec loop F start L =
        match L with
        | [] -> start
        | [a] -> F start a
        | a::rest -> loop F (F start a) rest
    loop F start L 

let flatten L = 
    let rec loop ret L =
        match L with
        | [] -> ret
        | x::rest -> loop (ret@x) rest
    loop [] (L)

let zip L1 L2 = 
    let rec loop L1 L2 =
        match L1, L2 with
        | [], [] -> []
        | x::rest, y::rest2 -> (x, y) :: (loop rest rest2)         
    loop L1 L2

let unzip L =
    let rec loop L =
        match L with
        | [] -> ([],[])
        | x::rest -> let (a, b) = x
                     let tup = loop rest
                     let (el1, el2) = tup
                     (a::el1,b::el2)
    loop L

let range stop =
    let rec loop stop num =
        match stop with
        | 0 -> []
        | x -> (num+1)::(loop (x-1) (num+1))
    loop stop -1
    
let range2 start stop  =
    let rec loop start stop =
        match start, stop with
        | a, b when a = b -> []
        | a, b -> a::(loop (a+1) b)
    loop start stop
    
let range3 start stop step =
    let rec loop start stop step =
        match start, stop with
        | a, b when a = b -> [] 
        | a, b when a < b && step < 0 -> []
        | a, b -> a::(loop (a+step) b step)
    loop start stop step
    
let slice L start stop =
    let rec loop L start stop num =
        match L, start, stop with
        | [], a, b -> []
        | x::rest, a, b -> if num=a && num<b then x::(loop rest (a+1) b (num+1)) else loop rest a b (num+1)
    loop L start stop 0
    
let rec filter F L = 
    let rec loop F L =
        match L with
        | [] -> []
        | x::rest -> if (F x) = true then x::(loop (F) (rest)) else loop F rest
    loop F L
