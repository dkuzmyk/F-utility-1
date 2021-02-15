module Project01

// name: Dmytro Kuzmyk
// netID: dkuzmy3
// project 1
// slice L start stop
//
// Returns a slice of the list with the specified starting and ending indices (start inclusive and end non-inclusive)
// This function creates a list containing the copied values from the input list between the starting and ending index
// 
// Examples: 
//          slice [1; 2; 3; 4; 5] 0 0 => []
//          slice [1; 2; 3; 4; 5] 0 1 => [1]
//          slice [1; 2; 3; 4; 5] 1 4 => [2; 3; 4]
//          slice [1; 2; 3; 4; 5] 0 5 => [1; 2; 3; 4; 5]
//          slice [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] 6 2 => []
//          
let slice L start stop =
    let rec loop L start stop num =
        match L, start, stop with
        | [], a, b -> []
        | x::rest, a, b -> if num=a && num<b then x::(loop rest (a+1) b (num+1)) else loop rest a b (num+1)
    loop L start stop 0

[<EntryPoint>]
let main argv =
    let t1 = slice [1; 2; 3; 4; 5] 0 0
    if t1 = [] then
        printfn "Passed!"
    else
        printfn "Failed!"
        
    let t2 = slice [1; 2; 3; 4; 5] 0 1
    if t2 = [1] then
        printfn "Passed!"
    else
        printfn "Failed!"
        
    let t3 = slice [1; 2; 3; 4; 5] 1 4
    if t3 = [2; 3; 4] then
        printfn "Passed!"
    else
        printfn "Failed!"
        printfn "%A" t3
        
    let t4 = slice [1; 2; 3; 4; 5] 0 5
    if t4 = [1; 2; 3; 4; 5] then
        printfn "Passed!"
    else
        printfn "Failed!"
        
    let t5 = slice [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] 6 2
    if t5 = [] then
        printfn "Passed!"
    else
        printfn "Failed!"           
        
    0 // return an integer exit code
