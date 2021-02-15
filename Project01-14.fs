module Project01

//
// name: Dmytro Kuzmyk
// netID: dkuzmy3
// project 1

// range3 start stop step
//
// Returns the a list of integers over the range from start as the lower limit (inclusive) to stop as the upper limit (non-inclusive), incrementing by the amount specified in step 
// 
// Examples: 
//          range3 0 0 1 => []
//          range3 0 2 1 => [0; 1]
//          range3 1 5 2 => [1; 3]
//          range3 5 -2 -3 => [5; 2; -1]
//          

let range3 start stop step =
    let rec loop start stop step =
        match start, stop with
        | a, b when a = b -> [] 
        | a, b when a < b && step < 0 -> []
        | a, b -> a::(loop (a+step) b step)
    loop start stop step


[<EntryPoint>]
let main argv =
    let d1 = range3 0 0 1
    if d1 = [] then
        printfn "Passed!"
    else
        printfn "Failed!"

    let d2 = range3 0 2 1
    if d2 = [0; 1] then
        printfn "Passed!"
    else
        printfn "Failed!"
        
    let d3 = range3 1 5 2
    if d3 = [1; 3] then
        printfn "Passed!"
    else
        printfn "Failed!"

    let d4 = range3 5 -2 -3
    if d4 = [5; 2; -1] then
        printfn "Passed!"
    else
        printfn "Failed!"
        //printfn "%A" d4
        
    0 // return an integer exit code
