module Project01

//
// name: Dmytro Kuzmyk
// netID: dkuzmy3
// project 1

// unzip L
//
// Unzip a list of pairs to a pair of lists
//
// Returns tuple of lists
// 
// Examples: 
//          unzip [] => ([], [])
//          unzip [(1, 3); (2, 56); (40, 6)] => ([1; 2; 40], [3; 56; 6])
//          unzip [(1, 'a'); (2, 'b'); (3, 'c')] => ([1; 2; 3], ['a'; 'b'; 'c'])
//
// You may not call List.unzip directly in your solution.
//
// For more information visit https://msdn.microsoft.com/visualfsharpdocs/conceptual/list.unzip%5b%27t1%2c%27t2%5d-function-%5bfsharp%5d
// 
let unzip L =
    let rec loop L =
        match L with
        | [] -> ([],[])
        | x::rest -> let (a, b) = x
                     let tup = loop rest
                     let (el1, el2) = tup
                     (a::el1,b::el2)
    loop L

[<EntryPoint>]
let main argv =
    let u11,u12 = unzip []
    if (u11 = []) && (u12 = []) then
        printfn "Passed!"
    else
        printfn "Failed!"
        
    let u21,u22 = unzip [(1, 3); (2, 56); (40, 6)]
    if (u21 = [1; 2; 40]) && (u22 = [3; 56; 6]) then
        printfn "Passed!"
    else
        printfn "Failed!"

    let u31,u32 = unzip [(1, 'a'); (2, 'b'); (3, 'c')]
    if (u31 = [1; 2; 3]) && (u32 = ['a'; 'b'; 'c']) then
        printfn "Passed!"
    else
        printfn "Failed!"
        
    0 // return an integer exit code
