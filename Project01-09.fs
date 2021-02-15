module Project01


// name: Dmytro Kuzmyk
// netID: dkuzmy3
// project 1
//
// flatten L
//
// Flatten a list of lists to a single list
//
// Returns list 
// 
// Examples: 
//          flatten [[]] => []
//          flatten [[1]] => [1]
//          flatten [[1; 2]; [2; 3; 4]] => [1; 2; 2; 3; 4]
//          flatten [['o'; 'n']; [' ']; ['w'; 'i'; 'n'; 'g'; 's']] => ['o'; 'n'; ' '; 'w'; 'i'; 'n'; 'g'; 's']
// 
// You may not call List.concat or similar functions directly in your solution.
// 
// For more information visit https://msdn.microsoft.com/visualfsharpdocs/conceptual/list.concat%5b%27t%5d-function-%5bfsharp%5d
// 
// 

let flatten L = 
    let rec loop ret L =
        match L with
        | [] -> ret
        | x::rest -> loop (ret@x) rest
    loop [] (L)
    
[<EntryPoint>]
let main argv =
    let f1 = flatten [[]]
    if f1 = [] then
        printfn "Passed!"
    else
        printfn "Failed!"
        
    let f2 = flatten [[1]]
    if f2 = [1] then
        printfn "Passed!"
    else
        printfn "Failed!"
        //printfn "%A" f2
        
    let f3 = flatten [[1; 2]; [2; 3; 4]]
    if f3 = [1; 2; 2; 3; 4] then
        printfn "Passed!"
    else
        printfn "Failed!"
        //printfn "%A" f3

    let f4 = flatten [['o'; 'n']; [' ']; ['w'; 'i'; 'n'; 'g'; 's']]
    if f4 = ['o'; 'n'; ' '; 'w'; 'i'; 'n'; 'g'; 's'] then
        printfn "Passed!"
    else
        printfn "Failed!"
        //printfn "%A" f4
    0 // return an integer exit code
