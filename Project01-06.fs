module Project01


// name: Dmytro Kuzmyk
// netID: dkuzmy3
// project 1

//
// iter F L
//
// Applies function F to the elements of L
// 
// Examples: iter iter (fun x -> printfn "%A squared is %A" x (x*x) ) [1; 2] => 1 squared is 1
//                                                                     2 squared is 4
//           iter (fun x -> printf "%c" x) ['t'; 'r'; 'u'; 'e']     => true
//           iter (fun x -> printfn "Iterating...") []              => 
// 
// You may not call List.iter directly in your solution.
// For more information on the behavior of this function you can visit https://msdn.microsoft.com/visualfsharpdocs/conceptual/list.iter%5b%27t%5d-function-%5bfsharp%5d
// 
// 
let iter F L =
    let rec loop F L =
        match L with
        | [] -> []
        | x::rest -> (F x) :: (loop F rest)
    loop F L

[<EntryPoint>]
let main argv =
    iter (fun x -> printf "%c" x) ['t'; 'r'; 'u'; 'e']
    printfn ""
    printfn "Pass if the above outputs true"
    0 // return an integer exit code
