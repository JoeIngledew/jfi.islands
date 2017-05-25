module DiamondSquare

open System

//type Point = int * int

let rand = 
    function s -> if s = 0 then System.Random() else System.Random(s)

let randRangei (r : Random) rMin rMax =
    rMin + r.Next() * (rMax - rMin)

let randRanged (r : Random) rMin rMax =
    rMin + r.NextDouble() * (rMax - rMin)

let randRangef (r : Random) rMin rMax =
    rMin + float32 (r.NextDouble()) * (rMax - rMin)

let isPow2 (a : int) =
    let fa = float a
    let log = Math.Log(fa, float 2)
    let pow = Math.Pow(float 2, Math.Round(log))
    pow = fa

let DsGrid size seed rMin rMax noise =
    let s = size - 1
    if not (isPow2 s) || rMin >= rMax then
        None
    else 
        let modNoise = 0.0f
        let grid = Array.create<array<float32>> size (Array.zeroCreate<float32> size)
        let rand = rand seed
        // oh my this is janky mapping
        let grid' =
            grid
            |> Array.mapi (fun ix i -> 
                if ix = 0 || ix = s then
                    i |> Array.mapi (fun iix j -> if iix = 0 || iix = s then (randRangef rand rMin rMax) else j)
                else i)

        let modi =
            function i -> (rMax - rMin) * noise * (float32 i / float32 s)
        let mutable i = s
        let is = [ while i > 1 do yield (i, (modi i)); i <- i/2 ]
        printfn "%A" is
        //let mods = [ for i in is do yield (rMax - rMin) * noise * (float32 i / float32 s) ]

        // Diamonds!
        let rec innerLoopD xCurr yCurr iterator (gridx : float32 [][])=
            let xP = if (xCurr + iterator) > s then (s-1) else (xCurr + iterator)
            let yP = if (yCurr + iterator) > s then (s-1) else (yCurr + iterator)
            if xCurr < s then
                let s0 = gridx.[xCurr].[yCurr]
                let s1 = gridx.[xP].[yCurr]
                let s2 = gridx.[xCurr].[yP]
                let s3 = gridx.[xP].[yP]
                let grid' =
                    gridx
                    |> Array.mapi (fun ix x -> 
                        if ix = (xCurr + (iterator / 2)) then
                            x |> Array.mapi (fun iix y -> 
                                if iix = (yCurr + (iterator / 2)) then 
                                    ((s0 + s1 + s2 + s3) / 4.0f) 
                                    + (randRangef rand (0.0f - (snd (is |> List.find (fun (i,_) -> i = iterator)))) (snd (is |> List.find (fun (i,_) -> i = iterator))))
                                else y)
                        else x)
                innerLoopD (xCurr + 1) yCurr iterator grid'
            else gridx

        let rec loopD yCurr iterator gridd =
            if yCurr < s then
                let grid' = innerLoopD 0 yCurr iterator gridd
                loopD (yCurr + 1) iterator grid'
            else gridd

        let rec innerLoopS xCurr yCurr iterator (gridsx : float32 [][]) =
            let xP = if (xCurr + iterator) > s then (s-1) else (xCurr + iterator)
            let yP = if (yCurr + iterator) > s then (s-1) else (yCurr + iterator)
            let xC = if (xCurr + (iterator / 2)) > s then (s-1) else (xCurr + (iterator / 2))
            let yC = if (yCurr + (iterator / 2)) > s then (s-1) else (yCurr + (iterator / 2))
            let xNC = if (xCurr - (iterator / 2)) < 0 then 0 else (xCurr - (iterator / 2))
            let yNC = if (yCurr - (iterator / 2)) < 0 then 0 else (yCurr - (iterator / 2))
            let xW = if (xCurr + iterator + (iterator / 2)) > s then (s - 1) else (xCurr + iterator + (iterator / 2)) 
            let yW = if (yCurr + iterator + (iterator / 2)) > s then (s - 1) else (yCurr + iterator + (iterator / 2)) 
            if xCurr < s then 
                let s0 = gridsx.[xCurr].[yCurr]
                let s1 = gridsx.[xP].[yCurr]
                let s2 = gridsx.[xCurr].[yP]
                let s3 = gridsx.[xP].[yP]
                let cn = gridsx.[xC].[yC]
                let d0 = if yCurr <= 0 then (s0 + s1 + cn) / 3.0f else (s0 + s1 + cn + gridsx.[xC].[yNC]) / 4.0f
                let d1 = if xCurr <= 0 then (s0 + cn + s2) / 3.0f else (s0 + cn + s2 + gridsx.[xNC].[yC]) / 4.0f
                let d2 = if xCurr >= (s - iterator) then (s1 + cn + s3) / 3.0f else (s1 + cn + s3 + gridsx.[xW].[yC]) / 4.0f
                let d3 = if yCurr >= (s - iterator) then (cn + s2 + s3) / 3.0f else (cn + s2 + s3 + gridsx.[xC].[yW]) / 4.0f
                let gridsx' =
                    gridsx
                    |> Array.mapi (fun ix x -> 
                        if ix = (xCurr + (iterator / 2)) then
                            x |> Array.mapi (fun iix y -> 
                                if iix = (yCurr + iterator) then 
                                    d0 + (randRangef rand (0.0f - (snd (is |> List.find (fun (i,_) -> i = iterator)))) (snd (is |> List.find (fun (i,_) -> i = iterator))))
                                else d1 + (randRangef rand (0.0f - (snd (is |> List.find (fun (i,_) -> i = iterator)))) (snd (is |> List.find (fun (i,_) -> i = iterator)))))
                        elif ix = (xCurr + iterator) then 
                            x |> Array.mapi (fun iix y -> 
                                if iix = (yCurr + (iterator / 2)) then 
                                    d2 + (randRangef rand (0.0f - (snd (is |> List.find (fun (i,_) -> i = iterator)))) (snd (is |> List.find (fun (i,_) -> i = iterator))))
                                else y)
                        else 
                            x |> Array.mapi (fun iix y -> 
                                if iix = (yCurr + (iterator / 2)) then 
                                    d1 + (randRangef rand (0.0f - (snd (is |> List.find (fun (i,_) -> i = iterator)))) (snd (is |> List.find (fun (i,_) -> i = iterator))))
                                else y))
                innerLoopS (xCurr + 1) yCurr iterator gridsx'
            else gridsx

        let rec loopS yCurr iterator grids =
            if yCurr < s then
                let grid' = innerLoopS 0 yCurr iterator grids
                loopS (yCurr + 1) iterator grid'
            else grids
            

        let rec outerLoop acc grido =
            printfn "acc %A grid %A" acc grido
            match acc with 
            | [] -> grido
            | h::t -> 
                let grido' = loopD 0 (fst h) grido
                let grido'' = loopS 0 (fst h) grido'
                outerLoop t grido''

        let res = outerLoop (is |> List.rev) grid
        let min = res |> Array.minBy (fun l -> l |> Array.min) |> Array.min
        let max = res |> Array.maxBy (fun l -> l |> Array.max) |> Array.max
        let normalise = 
            function n -> 
                        let n' = if min < 0.0f then n - min else n
                        let nf = Math.Floor(float ((n' / 256.0f) * max))
                        if nf > 1.0 then nf - 1.0 else nf

        let normalised = res |> Array.map (fun l -> l |> Array.map (fun i -> normalise i))
        Some normalised

        // see https://github.com/eogas/DiamondSquare/blob/master/DiamondSquare/DiamondSquare/DiamondSquare.cs

//let testArr = [1;2;3;]
//
//let testArr' = testArr |> List.mapi (fun ix i -> if ix = 1 then 5 else i) // modifying lists!
//
//type Square = {
//    position : Point
//    tl : double // height @ topleft
//    tr : double
//    bl : double
//    br : double }

