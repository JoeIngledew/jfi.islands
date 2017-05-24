module DiamondSquare

open System

type Point = int * int

let rand = 
    function s -> if s = 0 then System.Random() else System.Random(s)

let randRangei (r : Random) rMin rMax =
    rMin + r.Next() * (rMax - rMin)

let randRanged (r : Random) rMin rMax =
    rMin + r.NextDouble() * (rMax - rMin)

let randRangef (r : Random) rMin rMax =
    rMin + float32 (r.NextDouble()) * (rMax - rMin)

let isPow2 (a : int) =
    (a &&& (a - 1)) = 0

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
        
        // and some nasty mutable temp vars
//        let mutable s0,s1,s2,s3,d0,d1,d2,d3,cn = 
//            Unchecked.defaultof<float32>,
//            Unchecked.defaultof<float32>,
//            Unchecked.defaultof<float32>,
//            Unchecked.defaultof<float32>,
//            Unchecked.defaultof<float32>,
//            Unchecked.defaultof<float32>,
//            Unchecked.defaultof<float32>,
//            Unchecked.defaultof<float32>,
//            Unchecked.defaultof<float32>

        let mutable i = s
        let is = seq { while i > 1 do yield i; i <- i/2 }
        let mods = [ for i in is do yield (rMax - rMin) * noise * (float32 i / float32 s) ]

        // Diamonds!
        let rec innerLoop xCurr yCurr iterator (gridx : float32 [][])=
            if xCurr < s then
                let s0 = gridx.[xCurr].[yCurr]
                let s1 = gridx.[xCurr + iterator].[yCurr]
                let s2 = gridx.[xCurr].[yCurr + iterator]
                let s3 = gridx.[xCurr + iterator].[yCurr + iterator]
                let grid' =
                    gridx
                    |> Array.mapi (fun ix x -> 
                        if ix = (xCurr + (iterator / 2)) then
                            x |> Array.mapi (fun iix y -> if iix = (yCurr + (iterator / 2)) then ((s0 + s1 + s2 + s3) / 4.0f) + (randRangef rand (0.0f - mods.[iterator]) mods.[iterator]) else y)
                        else x)
                innerLoop (xCurr + 1) yCurr iterator grid'
            else gridx

        let rec loopD yCurr iterator gridd =
            if yCurr < s then
                let grid' = innerLoop 0 yCurr iterator gridd
                loopD (yCurr + 1) iterator grid'
            else gridd

        // see https://github.com/eogas/DiamondSquare/blob/master/DiamondSquare/DiamondSquare/DiamondSquare.cs
        // just so it's valid...
        let a = "b"
        Some a
        

let testArr = [1;2;3;]

let testArr' = testArr |> List.mapi (fun ix i -> if ix = 1 then 5 else i) // modifying lists!

type Square = {
    position : Point
    tl : double // height @ topleft
    tr : double
    bl : double
    br : double }

