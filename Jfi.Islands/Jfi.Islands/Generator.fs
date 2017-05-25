module Generator 

//let roughness = 4
//let gridSize = 257
//
//let waterThreshold = 35
//let sandThreshold = 44
//let grassThreshold = 84
//let stoneThreshold = 94

let gen size seed =
    let ds = DiamondSquare.DsGrid size seed 0.0f 100.0f (4.0f / 10.0f)
    ds