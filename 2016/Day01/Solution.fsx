module rec Day01 = 
  type Point = {
    x: int
    y: int
  }

  type Direction = NORTH | SOUTH | WEST | EAST

  type Rotation = RIGHT | LEFT

  type Movement = {
    numBlocks: int
    rotation: Rotation
  }
  let getMovements (fileName: string) = 
    let tokens = 
      System.IO.File.ReadLines fileName 
              |> Seq.map (fun line -> line.Split(','))
              |> Seq.head
              |> Seq.toList
              |> List.map (fun x -> x.Trim())
    let createMovement (token: string): Movement =
      let elements = Seq.toList token
      let rotation = 
        match List.head elements with
          'L' -> LEFT
          | _ -> RIGHT 
      let numBlocks = List.tail elements |> List.toArray |> System.String |> int
      {numBlocks = numBlocks; rotation = rotation}
    tokens |> List.map createMovement

  let getNextDirection direction rotation = 
    let turnLeft direction = 
      match direction with 
        NORTH -> WEST
        | WEST -> SOUTH
        | SOUTH -> EAST
        | EAST -> NORTH
    let turnRight direction = 
      match direction with 
        NORTH -> EAST
        | EAST -> SOUTH
        | SOUTH -> WEST
        | WEST -> NORTH
    match rotation with 
      RIGHT -> turnRight direction
      | LEFT -> turnLeft direction

  let getNextPoint point direction numBlocks = 
    match direction with 
      NORTH -> {x = point.x; y = point.y - numBlocks}
      | SOUTH -> {x = point.x; y = point.y + numBlocks}
      | WEST -> {x = point.x - numBlocks; y = point.y}
      | EAST -> {x = point.x + numBlocks; y = point.y}

  let findShortestPath movements = 
    let startingPoint: Point = {x = 0; y = 0}
    let currentDirection = NORTH
    let rec loop currentPoint currentDirection movements = 
      match movements with 
        x::xs -> 
          let nextDirection = getNextDirection currentDirection x.rotation
          let nextPoint = getNextPoint currentPoint nextDirection x.numBlocks
          loop nextPoint nextDirection xs
        | _ -> currentPoint
    let lastPoint = loop {x = 0; y = 0} NORTH movements 
    abs lastPoint.x + abs lastPoint.y
  
printfn "%i" (Day01.findShortestPath (Day01.getMovements "input.txt"))
