module rec Day01 = 
  type Point = {
    x: int
    y: int
  }

  exception NoDuplicatePointFoundException 

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

  let distance point = abs point.x + abs point.y

  let findShortestPath movements = 
    let rec loop currentPoint currentDirection movements = 
      match movements with 
        x::xs -> 
          let nextDirection = getNextDirection currentDirection x.rotation
          let nextPoint = getNextPoint currentPoint nextDirection x.numBlocks
          loop nextPoint nextDirection xs
        | _ -> currentPoint
    let lastPoint = loop {x = 0; y = 0} NORTH movements 
    distance lastPoint

  let findIntersectingPointOnPath (pointsOnPath: seq<Point>) (visitedPoints: Set<Point>): Option<Point> = 
    Seq.fold (fun acc elem -> 
            match acc with
            None -> if Set.contains elem visitedPoints then Some elem else None
           | Some x -> Some x) None pointsOnPath

  let findFirstRepeatingPoint movements = 
    let rec loop currentPoint currentDirection movements visitedPoints = 
      match movements with 
        x::xs -> 
          let nextDirection = getNextDirection currentDirection x.rotation
          let pointsOnPath: seq<Point> = Seq.map (fun num -> getNextPoint currentPoint nextDirection num) (Seq.init x.numBlocks id)       
          let intersectionPoint: Option<Point> = findIntersectingPointOnPath pointsOnPath visitedPoints
          match intersectionPoint with
            Some x -> x
            | None ->
              let nextPoint = getNextPoint currentPoint nextDirection x.numBlocks
              let nextSet = Set.ofSeq (Seq.append visitedPoints pointsOnPath)
              loop nextPoint nextDirection xs nextSet
       | _ -> raise NoDuplicatePointFoundException
    let firstRepeatingPoint = loop {x = 0; y = 0} NORTH movements Set.empty
    distance firstRepeatingPoint

let movements = Day01.getMovements "input.txt"
printfn "%i" (Day01.findShortestPath movements)
printfn "%i" (Day01.findFirstRepeatingPoint movements)