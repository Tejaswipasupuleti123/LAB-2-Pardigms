type Coach = { Name: string; FormerPlayer: bool }
type Stats = { Wins: int; Losses: int }
type Team = { Name: string; Coach: Coach; Stats: Stats }

let coach1 = { Name = "Phil Jackson"; FormerPlayer = true }
let coach2 = { Name = "Gregg Popovich"; FormerPlayer = false }
let coach3 = { Name = "Pat Riley"; FormerPlayer = true }
let coach4 = { Name = "Steve Kerr"; FormerPlayer = true }
let coach5 = { Name = "Doc Rivers"; FormerPlayer = true }

let stats1 = { Wins = 1000; Losses = 500 }
let stats2 = { Wins = 1200; Losses = 600 }
let stats3 = { Wins = 800; Losses = 400 }
let stats4 = { Wins = 950; Losses = 450 }
let stats5 = { Wins = 750; Losses = 600 }

let team1 = { Name = "Chicago Bulls"; Coach = coach1; Stats = stats1 }
let team2 = { Name = "San Antonio Spurs"; Coach = coach2; Stats = stats2 }
let team3 = { Name = "Miami Heat"; Coach = coach3; Stats = stats3 }
let team4 = { Name = "Golden State Warriors"; Coach = coach4; Stats = stats4 }
let team5 = { Name = "Los Angeles Clippers"; Coach = coach5; Stats = stats5 }

let teams = [ team1; team2; team3; team4; team5 ]
let successfulTeams = teams |> List.filter (fun team -> team.Stats.Wins > team.Stats.Losses)
let successPercentage team = float team.Stats.Wins / float (team.Stats.Wins + team.Stats.Losses) * 100.0
let successPercentages = teams |> List.map (fun team -> team.Name, successPercentage team)
let uniqueCoachNames = teams |> List.map (fun team -> team.Coach.Name) |> List.distinct
let uniqueTeamNames = teams |> List.map (fun team -> team.Name) |> List.distinct

printfn "Unique Coach Names:"
uniqueCoachNames |> List.iter (printfn "%s")
printfn "\nUnique Team Names:"
uniqueTeamNames |> List.iter (printfn "%s")
printfn "\nSuccess Percentages:"
successPercentages |> List.iter (fun (name, percentage) -> printfn "Team %s: %.2f%%" name percentage)



type Cuisine = Korean | Turkish
type MovieType = Regular | IMAX | DBOX | RegularWithSnacks | IMAXWithSnacks | DBOXWithSnacks
type Activity = BoardGame | Chill | Movie of MovieType | Restaurant of Cuisine | LongDrive of int * float

let calculateBudget activity =
    match activity with
    | BoardGame -> 0.0
    | Chill -> 0.0
    | Movie movieType ->
        match movieType with
        | Regular -> 12.0
        | IMAX -> 17.0
        | DBOX -> 20.0
        | RegularWithSnacks -> 12.0 + 5.0
        | IMAXWithSnacks -> 17.0 + 5.0
        | DBOXWithSnacks -> 20.0 + 5.0
    | Restaurant cuisine ->
        match cuisine with
        | Korean -> 70.0
        | Turkish -> 65.0
    | LongDrive (km, costPerKm) -> float km * costPerKm

let test1 = calculateBudget BoardGame
let test2 = calculateBudget Chill
let test3 = calculateBudget (Movie Regular)
let test4 = calculateBudget (Movie IMAXWithSnacks)
let test5 = calculateBudget (Restaurant Korean)
let test6 = calculateBudget (LongDrive (100, 0.5))

printfn "BoardGame Cost: %.2f CAD" test1
printfn "Chill Cost: %.2f CAD" test2
printfn "Movie Regular Cost: %.2f CAD" test3
printfn "Movie IMAXWithSnacks Cost: %.2f CAD" test4
printfn "Restaurant Korean Cost: %.2f CAD" test5
printfn "LongDrive (100 km, 0.5 CAD/km) Cost: %.2f CAD" test6
