datatype circleState = Unvisited | Visited
datatype tile = Grey | X | Clear | Circle of circleState
type board = tile list list
type position = int * int
type direction = order * order (* first order is row diference, second order is column difference *)
datatype move = One of direction | Two of direction * direction
type state = board * position

fun getTile (r, c) board = List.nth (List.nth (board, r), c)
local
  fun update L i new = List.take (L, i) @ (new :: List.drop (L, i+1))
in
  fun visitCircle (r, c) board = update board r (update (List.nth (board, r)) c (Circle Visited))
end

local
  fun applyOrder LESS n = n-1
    | applyOrder EQUAL n = n
    | applyOrder GREATER n = n+1

  fun applyDirection (r, c) (orderR, orderC) = (applyOrder orderR r, applyOrder orderC c)
in
  (* REQUIRES: given position is valid *)
  fun go (board, position) move =
    let
      val currentDirection =
        case move of
          One d => d
        | Two (d1, d2) => d1
      val nextPosition = applyDirection position currentDirection
      val nextTile = getTile nextPosition board
    in
      case nextTile of
        Clear => go (board, nextPosition) move
      | X => NONE
      | Grey => (
          case move of
            One _ => NONE
          | Two (_, d) => go (board, position) (One d)
        )
      | Circle Visited => NONE
      | Circle Unvisited => (
          case move of
            One _ => SOME (visitCircle nextPosition board, nextPosition)
          | Two _ => NONE
        )
    end handle Subscript => NONE
end

local
  fun plucks [] = []
    | plucks (x::xs) = (x,xs) :: map (fn (x', xs') => (x', x::xs')) (plucks xs)
in
  fun try [] state = [[]]
    | try moves (board, position) =
        List.concat (
          List.mapPartial
            (fn (move, remainingMoves) => Option.map
              (fn state => map (fn L => move::L) (try remainingMoves state))
              (go (board, position) move))
            (plucks moves)
        )
end

local
  fun directionToString (orderR, orderC) =
    let
      val vert = case orderR of
        LESS => "up "
      | GREATER => "down "
      | _ => ""
      val horiz = case orderC of
        LESS => "left"
      | GREATER => "right"
      | _ => ""
    in
      vert ^ horiz
    end

  fun printMove move =
    case move of
      One d => print (directionToString d ^ "\n")
    | Two (d1, d2) => print (directionToString d1 ^ " then " ^ directionToString d2 ^ "\n")
in
  fun printMoves moves = (
    app printMove moves;
    print "~~~~~~~~~~~~~~\n"
  )
end

val board = [
  [Grey, Grey, Grey, Grey, Grey, X, X, Grey, Grey, X, Grey, Grey, X, X, Grey, Grey, Grey, Grey],
  [X, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Grey],
  [X, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Circle Unvisited, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Grey],
  [Grey, Clear, Clear, Clear, Circle Unvisited, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Grey],
  [Grey, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Circle Unvisited, Clear, Clear, Circle Unvisited, Clear, Clear, Clear, Clear, Grey],
  [X, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Grey, Grey, Clear, Clear, Clear, Clear, Clear, Clear, Grey, X],
  [X, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Circle Unvisited, Clear, Clear, Circle Unvisited],
  [X, Clear, Circle Unvisited, Clear, Clear, Circle Unvisited, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Grey, Grey],
  [X, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Grey, Clear, Clear, Clear, Grey],
  [X, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Circle Unvisited, Clear, Clear, Clear, Clear, Circle Unvisited, Clear, Clear, Grey],
  [X, Clear, Grey, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Grey, Circle Unvisited, Clear, Clear, Clear, Clear, Grey],
  [Grey, Clear, Grey, Clear, Clear, Clear, Circle Unvisited, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, X],
  [X, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Clear, Circle Unvisited, Clear, Clear, Clear, Clear, Clear, Clear, Clear, X],
  [Grey, Grey, Grey, Grey, X, X, Grey, Grey, X, Grey, X, X, Grey, Grey, X, Grey, Grey, Grey]
]

local
  val up = (LESS, EQUAL)
  val down_left = (GREATER, LESS)
  val up_left = (LESS, LESS)
  val right = (EQUAL, GREATER)
  val left = (EQUAL, LESS)
  val down = (GREATER, EQUAL)
  val down_right = (GREATER, GREATER)
in
  val moves = [
    Two (up, down_left),
    Two (up_left, right),
    (*One right,*)
    One right,
    One right,
    Two (right, up_left),
    Two (down, right),
    One down_right,
    Two (down_right, up),
    Two (down_right, right),
    Two (down_left, up),
    Two (down_left, up_left),
    One left
  ]
end