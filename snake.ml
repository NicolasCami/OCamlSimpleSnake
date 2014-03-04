(**
 * Very simple snake game.
 * L3 - 2013/2014 - Université de Perpignan Via Domitia
 * @author Nicolas Cami
 * @date April, 2014
 *)

open Graphics;;

exception Lose;;
exception Win;;

Random.self_init ();;
Graphics.open_graph " 640x480";;

let width,height = 640,480;;
let gridSize = 16;;
let cellSize = [|640/gridSize;480/gridSize|];;

type direction = Top | Right | Down | Left;;

type cell = Full | Empty;;

type world = { mutable grid : cell array array ;
               size : int };;

type snake = { mutable pos : (int * int) array ;
             mutable dir : direction };;

(**
 * Make the program sleep for n seconds.
 *)
let sleep n =
	let start = Unix.gettimeofday() in
		let rec delay t =
		try
			ignore (Unix.select [] [] [] t)
			with Unix.Unix_error(Unix.EINTR, _, _) ->
			let now = Unix.gettimeofday() in
				let remaining = start +. n -. now in
					if remaining > 0.0 then delay remaining in
						delay n;;

(**
 * Initialize an empty grid and snake position.
 *)
let initialize size =
	{ grid = Array.make_matrix size size Empty ;
	  size = size },
	{ pos = [|(size/2,size/2);(size/2,size/2+1)|] ;
      dir = Top };;

(**
 * Check if the snake eat hitself.
 *)
let rec bite (x,y) snake i =
	if i = Array.length snake.pos then
		false
	else
		let a,b = snake.pos.(i) in
			if a=x && b=y then
				true
			else
				bite (x,y) (snake) (i+1);;

(**
 * Add an apple to the grid.
 *)
let addApple snake world x y =
	if not (bite (x,y) (snake) (0)) then
		world.grid.(x).(y) <- Full;;

(**
 * Add n random apples to the grid.
 *)
let rec addRandomApple snake world n =
	if n > 0 then
		begin
			addApple snake world (Random.int world.size) (Random.int world.size);
			addRandomApple snake world (n-1);
		end
	else
		();;

(**
 * Display a rectangle.
 *)
let displayCell i j couleur =
    Graphics.set_color couleur;
    Graphics.fill_rect (i*cellSize.(0)) (j*cellSize.(1)) cellSize.(0) cellSize.(1);;

(**
 * Display an apple.
 *)
let displayApple i j =
    Graphics.set_color 0xff0000;
    Graphics.fill_ellipse (i*cellSize.(0)+cellSize.(0)/2) (j*cellSize.(1)+cellSize.(1)/2) (cellSize.(0)/2) (cellSize.(1)/2);
    Graphics.set_color 0x00ff00;
	Graphics.fill_arc (i*cellSize.(0)+cellSize.(0)/2) (j*cellSize.(1)+cellSize.(1)/2) (cellSize.(0)/3) (cellSize.(1)/5) 180 210;
	Graphics.fill_arc (i*cellSize.(0)+cellSize.(0)/2) (j*cellSize.(1)+cellSize.(1)/2) (cellSize.(0)/3) (cellSize.(1)/5) 100 150;;

(**
 * Display the whole grid.
 *)
let displayWorld world =
	for i=0 to world.size-1 do
		for j=0 to world.size-1 do
			if world.grid.(i).(j) = Full then
				begin
					displayCell i j 0xffffff;
					displayApple i j;
				end
			else
				displayCell i j 0xffffff
		done
	done;;

(**
 * Display the snake.
 *)
let displaySnake snake =
	for i=0 to Array.length snake.pos -1 do
		let x,y = snake.pos.(i) in
			if i = Array.length snake.pos -1 then
				displayCell x y 0xaaff44
			else
				displayCell x y 0x55ff55
	done;;

(**
 * Display grid and snake.
 *)
let display snake world =
	displayWorld world;
	displaySnake snake;;

(**
 * Check key pressed and execute related action.
 *)
let action snake k =
	match k with
	'z' -> snake.dir <- Top
	| 'q' -> snake.dir <- Left
	| 's' -> snake.dir <- Down
	| 'd' -> snake.dir <- Right
	| _ -> ();;

(**
 * Check if the snake eat an apple.
 *)
let eatApple world (x,y) =
	if world.grid.(x).(y) = Full then
		begin
			world.grid.(x).(y) <- Empty;
			true;
		end
	else
		false;;

(**
 * Move all snake parts.
 *)
let moveSnake (x,y) dx dy world snake =
	let nx,ny = x+dx,y+dy in
		if(nx<0 || nx>world.size || ny<0 || ny>world.size || bite (nx,ny) (snake) (0)) then
			raise Lose
		else if Array.length snake.pos >= 10 then
			raise Win
		else
			(nx,ny);;

(**
 * Move the snake, and execute actions.
 *)
let run snake world =
	match snake.dir with
		Right ->
			let n = moveSnake snake.pos.(Array.length snake.pos -1) (1) (0) (world) (snake) in
			if eatApple world n then
				Array.append snake.pos [|n|]
			else
				Array.sub (Array.append snake.pos [|n|]) (1) (Array.length snake.pos)
		| Left ->
			let n = moveSnake snake.pos.(Array.length snake.pos -1) (-1) (0) (world) (snake) in
			if eatApple world n then
				Array.append snake.pos [|n|]
			else
				Array.sub (Array.append snake.pos [|n|]) (1) (Array.length snake.pos)
		| Top ->
			let n = moveSnake snake.pos.(Array.length snake.pos -1) (0) (1) (world) (snake) in
			if eatApple world n then
				Array.append snake.pos [|n|]
			else
				Array.sub (Array.append snake.pos [|n|]) (1) (Array.length snake.pos)
		| Down ->
			let n = moveSnake snake.pos.(Array.length snake.pos -1) (0) (-1) (world) (snake) in
			if eatApple world n then
				Array.append snake.pos [|n|]
			else
				Array.sub (Array.append snake.pos [|n|]) (1) (Array.length snake.pos);;

(**
 * Start a new action loop.
 *)
let move snake world =
	snake.pos <- run snake world;;

(**
 * Start the game loops.
 *)
let program snake world fKey =
	while true do
		begin
			sleep 1.0;
			if Graphics.key_pressed() then
				fKey (snake) (Graphics.read_key());
			move snake world;
			display snake world;
		end
	done;;

let world,snake = initialize gridSize;;
addRandomApple snake world 10;;

display snake world;;

program snake world action;;

read_line();;
