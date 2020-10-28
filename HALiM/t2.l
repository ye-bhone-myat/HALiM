;; HALiM
;; Task 2: The Market Square
;; Ye Bhone Myat

;; Load previous task

( load "t1.l" )

;; ==========

;; Declare constants

;; ==========

;; function open-for-business
;; ==========================
;; Takes no inputs
;; -----
;; Populates the *market-square* list with seven lists
;; containing seven tiles, each list representitive
;; of a row on the Marrakech game board.
;; The function then assigns the neighbor relations
;; to each tile, according to the Marrakech game board specs.
;; 
;; Coordinates start at 1.
;; -----
( defun open-for-business ()
	( setf *market-square* ( fill-grid 7 7 ) )
	( assign-neighbors *market-square* )
)

;; function fill-col
;; ==========
;; Takes the numbers row and col which represent
;; the row of the current list of tile objects
;; being created, and the column of each tile
;; object being created.
;; 
;; tile-position's are 1-indexed
;; Indexing starts from the bottom-left
;; -----
;; Recursively fills the columns of the given row by
;;  instantiating tile objects for each
;; column of the given row and putting them into a list.
;; During instantiation, each tile object
;; is also assigned its tile-position list.
;; 
;; row is only present for assigning tile-position.
;; It is not changed during recursion.
;; -----
;; Returns a list containing one tile object if the
;; value of col is less than or equal to 1. Returns
;; a list containing multiple tile objects otherwise.
;; -----
( defun fill-col ( col row &aux tle )
	( setf tle ( make-instance 'tile :position `( ,col ,row ) ) )
	( if ( <= col 1 )
		( cons tle () )
		( snoc tle ( fill-col ( - col 1 ) row ) )
	)
)
;; ==========

;; function fill-grid
;; ==========
;; Takes the numbers row and col which represent
;; the size of the grid to be created.
;; 
;; tile-position's are 1-indexed.
;; Indexing starts from the bottom-left
;; -----
;; Recursively fills a grid of size row x col
;; with tile-objects. Each tile object is also
;; assigned it's tile-position.
;; 
;; see function fill-col
;; -----
;; Returns a list containing row lists, each
;; of length col which contain tile objects.
;; -----
( defun fill-grid ( col row )
	( cond
		( ( = row 1 )
			( cons ( fill-col col row ) () )
		)
		( t
			( snoc ( fill-col col row ) ( fill-grid col ( - row 1 ) ) )
		)
	)
)
;; ==========

;; function display-grid
;; ==========
;; Takes an accessor for the Tile class
;; -----
;; Displays the accessor values of all the Tile objects
;; in the *market-square*, in a grid arrangement
;; -----
( defun display-grid ( accessor )
	( display-grid-supl ( - ( length *market-square* ) 1 ) accessor )
	( format t "   ~{~d ~}~%" ( iota ( length *market-square* ) ) )
)
;; ==========

;; function display-grid-supl
;; ==========
;; Takes the number of the current row to be displayed, and
;; an accessor for the Tile class
;; -----
;; Displays the accessor values of all the Tile objects
;; in the list that is at the given index (row) in the
;; *market-square* list
;; -----
( defun display-grid-supl ( row accessor )
	( cond
		( ( = row 0 )
			( format t " ~d ~{~a ~}~%" ( + row 1 ) ( mapcar (lambda (x) ( funcall accessor x ) ) (nth 0 *market-square*) ) )
		)
		( t
			( format t " ~d ~{~a ~}~%" ( + row 1 ) ( mapcar (lambda (x) ( funcall accessor x ) ) (nth row *market-square*) ) )
			( display-grid-supl ( - row 1 ) accessor )
		)
	)
)
;; ==========

;; function get-tile
;; ==========
;; Takes the coordinates of the desired tile, along with
;; the list tiles ( which should be a nested list )
;; which contains the desired tile.
;; 
;; The coordinates start from 1
;; -----
;; Returns the desired tile from the list of tiles,
;; if found
;; -----
( defun get-tile ( col row tiles )
	( nth ( - col 1 ) ( nth ( - row 1 ) tiles ) )
)
;; ==========

;; function assign-neighbors
;; ==========
;; Takes the nested list tiles containing
;; tile objects which need to be assigned their
;; neighbors.
;; -----
;; The function assigns the inner cells their
;; neighbors first, then assigns the outer
;; cells.
;; The order of assignment does not matter,
;; and this order was chosen for no particular
;; reason.
;; 
;; Each neighbor is represented as an association list
;; with the car being the direction Assam will face
;; should he travel to the tile.
;; The cdr is the neighboring tile object.
;; 
;; The order of a-lists in the neighbors list of each
;; tile object is representative of the neighbor's 
;; relative position to the tile. The 
;; order is ( N E S W ).
;; -----
( defun assign-neighbors ( tiles )
	( setf col ( length ( nth 0 tiles ) ) )
	( setf row ( length tiles ) )
	( assign-neighbors-inner ( - col 1 ) ( - row 1 ) tiles )
	( assign-neighbors-outer tiles )
)
;; ==========

;; function assign-neighbors-outer
;; ==========
;; Takes the nested list tiles containing
;; tile objects, the members lying on the 'outer
;; edges' of which need their neighbors assigned
;; -----
;; Assigns neighbors to the 'top row' ( list at index 6 ),
;; the 'bottom row' ( list at index 0 ), and the left and
;; right columns ( tiles at indices col,1 and col,5 ).
;; -----
( defun assign-neighbors-outer ( tiles &aux col row )
	( setf col ( length ( nth 0 tiles ) ) )
	( setf row ( length tiles ) )
	( assign-neighbors-top col row tiles )
	( assign-neighbors-btm col 1 tiles )
	( assign-neighbors-left-right ( - row 1 ) tiles )
)
;; ==========

;; function assign-neighbors-top
;; ==========
;; Takes the coordinate of the right-most tile
;; in the top row of the nested list of tile objects,
;; and tiles - the nested list of tile objects.
;; 
;; The coordinates start at 1.
;; -----
;; Assigns neighbors to tiles at the top row ( index 6 )
;; of the tiles nested list.
;; -----
( defun assign-neighbors-top ( col row tiles )
	( cond
		( ( = col ( length tiles ) )
			( setf ( tile-neighbors ( get-tile col row tiles ) ) `(
					( s . ,( get-tile ( - col 1 ) row tiles ) )
					( w . ,( get-tile col ( - row 1 ) tiles ) )
					( s . ,( get-tile col ( - row 1 ) tiles ) )
					( w . ,( get-tile ( - col 1 ) row tiles ) )
				)				
			)
			( assign-neighbors-top ( - col 1 ) row tiles )
		)
		( ( = col 1 )
			( setf ( tile-neighbors ( get-tile 1 row tiles ) ) `(
					( e . ,( get-tile col row tiles ) )
					( e . ,( get-tile ( + col 1 ) row tiles ) )
					( s . ,( get-tile col ( - row 1 ) tiles ) )
					( s . ,( get-tile col row tiles ) )
				)				
			)
		)
		( t
			( if ( = ( mod col 2 ) 0 )
				( setf ( tile-neighbors ( get-tile col row tiles ) ) `(
						( s . ,( get-tile ( + col 1 ) row tiles ) )
						( e . ,( get-tile ( + col 1 ) row tiles ) )
						( s . ,( get-tile col ( - row 1 ) tiles ) )
						( w . ,( get-tile ( - col 1 ) row tiles ) )
					)				
				)
				( setf ( tile-neighbors ( get-tile col row tiles ) ) `(
						( s . ,( get-tile ( - col 1 ) row tiles ) )
						( e . ,( get-tile ( + col 1 ) row tiles ) )
						( s . ,( get-tile col ( - row 1 ) tiles ) )
						( w . ,( get-tile ( - col 1 ) row tiles ) )
					)				
				)	
			)
			( assign-neighbors-top ( - col 1 ) row tiles )
		)
	)
)
;; ==========

;; function assign-neighbors-btm
;; ==========
;; Takes the coordinate of the right-most tile
;; in the bottom row of the nested list of tile objects,
;; and tiles - the nested list of tile objects.
;; 
;; The coordinates start at 1.
;; -----
;; Assigns neighbors to tiles at the bottom row ( index 0 )
;; of the tiles nested list.
;; -----
( defun assign-neighbors-btm ( col row tiles )
	( cond
		( ( = col 1 )
			( setf ( tile-neighbors ( get-tile col row tiles ) ) `(
					( n . ,( get-tile col ( + row 1 ) tiles ) )
					( e . ,( get-tile ( + col 1 ) row tiles ) )
					( n . ,( get-tile ( + col 1 ) row tiles ) )
					( e . ,( get-tile col ( + row 1 ) tiles ) )
				)				
			)
		)
		( ( = col ( length tiles ) )
			( setf ( tile-neighbors ( get-tile col row tiles ) ) `(
					( n . ,( get-tile col ( + row 1 ) tiles ) )
					( n . ,( get-tile col row tiles ) )
					( w . ,( get-tile col row tiles ) )
					( w . ,( get-tile ( - col 1 ) row tiles ) )
				)				
			)
			( assign-neighbors-btm ( - col 1 ) row tiles )
		)
		( t
			( if ( = ( mod col 2 ) 0 )
				( setf ( tile-neighbors ( get-tile col row tiles ) ) `(
						( n . ,( get-tile col ( + row 1 ) tiles ) )
						( e . ,( get-tile ( + col 1 ) row tiles ) )
						( n . ,( get-tile ( - col 1 ) row tiles ) )
						( w . ,( get-tile ( - col 1 ) row tiles ) )
					)
				)
				( setf ( tile-neighbors ( get-tile col row tiles ) ) `(
						( n . ,( get-tile col ( + row 1 ) tiles ) )
						( e . ,( get-tile ( + col 1 ) row tiles ) )
						( n . ,( get-tile ( + col 1 ) row tiles ) )
						( w . ,( get-tile ( - col 1 ) row tiles ) )
					)
				)	
			)
			( assign-neighbors-btm ( - col 1 ) row tiles )
		)
	)
)
;; =====

;; function assign-neighbors-left-right
;; ==========
;; Takes the row ( list of tile objects ) that is
;; below the top row, and tiles - the nested list of tile
;; objects that need their neighbors assigned.
;; 
;; The coordinates start at 1.
;; -----
;; Assigns neighbors to tiles at the left and right columns
;; ( tiles at indices col,1 and col,5 ) of the tiles
;; nested list.
;; -----
( defun assign-neighbors-left-right ( row tiles &aux col )
	( if ( and ( < row ( length tiles ) ) ( > row 1 ) )
		( let ()
			( cond
				( ( = ( mod row 2 ) 0 )
					( setf col 1 )
					( setf ( tile-neighbors ( get-tile col row tiles ) ) `(
							( n . ,( get-tile col ( + row 1 ) tiles ) )
							( e . ,( get-tile ( + col 1 ) row tiles ) )
							( s . ,( get-tile col ( - row 1 ) tiles ) )
							( e . ,( get-tile col ( - row 1 ) tiles ) )
						)
					)
					( setf col ( length tiles ) )
					( setf ( tile-neighbors ( get-tile col row tiles ) ) `(
							( n . ,( get-tile col ( + row 1 ) tiles ) )
							( w . ,( get-tile col ( + row 1 ) tiles ) )
							( s . ,( get-tile col ( - row 1 ) tiles ) )
							( w . ,( get-tile ( - col 1 ) row tiles ) )
						)
					)
				)
				( t
					( setf col 1 )
					( setf ( tile-neighbors ( get-tile col row tiles ) ) `(
							( n . ,( get-tile col ( + row 1 ) tiles ) )
							( e . ,( get-tile ( + col 1 ) row tiles ) )
							( s . ,( get-tile col ( - row 1 ) tiles ) )
							( e . ,( get-tile col ( + row 1 ) tiles ) )
						)
					)
					( setf col ( length tiles ) )
					( setf ( tile-neighbors ( get-tile col row tiles ) ) `(
							( n . ,( get-tile col ( + row 1 ) tiles ) )
							( w . ,( get-tile col ( - row 1 ) tiles ) )
							( s . ,( get-tile col ( - row 1 ) tiles ) )
							( w . ,( get-tile ( - col 1 ) row tiles ) )
						)
					)
				)
			)
			( assign-neighbors-left-right ( - row 1 ) tiles )
		)
	)
)

;; function assign-neighbors-inner-row
;; ==========
;; Takes the coordinates of the top-right-most inner tile
;; and the nested list of tile objects.
;; -----
;; Assigns neighbors to each tile in the list, that are
;; not at the extreme ends of the list
;; ( and ( > index 0 ) ( < index ( length tiles ) ) )
;; -----
( defun assign-neighbors-inner-row ( col row tiles &aux current-tile neighbors )
	( setf current-tile ( get-tile col row tiles ) )
	( setf neighbors `(
			( n . ,( get-tile col ( + row 1 ) tiles ) )
			( e . ,( get-tile ( + col 1 ) row tiles ) )
			( s . ,( get-tile col ( - row 1 ) tiles ) )
			( w . ,( get-tile ( - col 1 ) row tiles ) )
		)
	)
	( cond
		( ( = col 2 )
			( setf ( tile-neighbors current-tile ) neighbors )
			nil
		)
		( t
			( setf ( tile-neighbors current-tile ) neighbors )
			( assign-neighbors-inner-row ( - col 1 ) row tiles ) 
		)
	)
)
;; ==========

;; function assign-neighbors-inner
;; ==========
;; Takes the nested list of tiles objects that
;; need their neighbors assigned.
;; 
;; Coordinates start at 1
;; -----
;; Assigns neighbors to tiles in each row ( list )
;; that are neighter top nor bottom rows
;; -----
( defun assign-neighbors-inner ( col row tiles )
	( cond
		( ( = row 2 )
			( assign-neighbors-inner-row col row tiles ) 
		)
		( t
			( assign-neighbors-inner-row col row tiles ) 
			( assign-neighbors-inner col ( - row 1 ) tiles )
		)
	)
)
;; ==========
