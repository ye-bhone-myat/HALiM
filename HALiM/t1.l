;; HALiM
;; Task 1: The Tile Object
;; Ye Bhone Myat
;; rev 12-03-2019-13:45

;; Load supporting functions
;; and declare constants

( load "lp.l" )

( setf *directions* '( ( n . "north" ) ( e . "east" ) ( s . "south" ) ( w . "west" ) ) )

( setf *colors* '( ( R . "red" ) ( G . "green" ) ( B . "blue" ) ( Y . "yellow" ) ) )

( setf *verbose* nil )

( setf *debug* nil )

;; ==========


;; === Tile class ===

( defclass tile ()
	(
		( neighbors
			:accessor tile-neighbors
			:initarg :neighbors
		)
		( carpet-color
			:accessor tile-carpet-color
			:initform nil
		)
		( buddy-tile
			:accessor tile-buddy
			:initform nil
		)
		( position
			:accessor tile-position
			:initarg :position
		)
	)
)

;; ==========


;; method get-neighbor-pairlis
;; ==============
;; Takes a tile object tl and a direction ( see *direction* )
;;-----
;; Returns the pair-list ( direction . tile ) from tl's
;; neighbors alist
;;-----
( defmethod get-neighbor-pairlis ( ( tl tile ) direction &aux d-index )
	( setf d-index ( position direction ( mapcar #'car *directions* ) ) )
	( nth d-index ( tile-neighbors tl ) )
)

;; method project
;; ==============
;; Takes a tile object t, the direction of travel ( see *directions* )
;; and a number n as parameters
;;-----
;; Returns the a-list containing the new heading, and the
;; nth tile in the given direction.
;;-----
( defmethod project ( ( tl tile ) direction n &aux neighbor-dir-pairlis neighbor new-direction )
	( cond
		( ( = n 0 ) `( ,direction . ,tl ) )
		( t
			( setf neighbor-dir-pairlis ( get-neighbor-pairlis tl direction ) )
			( setf neighbor ( cdr neighbor-dir-pairlis ) )
			( setf new-direction ( car neighbor-dir-pairlis ) )
			( project neighbor new-direction ( - n 1 ) )
		)
	)
	
)
;; ==========

;; method carpet-buddy
;; ===================
;; Takes two tile objects and a color ( see *colors* )
;;-----
;; If the two tiles are adjacent to each other,
;; 	each tile will be set as the other tile's :tile-buddy
;; otherwise, nothing will happen.
;;-----
( defmethod carpet-buddy ( ( t1 tile ) ( t2 tile ) color )
	( cond
		;; if the tiles are equal, do nothing
		( ( equal ( tile-position t1 ) ( tile-position t2 ) )
			( format t "t1: ~A, t2: ~A, color: ~A~%" ( tile-position t1 ) ( tile-position t2 ) color )
			( format t "Can't place a carpet that way~%" )
			nil
		)
		;; all other cases, set each other as tile-buddies
		( t
			;; set carpet colors
			( setf ( tile-carpet-color t1 ) color )
			( setf ( tile-carpet-color t2 ) color )
			;; set tile-buddies
			( if ( tile-buddy t1 )
				( setf ( tile-buddy ( tile-buddy t1 ) ) nil )
			)
			( if ( tile-buddy t2 )
				( setf ( tile-buddy ( tile-buddy t2 ) ) nil )
			)
			( setf ( tile-buddy t1 ) t2 )
			( setf ( tile-buddy t2 ) t1 )
			t
;;			( format t "~@(~A carpet placed on tiles (~A, ~A) and (~A, ~A).~)~%~%" 
;;				( cdr (assoc color *colors* ) )
;;				( car ( tile-position t1 ) ) 
;;				( cadr ( tile-position t1 ) )
;;				( car ( tile-position t2 ) )
;;				( cadr ( tile-position t2 ) )
;;			)
		)
	)
)
;; ==========


;; method carpet-place
;; ===================
;; Takes a tile object tl, a direction ( see *directions* ) ,
;; and a color ( see *colors* )
;;-----
;; Places a tile onto the tiles tl and the tile that lies towards
;; 'direction'.
;;-----
;; Returns true only if a carpet was placed
;; -----
( defmethod carpet-place ( ( tl tile ) direction color &aux neighbor )
	( setf neighbor ( cdr ( get-neighbor-pairlis tl direction ) ) )
	( if ( not ( carpet-buddy tl neighbor color ) )
		( format t "tile: ~A, direction: ~A, color: ~A~%" ( tile-position tl ) direction color )
	)
	;; ( display )
)
;; ==========

;; method count-contiguous-carpets-supl
;; ====================
;; Takes a tile, a color ( see *colors* ), and the list - contiguous - of
;; contiguous tiles with the same color
;; -----
;; Returns the list of contiguous tiles with color 'color'
;; -----
( defmethod count-contiguous-carpets-supl ( ( tl tile )  color contiguous &aux neighbors )
	(cond
		( ( null color )
			contiguous
		)
		( ( not ( equal color ( tile-carpet-color tl ) ) )
			contiguous
		)
		( ( find tl contiguous )
			contiguous
		)
		( t
			( setf neighbors ( mapcar #'cdr ( tile-neighbors tl ) ) )
			( setf c1 ( count-contiguous-carpets-supl ( nth 0 neighbors ) color ( cons tl contiguous ) ) )
			( setf c2 ( count-contiguous-carpets-supl ( nth 1 neighbors ) color c1 ) )
			( setf c3 ( count-contiguous-carpets-supl ( nth 2 neighbors ) color c2 ) )
			( setf c4 ( count-contiguous-carpets-supl ( nth 3 neighbors ) color c3 ) )
			c4
		)
	)
)
;; ==========

;; method count-contiguous-carpets
;; ====================
;; Takes a tile, and a color (see *colors* )
;; -----
;; Returns the number of contiguous tiles with color 'color'
;; -----
( defmethod count-contiguous-carpets ( ( tl tile ) color )
	( length ( count-contiguous-carpets-supl tl color () ) )
)
;; ==========

( defun contiguous-tiles ( ( tl tile ) color )
	( count-contiguous-carpets-supl tl color () )
)
;; ==========

;; function count-tiles
;; ==========
;; Takes a nested list containing tiles,
;; and the color to count for.
;; -----
;; Returns the number of tiles that have
;; the specified color in the given nested list.
;; 
;; Note that the function counts the tiles, not
;; carpets.
;; -----
( defun count-tiles ( color lst )
	( sum ( mapcar 
		( lambda (y)
			( count t ( mapcar 
						( lambda (x)
							( eq color ( tile-carpet-color x ) )
						)
						  y
						)
			) 
		)
		lst
		)
	)
)
;; ==========
