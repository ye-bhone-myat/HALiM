;; HALiM
;; Task 4: The Display Function
;; Ye Bhone Myat

;; Load previous task

( load "t3.l" )

;; ==========

;; Declare constants

( hi-assam )

;; ==========

;; function display
;; ==========
;; Takes nothing
;; -----
;; Displays the game board.
;; The color of the carpet on each tile is displayed,
;; except for the tile Assam is on, which shows 'A'.
;; The heading is displayed above the column Assam
;; is in if he is facing North, and below the column
;; if he is facing South.
;; -----
( defun display ( &aux color a-row a-col )
	( setf a-row ( car ( cdr ( tile-position ( assam-tile *assam* ) ) ) ) )
	( setf a-col ( car ( tile-position ( assam-tile *assam* ) ) ) )
	( setf color 
		( cdr ( assoc ( tile-carpet-color ( assam-tile *assam* ) ) *colors* ) )
	)
	;; show N at the top, if facing North
	;; ~v,,,@a determines the padding
	;; ~:[ ~;N~] shows N or ' '
	( format t "     ~v,,,@a~:[ ~;N~]~%" ( * 2 ( - a-col 1 ) )
		""
		( eq ( car ( assam-heading *assam* ) ) 'n )
	)
	( if ( = ( length *market-square*) a-row )
		( display-supl-spcl ( - ( length *market-square* ) 1 ) )
		( display-supl-nrml ( - ( length *market-square* ) 1 ) )
	)
	;; show S at the bottom, if facing South
	;; ~v,,,@a determines the padding
	;; ~:[ ~;N~] shows N or ' '
	( format t "     ~v,,,@a~:[|~;S~]~%" ( * 2 ( - a-col 1 ) )
		""
		( eq ( car ( assam-heading *assam* ) ) 's )
	)
	;; show coordinates
	( format t "     ~{~d ~}~%~%" ( iota ( length *market-square* ) ) )
	;; textual description of Assam's heading and color of the carpet
	;; underneath him
	( format t "Assam is facing ~a. ~:[He is not on a carpet.~;He is on a ~:*~a carpet.~]~%"
		( cdr ( assam-heading *assam* ) )
		color
	)
)
;; ==========

;; function display-supl-spcl
;; ==========
;; Takes the index of the row which contains
;; the tile that Assam is on.
;; 
;; The row numbers are 0-indexed.
;; -----
;; Displays the row with the tile Assam is on.
;; This includes displaying the coordinate of
;; the row ( starts at 1, not to be mistaken
;; with the INDEX of the row ), Assam's heading
;;  ( right after the coordinate if West, or 
;; after the whole row if East ), then the
;; colors of the tiles in the row. The tile
;; Assam is on is surrounded by "|" to it's
;; left and right ( e.g. |G| for a green tile )
;; -----
( defun display-supl-spcl ( row &aux a-col )
	( setf a-col ( car ( tile-position ( assam-tile *assam* ) ) ) )
	( cond 
		( ( > row -1 )
			( format t " ~d ~:[—~;W~]~{ ~:[ ~:;~:*~a~]~}~a~{~:[ ~:;~:*~a~] ~}~:[ ~;E~]~%" 
				( + row 1 ) 
				( if ( eq ( car ( assam-heading *assam* ) ) 'w ) t nil )
				( mapcar #'tile-carpet-color ( car ( split (nth row *market-square*) ( - a-col 1 ) ) ) )
				( format nil "|~:[ ~:;~:*~a~]|" ( tile-carpet-color ( assam-tile *assam* ) ) )
				( mapcar #'tile-carpet-color ( cdr ( car ( cdr ( split (nth row *market-square*) ( - a-col 1 ) ) ) ) ) ) 
				( if ( eq ( car ( assam-heading *assam* ) ) 'e ) t nil )
			)
			( display-supl-nrml ( - row 1 ) )
		)
	)
)
;; ==========

;; function display-supl-nrml
;; ==========
;; Takes the index of the row to display
;; 
;; The row numbers are 0-indexed.
;; -----
;; Displays the given row.
;; This includes displaying the coordinate of
;; the row ( starts at 1, not to be mistaken
;; with the INDEX of the row ), and the
;; colors of the tiles in the row.
;; -----
( defun display-supl-nrml ( row &aux a-row )
	( setf a-row ( car ( cdr ( tile-position ( assam-tile *assam* ) ) ) ) )
	( cond
		( ( = row a-row )
			( format t " ~d   ~{~:[ ~:;~:*~a~] ~:}~%"
				( + row 1 ) 
				( mapcar #'tile-carpet-color (nth row *market-square*) ) 
			)
			( display-supl-spcl ( - row 1 ) )
		)
		( ( > row -1 )
			( format t " ~d   ~{~:[ ~:;~:*~a~] ~:}~%" 
				( + row 1 ) 
				( mapcar #'tile-carpet-color (nth row *market-square*) ) 
			)
			( display-supl-nrml ( - row 1 ) )
		)
	)
)
