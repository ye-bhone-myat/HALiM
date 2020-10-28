;; HALiM
;; Task 7: Rules Representation
;; Ye Bhone Myat

;; Load previous task
( load "t6.l" )
;; ==========

;; Declare constants
( setf *turn* 0 )
;; ==========

;; function turnable-p
;; ==========
( defun turnable-p ( direction &aux ind )
	( setf ind ( position direction ( mapcar #'car *directions* ) ) )
	( if ( null ind )
		nil
		( not ( eq 
			( car ( assam-heading *assam* ) )
			( nth ( mod ( + 2 ind ) 4 ) ( mapcar #'car *directions* ) ) ) 
		)
	)
)
;; ==========

;; function remove-opposite
;; ==========
;; Takes a direction ( see *directions* )
;; and a list directions, which should always
;; be the list *directions*
;; -----
;; Removes the direction that is the opposite
;; of the given direction from the list
;; of directions
;; -----
( defun remove-opposite ( direction directions &aux ind)
	( setf ind ( position direction directions ) )
	( kick ( nth ( mod ( + 2 ind ) ( length directions ) ) directions ) directions )
)
;; ==========

;; function carpet-placeables
;; ==========
;; Takes a target tile and returns
;; a list of its neighbor tiles upon
;; which a tile could be placed legally
;; -----
( defun carpet-placeables ( ( target tile ) &aux raw )
	( setf raw ( mapcar 
		( lambda ( x )
			( cond
				( ( equal ( assam-tile *assam* ) ( cdr x ) )
					nil
				)
				( ( equal target ( cdr x ) )
					nil
				)
				( t
					( cond
						( ( not ( equal ( position x ( tile-neighbors target ) ) ( position ( car x ) ( mapcar #'car *directions* ) ) ) )
							nil
						)
						( t
							x
						)		
					)
				)
			)
		)
		( tile-neighbors target )
	) )
	( filter-out 'null raw )
)
;; ==========

;; function carpet-orientables
;; ==========
;; Takes a target tile
;; -----
;; Returns a list of tiles which are
;; adjacent to the target tile, such that
;; a carpet can be placed on the target tile
;; and an adjacent tile
;; -----
( defun carpet-orientables ( ( target tile ) )
	( filter-out 
		;; filters out any tiles that are tile buddies
		;; with the target tile (i.e. if a tile and the
		;; target tile carry the same carpet, that tile
		;; gets filtered out, because rule reasons)
		( lambda ( x )
			( equal target ( tile-buddy ( cdr x ) ) )
		)
		;; filters out any non-placeable neighboring tiles
		( filter-out 'null ( carpet-placeables target ) )
	)
)
;; ==========

;; generic method placeable-p
;; ==========
( defgeneric placeable-p ( p color direction orient ) )
;; ==========

;; method placeable-p [human]
;; ==========
;; Takes a human player, a color,
;; a direction, and an orientation
;; -----
;; 
( defmethod placeable-p ( ( p human ) color direction orient &aux placeables )
	( setf placeables ( carpet-placeables ( assam-tile *assam* ) ) )
	( cond
		;; checks for input format errors
		( ( null ( assoc color *colors* ) )
			( format t "!~A is not a recognized color. Try one of ~A!~%~%" color ( mapcar #'car *colors* ) )
			nil
		)
		( ( null ( assoc direction *directions* ) )
			( format t "!~A is not a recognized direction. Try one of ~A!~%~%" direction ( mapcar #'car *directions* ) )
			nil
		)
		( ( null ( assoc orient *directions* ) )
			( format t "!~A is not a recognized direction. Try one of ~A!~%~%" orient ( mapcar #'car *directions* ) )
			nil
		)
		;; end of checks for input format errors
		;; checks for player's carpet inventory
		( ( null ( assoc color ( player-carpets p ) ) )
			( format t "!~A does not have a ~A carpet!~%~%"
				( player-name p )
				( cdr ( assoc color *colors* ) )
			)
			nil
		)
		( ( = 0 ( cdr ( assoc color ( player-carpets p ) ) ) )
			( format t "!~A does not have any more ~A carpets!~%~%"
				( player-name p )
				( cdr ( assoc color *colors* ) )
			)
			nil
		)
		;; end of checks for player's carpet inventory
		;; checks for illegal placements
		( t
			( cond
				( ( null ( position direction ( mapcar #'car placeables ) ) )
					( format t "!Can't place a carpet to the ~:(~A~) of Assam!~%" ( cdr ( assoc direction *directions* ) ) )
				)
				( ( null ( assoc orient ( carpet-orientables ( cdr ( assoc direction placeables ) ) ) ) )
					( format t "!Can't orient the carpet towards the ~:(~A~) if you place it to the ~:*~(~A~) of Assam!~%" ( cdr ( assoc orient *directions* ) ) ( cdr ( assoc direction *directions* ) ) )
				)
				( t t )
			)
		)
	)
)

( defmethod placeable-p ( ( p computer ) color direction orient )
	( cond
		;; checks for player's carpet inventory
		( ( null ( assoc color ( player-carpets p ) ) )
			nil
		)
		( ( = 0 ( cdr ( assoc color ( player-carpets p ) ) ) )
			nil
		)
		;; end of checks for player's carpet inventory
		;; checks for illegal placements
		( t
			( cond
				( ( null ( position direction ( mapcar #'car placeables ) ) )
					nil
				)
				( ( null ( assoc direction ( mapcar #'carpet-placeables ( mapcar #'cdr placeables ) ) ) )
					nil
				)
				( t t )
			)
		)
	)
)

( defun game-over-p ( ( p1 player ) ( p2 player ) )
	( or
		( >= 0
			( sum
				( mapcar
					( lambda ( ( x player ) )
						( sum
							( mapcar #'cdr ( player-carpets x ) )
						)
					)
					( list p1 p2 )
				)
			)
		)
		( or
			( <= ( player-coins p1 ) 0 )
			( <= ( player-coins p2 ) 0 )
		)
	)
)

( defun count-score ( ( p player ) &aux coverage )
	( setf coverage
		( sum
			( mapcar
				( lambda (color)
					( count-tiles color *market-square* )
				)
				( mapcar #'car ( player-carpets p ) )
			)
		)
	)
	( + ( player-coins p ) coverage )
)

( defun choose-player ( p &aux str pl )
	( setf str ( format nil "~(~A~)" p ) )
	( cond
		( ( string= str "human" )
			( make-instance 'human :carpets nil )
		)
		( ( string= str "halim" )
			( make-instance 'halim )
		)
		( ( string= str "randa" )
			( make-instance 'randa )
		)	
	)
)

( defun choose-name ( ( p human ) &aux name )
	( format t "Please enter the player's name~%" )
	( setf name ( read-line ) )
	( if ( not ( null name ) ) ( setf ( player-name p ) name ) )
)

( defun choose-players ( &aux usr )
	( format t "Choose Player 1 (human, halim, or randa) ~%" )
	( setf usr (read) )
	( setf *player1* ( choose-player usr ) )
	( loop while ( null *player1* )
		do ( format t "~@(~A~) is not a valid player choice.~%" *player1* )
		do ( format t "Choose Player 1 (human, halim, or randa) ~%" )
		do ( setf usr (read-line) )
		do ( setf *player1* ( choose-player usr ) )
	)
	( setf ( player-carpets *player1* ) `( ( R . ,*n-carpets* ) ( G . ,*n-carpets* ) ) )
	( if ( string= ( format nil "~(~A~)" usr ) "human" ) ( choose-name *player1* ) 	
		( setf ( player-name *player1* ) ( concatenate 'string ( player-name *player1* ) "1" ) )
	)
	( display-player *player1* )
	( terpri )
	( format t "Choose Player 2 (human, halim, or randa) ~%" )
	( setf usr (read-line) )
	( setf *player2* ( choose-player usr ) )
	( loop while ( null *player2* )
		do ( format t "~@(~A~) is not a valid player choice.~%" *player2* )
		do ( format t "Choose Player 2 (human, halim, or randa) ~%" )
		do ( setf usr (read-line) )
		do ( setf *player2* ( choose-player usr ) )
	)
	( setf ( player-carpets *player2* ) `( ( B . ,*n-carpets* ) ( Y . ,*n-carpets* ) ) )
	( if ( string= ( format nil "~(~A~)" usr ) "human" ) ( choose-name *player2* )
		( setf ( player-name *player2* ) ( concatenate 'string ( player-name *player2* ) "2" ) )
	)
	( display-player *player2* )
	( terpri )
	( setf *players* ( list *player1* *player2* ) )
	( display )
)

( defun resolve-win ( &aux score )
	( setf score ( mapcar #'count-score *players* ) )
	( cond
		( ( > ( nth 0 score ) ( nth 1 score ) )
			( setf ( player-stats ( nth 0 *players* ) ) ( + 1 ( player-stats ( nth 0 *players* ) ) ) )
			( nth 0 *players* )
		)
		( ( < ( nth 0 score ) ( nth 1 score ) )
			( setf ( player-stats ( nth 1 *players* ) ) ( + 1 ( player-stats ( nth 1 *players* ) ) ) )
			( nth 1 *players* )
		)
		( t
			nil
		)
		
	)
)

( defun resolve-payments ( &aux assam-color )
	( setf assam-color ( tile-carpet-color ( assam-tile *assam* ) ) )
	( if ( not ( null assam-color ) )
		( if
			( null ( assoc assam-color ( player-carpets ( nth *turn* *players* ) ) ) )
			( pay-up
				( nth *turn* *players* )
				( nth ( mod ( + 1 *turn* ) ( length *players* ) ) *players* )
				( count-contiguous-carpets ( assam-tile *assam* ) assam-color )
			)
			nil
		)
	)
)

( defun make-move ( roll )
	( assam-move roll *assam* )
	
)
( defgeneric play ( p1 p2 ) )

( defmethod play ( ( p1 human ) ( p2 halim ) &aux user-in current-player )
	( open-for-business )
	( hi-assam )
	( format t "Choose Assam's initial heading (N, E, S, or W)~%" )
	( setf user-in ( read ) )
	( loop while ( null ( assoc user-in *directions* ) )
		do ( format t "~A is not a recognized direction. Try one of ~A.~%~%" user-in ( mapcar #'car *directions* ) )
		do ( setf user-in ( read ) )
	)
	( setf ( assam-heading *assam* ) ( assoc user-in *directions* ) )
	( loop while ( not ( game-over-p *player1* *player2* ) )
		do ( setf current-player ( nth *turn* *players* ) )
		do ( display-player current-player )
		do ( display )
		do ( format t "> ~@(~A~)'s turn.~%" ( player-name current-player ) )
		do ( handle-turn current-player )
		do ( make-move ( roll ) )
		do ( resolve-payments )
		do ( display-player current-player )
		do ( display )
		do ( handle-placement current-player )
		do ( setf *turn* ( mod ( + 1 *turn* ) ( length *players* ) ) )
	)
	( setf current-player ( resolve-win ) )
	( if (null current-player)
		( format t "The game was a draw!~%" )
		( format t "~:(~A~) won!~%" ( player-name current-player ) )
	)
	( if ( equal current-player *player2* )
		( format t "Git Gud, human~%" )
		( let ()
			( format t "gitting gud~%" )
			( rotate-play-modes ( halim-game-phase *player2* ) ( halim-finances *player2* ) *path-kb* )
			( rotate-play-modes ( halim-game-phase *player2* ) ( halim-finances *player2* ) *placement-kb* )
			nil
		)
	)
)

( defmethod play ( ( p1 human ) ( p2 player ) &aux user-in current-player )
	( open-for-business )
	( hi-assam )
	( format t "Choose Assam's initial heading (N, E, S, or W)~%" )
	( setf user-in ( read ) )
	( loop while ( null ( assoc user-in *directions* ) )
		do ( format t "~A is not a recognized direction. Try one of ~A.~%~%" user-in ( mapcar #'car *directions* ) )
		do ( setf user-in ( read ) )
	)
	( setf ( assam-heading *assam* ) ( assoc user-in *directions* ) )
	( loop while ( not ( game-over-p *player1* *player2* ) )
		do ( setf current-player ( nth *turn* *players* ) )
		do ( display-player current-player )
		do ( display )
		do ( format t "> ~@(~A~)'s turn.~%" ( player-name current-player ) )
		do ( handle-turn current-player )
		do ( make-move ( roll ) )
		do ( resolve-payments )
		do ( display-player current-player )
		do ( display )
		do ( handle-placement current-player )
		do ( setf *turn* ( mod ( + 1 *turn* ) ( length *players* ) ) )
	)
	( setf current-player ( resolve-win ) )
	( if (null current-player)
		( format t "The game was a draw!~%" )
		( format t "~:(~A~) won!~%" ( player-name current-player ) )
	)
)

( defmethod play ( ( p1 computer ) ( p2 computer ) &aux user-in current-player )
	( loop while ( not ( game-over-p *player1* *player2* ) )
		do ( setf current-player ( nth *turn* *players* ) )
		do ( if *verbose* ( let ()
					( format t "> ~@(~A~)'s turn.~%" ( player-name current-player ) )
					( display-player current-player )
					( display )
				)
			)
		do ( handle-turn current-player )
		do ( make-move ( roll ) )
		do ( resolve-payments )
		do ( if *verbose* ( let () ( display-player current-player ) ( display ) ) )
		do ( handle-placement current-player )
		do ( setf *turn* ( mod ( + 1 *turn* ) ( length *players* ) ) )
	)
	( setf current-player ( resolve-win ) )
	( if *verbose* ( let ()
			( if (null current-player )
				( format t "The game was a draw!~%" )
				( format t "~:(~A~) won!~%" ( player-name current-player ) )
			)
			( mapcar #'display-player *players* )
			( display )
		)
	)
	current-player
)

( defun make-game ( verbose )
	( setf *verbose* verbose )
	( choose-players )
	( open-for-business )
	( hi-assam )
	( play *player1* *player2* )
	( setf *turn* 0 )
	( open-for-business )
	( hi-assam )
)

( defun play-for ( rounds )
	( choose-players )
	( loop for i from 1 to rounds
		do ( if *verbose ( format t "Round ~A~%" i ) )
		do ( open-for-business )
		do ( hi-assam )
		do ( mapcar ( lambda (x) ( setf ( player-coins x ) 30 ) ) *players* )
		do ( setf ( player-carpets *player1* ) `( ( R . ,*n-carpets* ) ( G . ,*n-carpets* ) ) )
		do ( setf ( player-carpets *player2* ) `( ( B . ,*n-carpets* ) ( Y . ,*n-carpets* ) ) )
		do ( play *player1* *player2* )
	)
	( format t "Statistics:~%==========~%~A win rate: ~,2f%~%~A win rate: ~,2f%~%Draws: ~,2f%" 
		( player-name *player1* )
		( * ( / ( player-stats *player1* ) rounds ) 100 )
		( player-name *player2* )
		( * ( / ( player-stats *player2* ) rounds ) 100 )
		( * ( / ( - rounds ( sum ( mapcar #'player-stats *players* ) ) ) rounds ) 100 )
	)
)

( defun train ( rounds &aux winner v )
	( setf v *verbose* )
	( setf *verbose* nil )
	( setf *player1* ( make-instance 'randa :carpets `( ( R . ,*n-carpets* ) ( G . ,*n-carpets* ) ) ) )
	( setf *player2* ( make-instance 'halim :carpets `( ( b . ,*n-carpets* ) ( y . ,*n-carpets* ) ) ) )
	( setf *players* ( list *player1* *player2* ) )
	( loop for i from 1 to rounds
		do ( if *verbose* ( format t "Round ~A~%" i ) )
		do ( open-for-business )
		do ( hi-assam )
		do ( mapcar ( lambda (x) ( setf ( player-coins x ) 30 ) ) *players* )
		do ( setf ( player-carpets *player1* ) `( ( R . ,*n-carpets* ) ( G . ,*n-carpets* ) ) )
		do ( setf ( player-carpets *player2* ) `( ( B . ,*n-carpets* ) ( Y . ,*n-carpets* ) ) )
		do ( setf winner ( play *player1* *player2* ) )
		do ( if ( equal winner *player2* )
				nil
				( let ()
					( rotate-play-modes ( halim-game-phase *player2* ) ( halim-finances *player2* ) *path-kb* )
					( rotate-play-modes ( halim-game-phase *player2* ) ( halim-finances *player2* ) *placement-kb* )
				)
			)
	)
	( format t "Statistics:~%==========~%~A win rate: ~,2f%~%~A win rate: ~,2f%~%Draws: ~,2f%" 
		( player-name *player1* )
		( * ( / ( player-stats *player1* ) rounds ) 100 )
		( player-name *player2* )
		( * ( / ( player-stats *player2* ) rounds ) 100 )
		( * ( / ( - rounds ( sum ( mapcar #'player-stats *players* ) ) ) rounds ) 100 )
	)
	( setf *verbose* v )
)

( defun mock-game ( rounds verbose )
	( setf *verbose* verbose )
	( setf *player1* ( make-instance 'randa :carpets `( ( R . ,*n-carpets* ) ( G . ,*n-carpets* ) ) ) )
	( setf *player2* ( make-instance 'halim :carpets `( ( b . ,*n-carpets* ) ( y . ,*n-carpets* ) ) ) )
	( setf *players* ( list *player1* *player2* ) )
	( loop for i from 1 to rounds
		do ( if *verbose* ( format t "Round ~A~%" i ) )
		do ( open-for-business )
		do ( hi-assam )
		do ( mapcar ( lambda (x) ( setf ( player-coins x ) 30 ) ) *players* )
		do ( setf ( player-carpets *player1* ) `( ( R . ,*n-carpets* ) ( G . ,*n-carpets* ) ) )
		do ( setf ( player-carpets *player2* ) `( ( B . ,*n-carpets* ) ( Y . ,*n-carpets* ) ) )
		do ( setf winner ( play *player1* *player2* ) )
	)
	( format t "Statistics:~%==========~%~A win rate: ~,2f%~%~A win rate: ~,2f%~%Draws: ~,2f%" 
		( player-name *player1* )
		( * ( / ( player-stats *player1* ) rounds ) 100 )
		( player-name *player2* )
		( * ( / ( player-stats *player2* ) rounds ) 100 )
		( * ( / ( - rounds ( sum ( mapcar #'player-stats *players* ) ) ) rounds ) 100 )
	)
)
