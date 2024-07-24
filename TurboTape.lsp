(defun c:TT
  ( / 
    *error*
    oldsnap oldlayer oldecho
    tts ttq ttd
    plyline plylist plyset
    sset bset rset 
    blkname tagname
    pfeet pinch
    n
    pt0 pt1 pt2 pt3 pt4 pt5
    ang1 ang2
    bulge bulrad bulcent
    ppoff ppt1 ppt2
    textrot
    btmatrix
    ecsppt1 ecsppt2
    param1 param2
    fwid
    pmid pbulge prad pcent psag 
    clen cdist csag cbulge ccent
    slope oslope
    dist
  )

  ; error function
  (defun *error* ( msg )
    
    (vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))
    (if tts      (command-s "undo" ""))
    (if oldsnap  (setvar "OSMODE" oldsnap))
    (if oldlayer (setvar "CLAYER" oldlayer))
    (if oldecho  (setvar "CMDECHO" oldecho))

    (if (not (member msg '("Function cancelled" "quit / exit abort")))
        (princ (strcat "\nError: " msg))
    )
    (princ)
  ); end defun *error*
  
  ; load visual lisp extensions
  (vl-load-com)
  
  ; get shape selection from user, default to Horizontal
  (initget "Horizontal Vertical Fixture Custom Array")
  (setq tts (getkword "\nChoose a Turbo Tape Shape [Horizontal/Vertical/Fixture/Custom/Array] <Horizontal>: "))
    (if (not tts)
        (setq tts "Horizontal"))
  
  ; start undo marks
  (vla-startundomark (vla-get-activedocument (vlax-get-acad-object)))
  
  ; save and update system variables
  (setq oldsnap   (getvar "OSMODE")
        oldecho   (getvar "CMDECHO")
        oldlayer  (getvar "CLAYER"))
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)

  ; load layers, linetypes, and text styles if not found
  (if (= (tblsearch "style" "CDL3") nil)
      (entmake '((0 . "STYLE")
                (100 . "AcDbSymbolTableRecord")
                (100 . "AcDbTextStyleTableRecord")
                (2 . "CDL3")
                (3 . "FuturaBookBT.ttf")
                (70 . 0)
                (40 . 3.0)
                (41 . 1.0)
                (50 . 0.0)
                (71 . 0))
      )); end entmake / if
  (if (= (tblsearch "style" "CDL4.5-BOLD") nil)
      (entmake '((0 . "STYLE")
                (100 . "AcDbSymbolTableRecord")
                (100 . "AcDbTextStyleTableRecord")
                (2 . "CDL4.5-BOLD")
                (3 . "FUTURAM.TTF")
                (70 . 0)
                (40 . 4.5)
                (41 . 1.0)
                (50 . 0.0)
                (71 . 0))
      )); end entmake / if
  (if (= (tblsearch "ltype" "divide2") nil)
      (command "-linetype" "load" "divide2" "acad.lin" ""))
  (if (= (tblsearch "layer" "CDL-A-LTG-ATTRIBUTE" ) nil)
      (command "-layer" "Make" "CDL-A-LTG-ATTRIBUTE" "color" "white" "" "ltype" "continuous" "" ""))
  (if (= (tblsearch "layer" "CDL-A-LTG-FIXTURES" ) nil)
      (command "-layer" "Make" "CDL-A-LTG-FIXTURES" "color" "white" "" "ltype" "continuous" "" ""))
  (if (= (tblsearch "layer" "CDL-A-LTG-FIXTURES-BOLD" ) nil)
      (command "-layer" "Make" "CDL-A-LTG-FIXTURES-BOLD" "color" "5" "" "ltype" "divide2" "" ""))

; start conditionals for tts selection
  (cond
    
    ;;;;; =================================== start "Horizontal" condition  =================================== ;;;;
    ((= tts "Horizontal")
     
      ; repeat request for polyline until polyline is selected or function is exited
      (setvar "ERRNO" 0)
      (while (AND (not plyline) (/= (getvar "ERRNO") 52))
                (if (AND (setq plyline (car (entsel "\nSelect Polyline: "))) (/= (cdr (assoc 0 (entget plyline))) "LWPOLYLINE"))
                    (setq plyline nil)
                ); end if
      ); end while
     
      ;start selection set with polyline
      (setq sset (ssadd plyline))
     
      ;convert plyline to vl object
      (setq plyline (vlax-ename->vla-object plyline))
     
      ; change selected polyline color to 70
      (vla-put-color plyline 70)
     
      ; get feet value of polyline
      (setq pfeet (fix (/(vla-get-length plyline) 12)))
      ; get inches value rounded to nearest integer
      (setq pinch (fix(+(*(-(/(vla-get-length plyline)12)(fix(/(vla-get-length plyline)12)))12)0.5)))
        (cond 
          ((= pinch 12)
            (setq pinch 0)
            (setq pfeet (+ pfeet 1))
          )
          (t nil)
        ); end cond
     
      ; set block type
      (setq blkname (vl-string-trim " " (strcase (getstring T "\nEnter Type: "))))
          (if (= blkname "")
              (setq blkname "##"))
                
      ; set tag name
      (setq tagname (vl-string-trim " " (strcase (getstring T "\nEnter Tag: "))))
      
      ; set quantity of runs
      (setq ttq (getint "\n Enter Quantity <1>: "))
          (if (not ttq)
              (setq ttq 1))

      ; set blkname based on ttq with correct characters and check for duplicate blkname
      (if (= ttq 1)
          (setq blkname (strcat blkname "-" (itoa pfeet) "ft" (itoa pinch) "in" ))
          (setq blkname (strcat blkname "-" (itoa pfeet) "ft" (itoa pinch) "in(x" (itoa ttq) ")" ))
      ); end if
        (while (tblsearch "BLOCK" blkname)
               (setq blkname (strcat blkname "^"))
        ); end while

      ; set polyline layer and color
      (vla-put-layer plyline "CDL-A-LTG-FIXTURES-BOLD")
      (vla-put-color plyline acByLayer)

      ; change plyline to entity and create list of coordinates / set counter to 0
      (setq plyline (vlax-vla-object->ename plyline)
            plylist (mapcar 'cdr (vl-remove-if '(lambda (x) (/= 10 (car x))) (entget plyline)))
            n       0
      ); end setq
     
      ; if plyline is closed, add origin point to end of plylist
      (if
        (= (vla-get-closed (vlax-ename->vla-object plyline)) :vlax-true)
        (setq plylist (append plylist (list(cdr(assoc 10 (entget plyline))))))
      );end if
     
      ;;; ============================================= START REPEAT ============================================= ;;;

      ; repeat based on length of plylist - 1
      (repeat (- (length plylist) 1)
        
        ; start conditionals for segments with or without bulges
        (cond
          
          ; condition for segment where bulge = 0
          ((= (nth n (mapcar 'cdr (vl-remove-if '(lambda (x) (/= 42 (car x))) (entget plyline)))) 0 )
           
            ; build variables
            (setq pt1  (polar (nth n plylist) (angle (nth n plylist) (nth (+ n 1) plylist)) 3)
                  n    (+ n 1)
                  pt2  (polar (nth n plylist) (angle (nth n plylist) (nth (- n 1) plylist)) 3)
                  ang1 (angle pt1 pt2)
                  ang2 (angle pt2 pt1)
            ); end setq
                  (entmake (list  (cons 0 "CIRCLE")
                                  (cons 8 "CDL-A-LTG-FIXTURES-BOLD")
                                  (cons 10 pt1)
                                  (cons 40 1.0)
                           ); end list
                  ); end entmake
                  (ssadd (entlast) sset)
                  (entmake (list  (cons 0 "CIRCLE")
                                  (cons 8 "CDL-A-LTG-FIXTURES-BOLD")
                                  (cons 10 pt2)
                                  (cons 40 1.0)
                           ); end list
                  ); end entmake
                  (ssadd (entlast) sset)
           
          ); end condition for bulge = 0
          
          ; condition for segment where bulge /= 0
          ((/= (nth n (mapcar 'cdr (vl-remove-if '(lambda (x) (/= 42 (car x))) (entget plyline)))) 0 )
           
            ; build variables
            (setq pt1     (nth n plylist)
                  bulge   (nth n (mapcar 'cdr (vl-remove-if '(lambda (x) (/= 42 (car x))) (entget plyline))))
                  n       (+ n 1)
                  pt2     (nth n plylist)
                  bulrad  (/ (* (distance pt1 pt2) (1+ (* bulge bulge))) 4 (abs bulge))   ; bulge radius
                  bulcent (polar pt1 (+ (angle pt1 pt2) (- (/ pi 2) (* 2 (atan bulge))))(/ (* (distance pt1 pt2) (1+ (* bulge bulge))) 4 bulge))   ; bulge centerpoint
            ); end setq
           
            ; start conditionals for segments with or positive or negative bulges
            (cond
              
              ; condition for segment where bulge > 0
              ((> bulge 0)
                  (entmake (list  (cons 0 "CIRCLE")
                                  (cons 8 "CDL-A-LTG-FIXTURES-BOLD")
                                  (cons 10 pt1)
                                  (cons 40 1.0)
                           ); end list
                  ); end entmake
                  (vla-rotate (vlax-ename->vla-object (entlast)) (vlax-3d-point bulcent) (/ 3 bulrad))
                  (ssadd (entlast) sset)
              
                  (entmake (list  (cons 0 "CIRCLE")
                                  (cons 8 "CDL-A-LTG-FIXTURES-BOLD")
                                  (cons 10 pt2)
                                  (cons 40 1.0)
                           ); end list
                  ); end entmake
                  (vla-rotate (vlax-ename->vla-object (entlast)) (vlax-3d-point bulcent) (- (/ 3 bulrad)))
                  (ssadd (entlast) sset)
               
              ); end condition for segment where bulge > 0
              
              ; condition for segment where bulge < 0
              ((< bulge 0)
                  (entmake (list  (cons 0 "CIRCLE")
                                  (cons 8 "CDL-A-LTG-FIXTURES-BOLD")
                                  (cons 10 pt1)
                                  (cons 40 1.0)
                           );end list
                  );end entmake
                  (vla-rotate (vlax-ename->vla-object (entlast)) (vlax-3d-point bulcent) (- (/ 3 bulrad)))
                  (ssadd (entlast) sset)

                  (entmake (list  (cons 0 "CIRCLE")
                                  (cons 8 "CDL-A-LTG-FIXTURES-BOLD")
                                  (cons 10 pt2)
                                  (cons 40 1.0)
                           );end list
                  );end entmake
                  (vla-rotate (vlax-ename->vla-object (entlast)) (vlax-3d-point bulcent) (/ 3 bulrad))
                  (ssadd (entlast) sset)
               
              ); end condition for segment where bulge < 0
              
              (t nil)
              
            ); end conditionals for segments with or positive or negative bulges
          
          ); end condition for segment where bulge /= 0
          
          (t nil)
          
        ); end conditionals for segments with or without bulges
        
      ); end repeat

      ;;; ============================================= END REPEAT ============================================= ;;;     
     
      ; reset variables for first segment
      (setq pt1   (car plylist)
            pt2   (cadr plylist)
            ang1  (angle pt1 pt2)
            ang2  (angle pt2 pt1)
            bulge (cdr(assoc 42 (entget plyline))))
      (if
        ; check if first segment is a bulge
        (= (cdr (assoc 42 (entget plyline))) 0.0)
          ; if not a bulge
          (setq ppoff 0)
          ; if a bulge, calculate height of bulge (sagitta)
          (setq ppoff (* (/ (distance pt1 pt2) 2) (abs (car (mapcar 'cdr (vl-remove-if '(lambda (x) (/= 42 (car x))) (entget plyline)))))))
      ); end if
     
      ; establish text rotation conditionals
      (cond
        
        ((<= bulge 0.0)
          ; establish insertion points for parameters when bulge < 0
          ; get midpoint between first two points and offset the distance
          (setq ppt1 (polar (polar pt1 ang1 (/ (distance pt1 pt2) 2)) (+ ang1 (*(/ pi 180)90)) (+ ppoff 5)))
          ; get midpoint between first two points and offset the distance
          (setq ppt2 (polar (polar pt1 ang1 (/ (distance pt1 pt2) 2)) (+ ang1 (*(/ pi 180)90)) (+ ppoff 10)))
          (if
            (and
              (>  (rtos ang1 2 4) (rtos (/ pi 2) 2 4))
              (<= (rtos ang1 2 4) (rtos (* 3 (/ pi 2)) 2 4))
            )
            (setq textrot (* (+ ang1 pi) (/ 180 pi)))
            (setq textrot (* ang1 (/ 180 pi)))
          )

        ); end bulge < 0 condition

        ((> bulge 0.0)
          ; establish insertion points for parameters when bulge > 0
          (setq ppt1 (polar (polar pt1 ang1 (/ (distance pt1 pt2) 2)) (+ ang2 (*(/ pi 180)90)) (+ ppoff 5)))
          (setq ppt2 (polar (polar pt1 ang1 (/ (distance pt1 pt2) 2)) (+ ang2 (*(/ pi 180)90)) (+ ppoff 10)))
          (if
            (and
              (>  (rtos ang1 2 4) (rtos (/ pi 2) 2 4))
              (<= (rtos ang1 2 4) (rtos (* 3 (/ pi 2)) 2 4))
            )
            (setq textrot (* (+ ang1 pi) (/ 180 pi)))
            (setq textrot (* ang1 (/ 180 pi)))
          )

        ); end bulge > 0 condition
        
        (t nil)
                
      ); end text rotation conditionals
    
      ; build and insert block
      (command-s "-block" blkname pt1 sset "")
      (command "-insert" blkname pt1 "" "" "")
      (command "change" "last" "" "properties" "layer" "CDL-A-LTG-FIXTURES" "")
     
      ; extract WCS->ECS block transformation matrix
      (setq btmatrix (cdr (assoc 10 (entget (entlast)))))
     
      (command "-bedit" blkname)
      ; define ECS translation points for ppt1 ppt2
      (setq ecsppt1 (list (-(car ppt1)(car btmatrix)) (-(cadr ppt1)(cadr btmatrix)) '0.0))
      (setq ecsppt2 (list (-(car ppt2)(car btmatrix)) (-(cadr ppt2)(cadr btmatrix)) '0.0))
     
      ; insert block type text and add to selection set
      (command "text" "style" "CDL4.5-BOLD" "justify" "MC" ecsppt1 textrot (vl-string-subst "\"" "in" (vl-string-subst "'-" "ft" (vl-string-trim "^" blkname))))
      (command "change" "last" "" "properties" "annotative" "no" "layer" "CDL-A-LTG-ATTRIBUTE" "")
      (setpropertyvalue (entlast) "Height" 4.5)
      (setq param1 (entlast))
      (setq bset (ssget "_L"))

      ; insert block tag text and add to selection set if tagname /= nil
      (cond
        ( (/= tagname "")
          (command "text" "style" "CDL3" "justify" "MC" ecsppt2 textrot tagname)
          (command "change" "last" "" "properties" "annotative" "no" "layer" "CDL-A-LTG-ATTRIBUTE" "")
          (setpropertyvalue (entlast) "Height" 3)
          (setq param2 (entlast))
          (ssadd (entlast) bset)
        ); end condition
      ); end cond

      ; insert rotation parameter for param1 and add to bset/rset
      (command "bparameter" "rotation" ecsppt1 "10" (polar ecsppt1 (*(/ pi 180)textrot) 10) (polar ecsppt1 (*(/ pi 180)textrot) 12) "1")
      (ssadd (entlast) bset)
      (setq rset (ssadd (entlast)))
      (ssadd param1 rset)
      (sssetfirst nil (ssget "_L"))
      (command "baction" param1 "")

      ; insert move grip for param1 and add to bset
      (command "bparameter" "point" ecsppt1 (polar ecsppt1 ang1 5) "1")
      (ssadd (entlast) bset)
      (sssetfirst nil (ssget "_L"))
      (command "baction" "move" rset "")
      (setq rset nil)

      ; insert rotation parameter for param2 and add to bset/rset
      (cond
        ( (/= tagname "")
          (command "bparameter" "rotation" ecsppt2 "10" (polar ecsppt2 (*(/ pi 180)textrot) 10) (polar ecsppt2 (*(/ pi 180)textrot) 12) "1")
          (ssadd (entlast) bset)
          (setq rset (ssadd (entlast)))
          (ssadd param2 rset)
          (sssetfirst nil (ssget "_L"))
          (command "baction" param2 "")
        ); end condition
      ); end cond

      ; insert move grip for param2 and add to bset if tagname /= nil
      (cond
        ( (/= tagname "")
          (command "bparameter" "point" ecsppt2 (polar ecsppt2 ang1 10) "1")
          (ssadd (entlast) bset)
          (sssetfirst nil (ssget "_L"))
          (command "baction" "move" rset "")
        ); end condition
      ); end cond
                
      ; add flip parameter if bulge = 0
      (cond
        ( (= bulge 0)
          (command-s "bparameter" "flip" 
            ; basepoint for flip parameter is ECS translated 'pt1' + 1
            (polar (list(-(car pt1)(car btmatrix)) (-(cadr pt1)(cadr btmatrix))) ang1 6)
            ; rotation endpoint for flip parameter is ECS translated 'pt1' + 6
            (polar (list(-(car pt1)(car btmatrix)) (-(cadr pt1)(cadr btmatrix))) ang1 10)
            ;label location for flip parameter is ECS translated 'pt1' + 11
            (polar (list(-(car pt1)(car btmatrix)) (-(cadr pt1)(cadr btmatrix))) ang1 12)
            "1"
          ); end command - flip parameter
          ; select 'bset' selection set for flip parameter objects
          (sssetfirst nil (ssget "_L"))
          (command "baction" bset "")
        ); end condition
      ); end cond
     
      ; save and close block editor
      (command "bsave")
      (command "bclose")
    
    );;;;; ==================================  end "Horizontal" condition   =================================== ;;;;
    
    ;;;;; =================================== start "Vertical" condition  =================================== ;;;;
    ((= tts "Vertical")
    
      ; set number of segments
      (initget 1 "1 2 3 4")
      (setq ttq (atoi (getkword "\nEnter number of segments [1/2/3/4]: ")))

        (cond
          ((= ttq 1); entmake for 1 circle
            (setvar "OSMODE" oldsnap)
            (setq pt0 (getpoint "\nSelect Basepoint: "))
            (setvar "OSMODE" 0)
            (setq pt1   (polar pt0 pi 1)
                  pt2   (polar pt0 0 1)
                  ang1  (angle pt1 pt2)
                  ang2  (angle pt2 pt1))
            (entmake (list (cons 0 "CIRCLE")
                           (cons 8 "CDL-A-LTG-FIXTURES-BOLD")
                           (cons 10 pt0)
                           (cons 40 1.0)
                    ); end list
            ); end entmake
            (setq sset (ssadd (entlast)))
          ); end conditional 1 circle

          ((= ttq 2); entmake for 2 circles
            (setvar "OSMODE" oldsnap)         
            (setq pt1 (getpoint "\nSelect first corner: "))
            (setq pt2 (getpoint "\nSelect second corner: "))
            (setvar "OSMODE" 0)
            (setq pt0 (polar pt1 (angle pt1 pt2) (/ (distance pt1 pt2) 2))
                  ang1 (angle pt1 pt2)
                  ang2 (angle pt2 pt1))
            (entmake (list (cons 0 "CIRCLE")
                           (cons 8 "CDL-A-LTG-FIXTURES-BOLD")
                           (cons 10 (polar pt1 ang1 2))
                           (cons 40 1.0)
                    ); end list
            ); end entmake
            (setq sset (ssadd (entlast)))
            (entmake (list (cons 0 "CIRCLE")
                           (cons 8 "CDL-A-LTG-FIXTURES-BOLD")
                           (cons 10 (polar pt2 ang2 2))
                           (cons 40 1.0)
                      ); end list
            ); end entmake
            (ssadd (entlast) sset)
          
          ); end conditional 2 circles

          ((= ttq 3); entmake for 3 circles
            (setvar "OSMODE" oldsnap)         
            (setq pt1 (getpoint "\nSelect first outer corner: "))
            (setq pt2 (getpoint "\nSelect second outer corner: "))
            (setq pt3 (getpoint "\nSelect middle corner: "))
            (setvar "OSMODE" 0)
            (setq pt0 (list
                        (/ (+ (car pt1) (car pt2) (car pt3)) 3.0)
                        (/ (+ (cadr pt1) (cadr pt2) (cadr pt3)) 3.0)
                        (/ (+ (caddr pt1) (caddr pt2) (caddr pt3)) 3.0)
                      )
                  ang1 (angle pt1 pt0)
                  ang2 (angle pt3 pt0))
            (entmake  (list (cons 0 "CIRCLE")
                            (cons 8 "CDL-A-LTG-FIXTURES-BOLD")
                            (cons 10 pt1)
                            (cons 40 1.0)
                      ); end list
            ); end entmake
            (setq sset (ssadd (entlast)))
            (entmake  (list (cons 0 "CIRCLE")
                            (cons 8 "CDL-A-LTG-FIXTURES-BOLD")
                            (cons 10 pt2)
                            (cons 40 1.0)
                      ); end list
            ); end entmake
            (ssadd (entlast) sset)
            (entmake  (list (cons 0 "CIRCLE")
                            (cons 8 "CDL-A-LTG-FIXTURES-BOLD")
                            (cons 10 pt3)
                            (cons 40 1.0)
                      ); end list
            ); end entmake
            (ssadd (entlast) sset)
           
          ); end conditional 3 circles
          
          ((= ttq 4); entmake for 4 circles
            (setvar "OSMODE" oldsnap)
            (setq pt0 (getpoint "\nSelect centerpoint: "))
            (setq pt1 (getpoint "\nSelect outer corner: "))
            (setvar "OSMODE" 0)
            (setq pt2  (polar pt1 (angle pt1 pt0) (* (distance pt1 pt0) 2))
                  ang1 (angle pt1 pt2)
                  ang2 (angle pt2 pt1))
            (entmake (list (cons 0 "CIRCLE")
                           (cons 8 "CDL-A-LTG-FIXTURES-BOLD")
                           (cons 10 (polar pt0 ang1 2))
                           (cons 40 1.0)
                    ); end list
            ); end entmake
            (setq sset (ssadd (entlast)))
            (entmake (list (cons 0 "CIRCLE")
                           (cons 8 "CDL-A-LTG-FIXTURES-BOLD")
                           (cons 10 (polar pt0 ang2 2))
                           (cons 40 1.0)
                    ); end list
            ); end entmake
            (ssadd (entlast) sset)
            (entmake (list (cons 0 "CIRCLE")
                           (cons 8 "CDL-A-LTG-FIXTURES-BOLD")
                           (cons 10 (polar pt1 ang1 2))
                           (cons 40 1.0)
                    ); end list
            ); end entmake
            (ssadd (entlast) sset)
            (entmake (list (cons 0 "CIRCLE")
                          (cons 8 "CDL-A-LTG-FIXTURES-BOLD")
                          (cons 10 (polar pt2 ang2 2))
                          (cons 40 1.0)
                    ); end list
            ); end entmake
            (ssadd (entlast) sset)
           
          ); end conditional for 4 circles
          
          (t nil); no conditions met
        ); end cond

      ; query user for block type, tag, and lengths
      (setq blkname (vl-string-trim " " (strcase (getstring T "\nEnter Type: "))))
        (if (= blkname "")
            (setq blkname "##"))
      (setq tagname (vl-string-trim " " (strcase (getstring T "\nEnter Tag: "))))
      (setq pfeet (getint "\nEnter Feet: "))
        (if (not (numberp pfeet))
            (setq pfeet "#"))
      (setq pinch (getint "\nEnter Inches: "))
        (if (not (numberp pinch))
            (setq pinch "##"))
     
      ; set blkname based on ttq with correct characters and check for duplicate blkname
      (if (= ttq 1)
          (setq blkname (strcat blkname "-" (if (numberp pfeet) (itoa pfeet) pfeet) "ft" (if (numberp pinch) (itoa pinch) pinch) "in" ))
          (setq blkname (strcat blkname "-" (if (numberp pfeet) (itoa pfeet) pfeet) "ft" (if (numberp pinch) (itoa pinch) pinch) "in(x" (itoa ttq) ")" ))
      ); end if
      (while (tblsearch "BLOCK" blkname)
             (setq blkname (strcat blkname "^"))
      ); end while 
      
      ; build and insert block, set to proper layer
      (command-s "-block" blkname pt0 sset "")
      (command "-insert" blkname pt0 "" "" "")
      (command "change" "last" "" "properties" "layer" "CDL-A-LTG-FIXTURES" "")
      
      ; extract WCS->ECS block transformation matrix
      (setq btmatrix (cdr (assoc 10 (entget (entlast)))))
      
      ; establish text rotation conditionals
      (cond
        ((< (car pt1) (car pt2))
        ;establish insertion points for parameters when xvalue of pt1 < xvalue of pt2
          (setq ppt1 (polar (polar pt1 (angle pt1 pt2) (/ (distance pt1 pt2) 2)) (+ ang1 (*(/ pi 180)90)) 6))
          (setq ppt2 (polar (polar pt1 (angle pt1 pt2) (/ (distance pt1 pt2) 2)) (+ ang1 (*(/ pi 180)90)) 11))
          (setq textrot (/(* ang1 180) pi))
                
        ); end conditional 1

        ((> (car pt1) (car pt2))
        ; establish insertion points for parameters when xvalue of pt1 > xvalue of pt2
          (setq ppt1 (polar (polar pt1 (angle pt1 pt2) (/ (distance pt1 pt2) 2)) (+ ang2 (*(/ pi 180)90)) 6))
          (setq ppt2 (polar (polar pt1 (angle pt1 pt2) (/ (distance pt1 pt2) 2)) (+ ang2 (*(/ pi 180)90)) 11))
          (setq textrot (/(* ang2 180) pi))
                              
        ); end conditional 2
                    
        ((= (car pt1) (car pt2))
        ; establish insertion points for parameters when xvalue of pt1 = xvalue of pt2
          (setq ppt1 (polar (polar pt1 (angle pt1 pt2) (/ (distance pt1 pt2) 2)) (+ ang2 (*(/ pi 180)90)) 6))
          (setq ppt2 (polar (polar pt1 (angle pt1 pt2) (/ (distance pt1 pt2) 2)) (+ ang2 (*(/ pi 180)90)) 11))
          (setq textrot 90)
                  
        ); end conditional 3
                  
        (t nil); no conditions are met
      
      ); end cond
      
      ; enter block editor
      (command "-bedit" blkname)
            
      ; define ECS translation points for ppt1 ppt2
      (setq ecsppt1 (list (-(car ppt1)(car btmatrix)) (-(cadr ppt1)(cadr btmatrix)) '0.0))
      (setq ecsppt2 (list (-(car ppt2)(car btmatrix)) (-(cadr ppt2)(cadr btmatrix)) '0.0))

      ; insert block type text and add to selection set
      (command "text" "style" "CDL4.5-BOLD" "justify" "MC" ecsppt1 textrot (vl-string-subst "\"" "in" (vl-string-subst "'-" "ft" (vl-string-trim "^" blkname))))
      (command "change" "last" "" "properties" "annotative" "no" "layer" "CDL-A-LTG-ATTRIBUTE" "")
      (setpropertyvalue (entlast) "Height" 4.5)
      (setq param1 (entlast))
      (setq bset (ssget "_L"))

      ; insert block tag text and add to selection set
      (cond
        ((/= tagname "")
          (command "text" "style" "CDL3" "justify" "MC" ecsppt2 textrot tagname)
          (command "change" "last" "" "properties" "annotative" "no" "layer" "CDL-A-LTG-ATTRIBUTE" "")
          (setpropertyvalue (entlast) "Height" 3)
          (setq param2 (entlast))
          (ssadd (entlast) bset)
        ); end condition
      ); end cond
                
      ; insert rotation parameter for param1 and add to bset/rset
      (command "bparameter" "rotation" ecsppt1 "10" (polar ecsppt1 (*(/ pi 180)textrot) 10) (polar ecsppt1 (*(/ pi 180)textrot) 12) "1")
      (ssadd (entlast) bset)
      (setq rset (ssadd (entlast)))
      (ssadd param1 rset)
      (sssetfirst nil (ssget "_L"))
      (command "baction" param1 "")
                
      ; insert move grip for param1 and add to bset
      (command "bparameter" "point" ecsppt1 (polar ecsppt1 (*(/ pi 180)textrot) 4) "1")
      (ssadd (entlast) bset)
      (sssetfirst nil (ssget "_L"))
      (command "baction" "move" rset "")
      (setq rset nil)

      ; insert rotation parameter for param2 and add to bset/rset if tagname /= nil
      (cond
        ((/= tagname "")
          (command "bparameter" "rotation" ecsppt2 "10" (polar ecsppt2 (*(/ pi 180)textrot) 10) (polar ecsppt2 (*(/ pi 180)textrot) 12) "1")
          (ssadd (entlast) bset)
          (setq rset (ssadd (entlast)))
          (ssadd param2 rset)
          (sssetfirst nil (ssget "_L"))
          (command "baction" param2 "")
        ); end condition
      ); end cond
      
      ; insert move grip for param2 and add to bset if tagname /= nil
      (cond
        ((/= tagname "")
          (command "bparameter" "point" ecsppt2 (polar ecsppt2 (*(/ pi 180)textrot) 4) "1")
          (ssadd (entlast) bset)
          (sssetfirst nil (ssget "_L"))
          (command "baction" "move" rset "")
        ); end condition
      ); end cond

      (command-s "bparameter" "flip" 
        ;basepoint for flip parameter is ECS translated 'pt0' + 4
        (polar (list(-(car pt0)(car btmatrix)) (-(cadr pt0)(cadr btmatrix))) (*(/ pi 180)textrot) 4)
        ;rotation endpoint for flip parameter is ECS translated 'pt0' + 8
        (polar (list(-(car pt0)(car btmatrix)) (-(cadr pt0)(cadr btmatrix))) (*(/ pi 180)textrot) 8)
        ;label location for flip parameter is ECS translated 'pt0' + 10
        (polar (list(-(car pt0)(car btmatrix)) (-(cadr pt0)(cadr btmatrix))) (*(/ pi 180)textrot) 10)
        "1"
      ); end command - flip parameter
     
      ; select 'bset' selection set for flip parameter objects
      (sssetfirst nil (ssget "_L"))
      (command "baction" bset "")

      ; select all circles 
      (setq cset (ssget "_X" '((0 . "CIRCLE") (8 . "CDL-A-LTG-FIXTURES-BOLD"))))
      (setq n 0)
      (repeat (sslength cset)
        ; add point parameter to circle
        (command 
          "bparameter" "point" (cdr (assoc 10 (entget (ssname cset n))))
          (polar  (cdr (assoc 10 (entget (ssname cset n))))
                  (angle '(0 0 0) (cdr (assoc 10 (entget (ssname cset n)))))
                  5.0
          )
          "1"
        )
        ; add move action to parameter and circle
        (sssetfirst nil (ssget "_L"))
        (command "baction" "move" (ssname cset n) "")
        
        (setq n (1+ n))
      ); end repeat
     
      (command "bsave")
      (command "bclose")
     
    );;;;; ==================================  end "Vertical" condition   =================================== ;;;;
    
    ;;;;; =================================== start "Fixture" condition  =================================== ;;;;
    ((= tts "Fixture")
  
      ; repeat request for polyline until polyline is selected or function is exited
      (setvar "ERRNO" 0)
      (while (AND (not plyline) (/= (getvar "ERRNO") 52))
                (if (AND (setq plyline (car (entsel "\nSelect Polyline: "))) (/= (cdr (assoc 0 (entget plyline))) "LWPOLYLINE"))
                    (setq plyline nil)
                ); end if
      ); end while

      ;convert plyline to vl object
      (setq plyline (vlax-ename->vla-object plyline))
     
      ; change selected polyline layer to 0 and color to 70
      (vla-put-layer plyline "0")
      (vla-put-color plyline 70)
     
      ; get feet value of polyline
      (setq pfeet (fix (/(vla-get-length plyline) 12)))
      ; get inches value rounded to nearest integer
      (setq pinch (fix(+(*(-(/(vla-get-length plyline)12)(fix(/(vla-get-length plyline)12)))12)0.5)))
        (cond 
          ((= pinch 12)
            (setq pinch 0)
            (setq pfeet (+ pfeet 1))
          )
          (t nil)
        ); end cond
     
      ; set block type
      (setq blkname (vl-string-trim " " (strcase (getstring T "\nEnter Type: "))))
                
      ; set tag name
      (setq tagname (vl-string-trim " " (strcase (getstring T "\nEnter Tag: "))))

      ; set blkname with correct characters and check for duplicate blkname
      (setq blkname (strcat blkname "-" (itoa pfeet) "ft" (itoa pinch) "in" ))
      (while (tblsearch "BLOCK" blkname)
             (setq blkname (strcat blkname "^"))
      ); end while

      ; change plyline to entity and create list of coordinates
      (setq plyline (vlax-vla-object->ename plyline)
            plylist (mapcar 'cdr (vl-remove-if '(lambda (x) (/= 10 (car x))) (entget plyline)))
      ); end setq

      ; set user defined width (default 2")
      (setq fwid (getreal "\nEnter Width <2>: "))
        (if (not fwid)
            (setq fwid 2))
     
      (setq ang1 (angle (nth 0 plylist) (nth 1 plylist))
            ang2 (angle (nth 1 plylist) (nth 0 plylist)))
     
      ; offset plyline a distance of +fwid/2
      (vla-offset (vlax-ename->vla-object plyline) (+ (/ fwid 2)))
        (vla-put-color (vlax-ename->vla-object (entlast)) acByLayer)
        (vla-put-layer (vlax-ename->vla-object (entlast)) "0")
        (setq sset (ssget "_L"))
          (if (/= (vla-get-closed (vlax-ename->vla-object plyline)) :vlax-true)
              (setq pt1 (cdr(assoc 10 (entget(entlast))))
                    pt3 (cdr(assoc 10 (reverse(entget(entlast))))))
          ); end if
     
      ; offset plyline a distance of -fwid/2
      (vla-offset (vlax-ename->vla-object plyline) (- (/ fwid 2)))
        (vla-put-color (vlax-ename->vla-object (entlast)) acByLayer)
        (vla-put-layer (vlax-ename->vla-object (entlast)) "0")
        (ssadd (entlast) sset)
          (if (/= (vla-get-closed (vlax-ename->vla-object plyline)) :vlax-true)
              (setq pt2 (cdr(assoc 10 (entget(entlast))))
                    pt4 (cdr(assoc 10 (reverse(entget(entlast))))))
          ); end if

      (if (/= (vla-get-closed (vlax-ename->vla-object plyline)) :vlax-true)
        (progn
          (command "pline" pt1 pt2 "")
            (vla-put-color (vlax-ename->vla-object (entlast)) acByLayer)
            (vla-put-layer (vlax-ename->vla-object (entlast)) "0")
            (ssadd (entlast) sset)
          (command "pline" pt3 pt4 "")
            (vla-put-color (vlax-ename->vla-object (entlast)) acByLayer)
            (vla-put-layer (vlax-ename->vla-object (entlast)) "0")          
            (ssadd (entlast) sset)
          (command "pedit" sset "join" "all" "" "")
        ); end progn
      ); end if

      (setq pt1   (nth 0 plylist)
            pt2   (nth 1 plylist)
            bulge (cdr(assoc 42 (entget plyline))))
     
      (if
        ; check if first segment is a bulge
        (= (cdr (assoc 42 (entget plyline))) 0.0)
          ; if not a bulge
          (setq ppoff 0)
          ; if a bulge, calculate height of bulge (sagitta)
          (setq ppoff (* (/ (distance pt1 pt2) 2) (abs (car (mapcar 'cdr (vl-remove-if '(lambda (x) (/= 42 (car x))) (entget plyline)))))))
      ); end if
     
      ; delete original polyline
      (entdel plyline)
     
      ; establish text rotation conditionals
      (cond
        
        ((<= bulge 0.0)
          ; establish insertion points for parameters when bulge < 0
          ; get midpoint between first two points and offset the distance
          (setq ppt1 (polar (polar pt1 ang1 (/ (distance pt1 pt2) 2)) (+ ang1 (*(/ pi 180)90)) (+ (/ fwid 2) (+ ppoff 5))))
          ; get midpoint between first two points and offset the distance
          (setq ppt2 (polar (polar pt1 ang1 (/ (distance pt1 pt2) 2)) (+ ang1 (*(/ pi 180)90)) (+ (/ fwid 2) (+ ppoff 10))))
          (if
            (and
              (>  (rtos ang1 2 4) (rtos (/ pi 2) 2 4))
              (<= (rtos ang1 2 4) (rtos (* 3 (/ pi 2)) 2 4))
            )
            (setq textrot (* (+ ang1 pi) (/ 180 pi)))
            (setq textrot (* ang1 (/ 180 pi)))
          )
        
        ); end bulge < 0 OR bulge = 0 condition

        ((> bulge 0.0)
          ; establish insertion points for parameters when bulge > 0
          (setq ppt1 (polar (polar pt1 ang1 (/ (distance pt1 pt2) 2)) (+ ang2 (*(/ pi 180)90)) (+ (/ fwid 2) (+ ppoff 5))))
          (setq ppt2 (polar (polar pt1 ang1 (/ (distance pt1 pt2) 2)) (+ ang2 (*(/ pi 180)90)) (+ (/ fwid 2) (+ ppoff 10))))
          (if
            (and
              (>  (rtos ang1 2 4) (rtos (/ pi 2) 2 4))
              (<= (rtos ang1 2 4) (rtos (* 3 (/ pi 2)) 2 4))
            )
            (setq textrot (* (+ ang1 pi) (/ 180 pi)))
            (setq textrot (* ang1 (/ 180 pi)))
          )
         
        ); end bulge > 0 condition
        
        (t nil)

      ); end text rotation conditionals
     
      ; build and insert block
      (command-s "-block" blkname pt1 sset "")
      (command "-insert" blkname pt1 "" "" "")
      (command "change" "last" "" "properties" "layer" "CDL-A-LTG-FIXTURES" "")
     
      ; extract WCS->ECS block transformation matrix
      (setq btmatrix (cdr (assoc 10 (entget (entlast)))))
     
      (command "-bedit" blkname)
      ; define ECS translation points for ppt1 ppt2
      (setq ecsppt1 (list (-(car ppt1)(car btmatrix)) (-(cadr ppt1)(cadr btmatrix)) '0.0))
      (setq ecsppt2 (list (-(car ppt2)(car btmatrix)) (-(cadr ppt2)(cadr btmatrix)) '0.0))
     
      ; insert block type text and add to selection set
      (command "text" "style" "CDL4.5-BOLD" "justify" "MC" ecsppt1 textrot (vl-string-subst "\"" "in" (vl-string-subst "'-" "ft" (vl-string-trim "^" blkname))))
      (command "change" "last" "" "properties" "annotative" "no" "layer" "CDL-A-LTG-ATTRIBUTE" "")
      (setpropertyvalue (entlast) "Height" 4.5)
      (setq param1 (entlast))
      (setq bset (ssget "_L"))
     
      ; insert block tag text and add to selection set
      (cond
        ((/= tagname "")
          (command "text" "style" "CDL3" "justify" "MC" ecsppt2 textrot tagname)
          (command "change" "last" "" "properties" "annotative" "no" "layer" "CDL-A-LTG-ATTRIBUTE" "")
          (setpropertyvalue (entlast) "Height" 3)
          (setq param2 (entlast))
          (ssadd (entlast) bset)
        ); end condition
      ); end cond
     
      ; insert rotation parameter for param1 and add to bset/rset
      (command "bparameter" "rotation" ecsppt1 "10" (polar ecsppt1 (*(/ pi 180)textrot) 10) (polar ecsppt1 (*(/ pi 180)textrot) 12) "1")
      (ssadd (entlast) bset)
      (setq rset (ssadd (entlast)))
      (ssadd param1 rset)
      (sssetfirst nil (ssget "_L"))
      (command "baction" param1 "")
     
      ; insert move grip for param1 and add to bset
      (command "bparameter" "point" ecsppt1 (polar ecsppt1 ang1 5) "1")
      (ssadd (entlast) bset)
      (sssetfirst nil (ssget "_L"))
      (command "baction" "move" rset "")
      (setq rset nil)
      
      ; insert rotation parameter for param2 and add to bset/rset if tagname /= nil
      (cond
        ((/= tagname "")
          (command "bparameter" "rotation" ecsppt2 "10" (polar ecsppt2 (*(/ pi 180)textrot) 10) (polar ecsppt2 (*(/ pi 180)textrot) 12) "1")
            (ssadd (entlast) bset)
            (setq rset (ssadd (entlast)))
            (ssadd param2 rset)
            (sssetfirst nil (ssget "_L"))
            (command "baction" param2 "")
        ); end condition
      ); end cond
      
      ; insert move grip for param2 and add to bset if tagname /= nil
      (cond
        ((/= tagname "")
          (command "bparameter" "point" ecsppt2 (polar ecsppt2 ang1 5) "1")
          (ssadd (entlast) bset)
          (sssetfirst nil (ssget "_L"))
          (command "baction" "move" rset "")
        ); end condition
      ); end cond
      
      ; add flip parameter if bulge = 0
      (cond
        ( (= bulge 0)
          (command-s "bparameter" "flip" 
            ; basepoint for flip parameter is ECS translated 'pt1' + 1
            (polar (list(-(car pt1)(car btmatrix)) (-(cadr pt1)(cadr btmatrix))) ang1 6)
            ; rotation endpoint for flip parameter is ECS translated 'pt1' + 6
            (polar (list(-(car pt1)(car btmatrix)) (-(cadr pt1)(cadr btmatrix))) ang1 10)
            ;label location for flip parameter is ECS translated 'pt1' + 11
            (polar (list(-(car pt1)(car btmatrix)) (-(cadr pt1)(cadr btmatrix))) ang1 12)
            "1"
          ); end command - flip parameter 
          ; select 'bset' selection set for flip parameter objects
          (sssetfirst nil (ssget "_L"))
          (command "baction" bset "")
        ); end condition
      ); end cond
     
      ; save and close block editor
      (command "bsave")
      (command "bclose")
     
    );;;;; ==================================  end "Fixture" condition   =================================== ;;;;
    
    ;;;;; =================================== start "Custom" condition  =================================== ;;;;
    ((= tts "Custom")
     
      ; repeat request for polyline until polyline is selected or function is exited
      (setvar "ERRNO" 0)
      (while (AND (not plyline) (/= (getvar "ERRNO") 52))
                (if (AND (setq plyline (car (entsel "\nSelect Polyline: "))) (/= (cdr (assoc 0 (entget plyline))) "LWPOLYLINE"))
                    (setq plyline nil)
                ); end if
      ); end while
     
      ;start selection set with polyline
      (setq sset (ssadd plyline))
     
      ;convert plyline to vl object
      (setq plyline (vlax-ename->vla-object plyline))
     
      ; change selected polyline color to 70
      (vla-put-color plyline 70)
     
      ; query user for block type, tag, and lengths
      (setq blkname (vl-string-trim " " (strcase (getstring T "\nEnter Type: "))))
        (if (= blkname "")
            (setq blkname "##"))
      (setq tagname (vl-string-trim " " (strcase (getstring T "\nEnter Tag: "))))
      (setq pfeet (getint "\nEnter Feet: "))
        (if (not (numberp pfeet))
            (setq pfeet "#"))
      (setq pinch (getint "\nEnter Inches: "))
        (if (not (numberp pinch))
            (setq pinch "##"))
     
      ; set quantity of runs
      (setq ttq (getint "\n Enter Quantity <1>: "))
          (if (not ttq)
              (setq ttq 1))
     
      ; set blkname based on ttq with correct characters and check for duplicate blkname
      (if (= ttq 1)
          (setq blkname (strcat blkname "-" (if (numberp pfeet) (itoa pfeet) pfeet) "ft" (if (numberp pinch) (itoa pinch) pinch) "in" ))
          (setq blkname (strcat blkname "-" (if (numberp pfeet) (itoa pfeet) pfeet) "ft" (if (numberp pinch) (itoa pinch) pinch) "in(x" (itoa ttq) ")" ))
      ); end if
      (while (tblsearch "BLOCK" blkname)
             (setq blkname (strcat blkname "^"))
      ); end while 
     
      ; set polyline layer and color
      (vla-put-layer plyline "CDL-A-LTG-FIXTURES-BOLD")
      (vla-put-color plyline acByLayer)

      ; change plyline to entity and create list of coordinates / set counter to 0
      (setq plyline (vlax-vla-object->ename plyline)
            plylist (mapcar 'cdr (vl-remove-if '(lambda (x) (/= 10 (car x))) (entget plyline)))
            n       0
      ); end setq
     
      ; if plyline is closed, add origin point to end of plylist
      (if
        (= (vla-get-closed (vlax-ename->vla-object plyline)) :vlax-true)
        (setq plylist (append plylist (list(cdr(assoc 10 (entget plyline))))))
      );end if
     
      ;;; ============================================= START REPEAT ============================================= ;;;

      ; repeat based on length of plylist - 1
      (repeat (- (length plylist) 1)
        
        ; start conditionals for segments with or without bulges
        (cond
          
          ; condition for segment where bulge = 0
          ((= (nth n (mapcar 'cdr (vl-remove-if '(lambda (x) (/= 42 (car x))) (entget plyline)))) 0 )
           
            ; build variables
            (setq pt1  (polar (nth n plylist) (angle (nth n plylist) (nth (+ n 1) plylist)) 3)
                  n    (+ n 1)
                  pt2  (polar (nth n plylist) (angle (nth n plylist) (nth (- n 1) plylist)) 3)
                  ang1 (angle pt1 pt2)
                  ang2 (angle pt2 pt1)
            ); end setq
                  (entmake (list  (cons 0 "CIRCLE")
                                  (cons 8 "CDL-A-LTG-FIXTURES-BOLD")
                                  (cons 10 pt1)
                                  (cons 40 1.0)
                           ); end list
                  ); end entmake
                  (ssadd (entlast) sset)
                  (entmake (list  (cons 0 "CIRCLE")
                                  (cons 8 "CDL-A-LTG-FIXTURES-BOLD")
                                  (cons 10 pt2)
                                  (cons 40 1.0)
                           ); end list
                  ); end entmake
                  (ssadd (entlast) sset)
           
          ); end condition for bulge = 0
          
          ; condition for segment where bulge /= 0
          ((/= (nth n (mapcar 'cdr (vl-remove-if '(lambda (x) (/= 42 (car x))) (entget plyline)))) 0 )
           
            ; build variables
            (setq pt1     (nth n plylist)
                  bulge   (nth n (mapcar 'cdr (vl-remove-if '(lambda (x) (/= 42 (car x))) (entget plyline))))
                  n       (+ n 1)
                  pt2     (nth n plylist)
                  bulrad  (/ (* (distance pt1 pt2) (1+ (* bulge bulge))) 4 (abs bulge))   ; bulge radius
                  bulcent (polar pt1 (+ (angle pt1 pt2) (- (/ pi 2) (* 2 (atan bulge))))(/ (* (distance pt1 pt2) (1+ (* bulge bulge))) 4 bulge))   ; bulge centerpoint
            ); end setq
           
            ; start conditionals for segments with or positive or negative bulges
            (cond
              
              ; condition for segment where bulge > 0
              ((> bulge 0)
                  (entmake (list  (cons 0 "CIRCLE")
                                  (cons 8 "CDL-A-LTG-FIXTURES-BOLD")
                                  (cons 10 pt1)
                                  (cons 40 1.0)
                           ); end list
                  ); end entmake
                  (vla-rotate (vlax-ename->vla-object (entlast)) (vlax-3d-point bulcent) (/ 3 bulrad))
                  (ssadd (entlast) sset)
              
                  (entmake (list  (cons 0 "CIRCLE")
                                  (cons 8 "CDL-A-LTG-FIXTURES-BOLD")
                                  (cons 10 pt2)
                                  (cons 40 1.0)
                           ); end list
                  ); end entmake
                  (vla-rotate (vlax-ename->vla-object (entlast)) (vlax-3d-point bulcent) (- (/ 3 bulrad)))
                  (ssadd (entlast) sset)
               
              ); end condition for segment where bulge > 0
              
              ; condition for segment where bulge < 0
              ((< bulge 0)
                  (entmake (list  (cons 0 "CIRCLE")
                                  (cons 8 "CDL-A-LTG-FIXTURES-BOLD")
                                  (cons 10 pt1)
                                  (cons 40 1.0)
                           );end list
                  );end entmake
                  (vla-rotate (vlax-ename->vla-object (entlast)) (vlax-3d-point bulcent) (- (/ 3 bulrad)))
                  (ssadd (entlast) sset)

                  (entmake (list  (cons 0 "CIRCLE")
                                  (cons 8 "CDL-A-LTG-FIXTURES-BOLD")
                                  (cons 10 pt2)
                                  (cons 40 1.0)
                           );end list
                  );end entmake
                  (vla-rotate (vlax-ename->vla-object (entlast)) (vlax-3d-point bulcent) (/ 3 bulrad))
                  (ssadd (entlast) sset)
               
              ); end condition for segment where bulge < 0
              
              (t nil)
              
            ); end conditionals for segments with or positive or negative bulges
          
          ); end condition for segment where bulge /= 0
          
          (t nil)
          
        ); end conditionals for segments with or without bulges
        
      ); end repeat

      ;;; ============================================= END REPEAT ============================================= ;;;     
     
      ; reset variables for first segment
      (setq pt1   (car plylist)
            pt2   (cadr plylist)
            ang1  (angle pt1 pt2)
            ang2  (angle pt2 pt1)
            bulge (cdr(assoc 42 (entget plyline))))
      (if
        ; check if first segment is a bulge
        (= (cdr (assoc 42 (entget plyline))) 0.0)
          ; if not a bulge
          (setq ppoff 0)
          ; if a bulge, calculate height of bulge (sagitta)
          (setq ppoff (* (/ (distance pt1 pt2) 2) (abs (car (mapcar 'cdr (vl-remove-if '(lambda (x) (/= 42 (car x))) (entget plyline)))))))
      ); end if
     
      ; establish text rotation conditionals
      (cond
        
        ((<= bulge 0.0)
          ; establish insertion points for parameters when bulge < 0
          ; get midpoint between first two points and offset the distance
          (setq ppt1 (polar (polar pt1 ang1 (/ (distance pt1 pt2) 2)) (+ ang1 (*(/ pi 180)90)) (+ ppoff 5)))
          ; get midpoint between first two points and offset the distance
          (setq ppt2 (polar (polar pt1 ang1 (/ (distance pt1 pt2) 2)) (+ ang1 (*(/ pi 180)90)) (+ ppoff 10)))      
          (if
            (and
              (>  (rtos ang1 2 4) (rtos (/ pi 2) 2 4))
              (<= (rtos ang1 2 4) (rtos (* 3 (/ pi 2)) 2 4))
            )
            (setq textrot (* (+ ang1 pi) (/ 180 pi)))
            (setq textrot (* ang1 (/ 180 pi)))
          )
         
        ); end bulge < 0 condition

        ((> bulge 0.0)
          ; establish insertion points for parameters when bulge > 0
          (setq ppt1 (polar (polar pt1 ang1 (/ (distance pt1 pt2) 2)) (+ ang2 (*(/ pi 180)90)) (+ ppoff 5)))
          (setq ppt2 (polar (polar pt1 ang1 (/ (distance pt1 pt2) 2)) (+ ang2 (*(/ pi 180)90)) (+ ppoff 10)))
          (if
            (and
              (>  (rtos ang1 2 4) (rtos (/ pi 2) 2 4))
              (<= (rtos ang1 2 4) (rtos (* 3 (/ pi 2)) 2 4))
            )
            (setq textrot (* (+ ang1 pi) (/ 180 pi)))
            (setq textrot (* ang1 (/ 180 pi)))
          )
         
        ); end bulge > 0 condition
        
        (t nil)
                
      ); end text rotation conditionals
    
      ; build and insert block
      (command-s "-block" blkname pt1 sset "")
      (command "-insert" blkname pt1 "" "" "")
      (command "change" "last" "" "properties" "layer" "CDL-A-LTG-FIXTURES" "")
     
      ; extract WCS->ECS block transformation matrix
      (setq btmatrix (cdr (assoc 10 (entget (entlast)))))
     
      (command "-bedit" blkname)
      ; define ECS translation points for ppt1 ppt2
      (setq ecsppt1 (list (-(car ppt1)(car btmatrix)) (-(cadr ppt1)(cadr btmatrix)) '0.0))
      (setq ecsppt2 (list (-(car ppt2)(car btmatrix)) (-(cadr ppt2)(cadr btmatrix)) '0.0))
     
      ; insert block type text and add to selection set
      (command "text" "style" "CDL4.5-BOLD" "justify" "MC" ecsppt1 textrot (vl-string-subst "\"" "in" (vl-string-subst "'-" "ft" (vl-string-trim "^" blkname))))
      (command "change" "last" "" "properties" "annotative" "no" "layer" "CDL-A-LTG-ATTRIBUTE" "")
      (setpropertyvalue (entlast) "Height" 4.5)
      (setq param1 (entlast))
      (setq bset (ssget "_L"))
     
      ; insert block tag text and add to selection set if tagname /= nil
      (cond
        ( (/= tagname "")
          (command "text" "style" "CDL3" "justify" "MC" ecsppt2 textrot tagname)
          (command "change" "last" "" "properties" "annotative" "no" "layer" "CDL-A-LTG-ATTRIBUTE" "")
          (setpropertyvalue (entlast) "Height" 3)
          (setq param2 (entlast))
          (ssadd (entlast) bset)
        ); end condition
      ); end cond

      ; insert rotation parameter for param1 and add to bset/rset
      (command "bparameter" "rotation" ecsppt1 "10" (polar ecsppt1 (*(/ pi 180)textrot) 10) (polar ecsppt1 (*(/ pi 180)textrot) 12) "1")
      (ssadd (entlast) bset)
      (setq rset (ssadd (entlast)))
      (ssadd param1 rset)
      (sssetfirst nil (ssget "_L"))
      (command "baction" param1 "")

      ; insert move grip for param1 and add to bset
      (command "bparameter" "point" ecsppt1 (polar ecsppt1 ang1 5) "1")
      (ssadd (entlast) bset)
      (sssetfirst nil (ssget "_L"))
      (command "baction" "move" rset "")
      (setq rset nil)

      ; insert rotation parameter for param2 and add to bset/rset
      (cond
        ( (/= tagname "")
          (command "bparameter" "rotation" ecsppt2 "10" (polar ecsppt2 (*(/ pi 180)textrot) 10) (polar ecsppt2 (*(/ pi 180)textrot) 12) "1")
          (ssadd (entlast) bset)
          (setq rset (ssadd (entlast)))
          (ssadd param2 rset)
          (sssetfirst nil (ssget "_L"))
          (command "baction" param2 "")
        ); end condition
      ); end cond

      ; insert move grip for param2 and add to bset if tagname /= nil
      (cond
        ( (/= tagname "")
          (command "bparameter" "point" ecsppt2 (polar ecsppt2 ang1 10) "1")
          (ssadd (entlast) bset)
          (sssetfirst nil (ssget "_L"))
          (command "baction" "move" rset "")
        ); end condition
      ); end cond
                
      ; add flip parameter if bulge = 0
      (cond
        ( (= bulge 0)
          (command-s "bparameter" "flip" 
            ; basepoint for flip parameter is ECS translated 'pt1' + 1
            (polar (list(-(car pt1)(car btmatrix)) (-(cadr pt1)(cadr btmatrix))) ang1 6)
            ; rotation endpoint for flip parameter is ECS translated 'pt1' + 6
            (polar (list(-(car pt1)(car btmatrix)) (-(cadr pt1)(cadr btmatrix))) ang1 10)
            ;label location for flip parameter is ECS translated 'pt1' + 11
            (polar (list(-(car pt1)(car btmatrix)) (-(cadr pt1)(cadr btmatrix))) ang1 12)
            "1"
          ); end command - flip parameter
          ; select 'bset' selection set for flip parameter objects
          (sssetfirst nil (ssget "_L"))
          (command "baction" bset "")
        ); end condition
      ); end cond
     
      ; save and close block editor
      (command "bsave")
      (command "bclose")
     
    );;;;; ==================================  end "Custom" condition   =================================== ;;;;
    
    ;;;;; =================================== start "Array" condition  =================================== ;;;;
    ((= tts "Array")
     
      ; repeat request for polyline until polyline is selected or function is exited
      (setvar "ERRNO" 0)
      (while (AND (not plyline) (/= (getvar "ERRNO") 52))
                (if (AND (setq plyline (car (entsel "\nSelect Polyline: "))) (/= (cdr (assoc 0 (entget plyline))) "LWPOLYLINE"))
                    (setq plyline nil)
                ); end if
      ); end while
     
      ; start selection set with polyline
      (setq sset (ssadd plyline))
     
      ; set polyline layer and color and highlight
      (vla-put-layer (vlax-ename->vla-object plyline) "CDL-A-LTG-FIXTURES-BOLD")
      (vla-put-color (vlax-ename->vla-object plyline) acByLayer)
      (redraw plyline 3)
 
      ; set stair direction
      (initget "Down Up")
      (setq ttd (getkword "\nSet Stair Direction [Down/Up] <Down>: "))
      (if (not ttd)
          (setq ttd "Down"))
  
      ; set snap mode
      (setvar "OSMODE" 1207)
  
      ; start conditionals for straight or curved plyline
      (cond
        
        ; start condition for straight plyline
        ( (= (cdr(assoc 42 (entget plyline))) 0.0)
          
          ; prompt user to select offsets
          (while  (setq pt3     (getpoint "\nSelect Additional Stairs or <Continue>: "))
            
                  (command "offset" "Through" plyline pt3 "") 
            
                  (vla-move
                      (vlax-ename->vla-object (entlast))
                      (vlax-3d-point (append (cdr (assoc 10 (entget (entlast)))) (list 0.0)))
                      (vlax-3d-point (append 
                                        (polar 
                                          (cdr (assoc 10 (entget (entlast)))) 
                                          (angle (cdr (assoc 10 (entget plyline))) (cdr (assoc 10 (entget (entlast)))))
                                          (if (= ttd "Down") (- 1) 1)
                                        )
                                        (list 0.0)
                                     ) )
                  )
                  (redraw plyline 3)
                  (ssadd (entlast) sset)
          ); end while

          ; move plyline based on ttd value
          (vla-move
            (vlax-ename->vla-object plyline)
            (vlax-3d-point (append (cdr (assoc 10 (entget plyline))) (list 0.0)))
            (vlax-3d-point (append
                              (polar
                                (cdr (assoc 10 (entget plyline)))
                                (angle (cdr (assoc 10 (entget plyline))) (cdr (assoc 10 (entget (entlast)))))
                                (if (= ttd "Down") (- 1) 1)
                              )
                              (list 0.0)
                            ) )
          )
         
        ); end condition for straight plyline
        
        ; start condition for curved plyline
        ( (/= (cdr(assoc 42 (entget plyline))) 0.0)

          ; set variables for original plyline
          (setq pt1     (car (mapcar 'cdr (vl-remove-if '(lambda (x) (/= 10 (car x))) (entget plyline))))
                pt2     (cadr (mapcar 'cdr (vl-remove-if '(lambda (x) (/= 10 (car x))) (entget plyline))))
                pmid    (polar pt1 (angle pt1 pt2) (/ (distance pt1 pt2) 2))
                pbulge  (cdr (assoc 42 (entget plyline)))
                prad    (/ (* (distance pt1 pt2) (1+ (* pbulge pbulge))) 4 (abs pbulge))
                pcent   (polar pt1 (+ (angle pt1 pt2) (- (/ pi 2) (* 2 (atan pbulge)))) (/ (* (distance pt1 pt2) (1+ (* pbulge pbulge))) 4 pbulge))
                psag    (polar pmid (angle pcent pmid) (* (/ (distance pt1 pt2) 2) (abs (cdr (assoc 42 (entget plyline))))))
                slope   (angle (polar pt1 (angle pt1 pt2) (/ (distance pt1 pt2) 2)) pcent)
          )

          ; prompt user to select offsets
          (while  (setq pt3     (getpoint "\nSelect Additional Stairs or <Continue>: "))
            
                  ; set variables for offset lines
                  (setq pt4     (polar pt3
                                  (angle pt1 pt2)
                                    (*
                                      (*
                                        (distance pt3 (polar pt1 (angle pt1 pt2) (/ (distance pt1 pt2) 2)))
                                        (sin (- (/ pi 2) (- pi (- (angle (polar pt1 (angle pt1 pt2) (/ (distance pt1 pt2) 2)) pt3) (angle pt1 pt2)))))
                                      )
                                    2)
                                )
                        clen    (distance pt3 pt4)
                        cdist   (sqrt (- (expt prad 2) (expt (/ clen 2) 2)))
                        csag    (- prad cdist)
                        pt5     (polar (polar pt3 (angle pt3 pt4) (/ (distance pt3 pt4) 2)) (+ pi slope) csag)
                        cbulge  ((lambda ( a ) (/ (sin a) (cos a))) (/ (+ (- pi (angle pt5 pt3)) (angle pt5 pt4)) 2))
                        oslope  (angle psag pt5) )

                        ; set distance and slope depending on csag value
                        (if (= csag 0.0)
                            (setq dist  (abs (- prad (distance pt5 pcent))))
                            (setq ccent (polar pt3 (+ (angle pt3 pt4) (- (/ pi 2) (* 2 (atan cbulge)))) (/ (* (distance pt3 pt4) (1+ (* cbulge cbulge))) 4 cbulge))
                                  dist  (distance pcent ccent)) )

                  ; move new line based on dist and oslope values
                  (vla-copy (vlax-ename->vla-object plyline))
                  (vla-move
                      (vlax-ename->vla-object (entlast))
                      (vlax-3d-point (append (cdr (assoc 10 (entget plyline))) (list 0.0)))
                      (vlax-3d-point (append (polar (cdr (assoc 10 (entget plyline))) oslope (if (= ttd "Down") (1- dist)(1+ dist))) (list 0.0)))
                  )
                  (redraw plyline 3)
                  (ssadd (entlast) sset)
          
          ); end while

          ; move original plyline based on ttd value
          (vla-move
            (vlax-ename->vla-object plyline)
            (vlax-3d-point (append (cdr (assoc 10 (entget plyline))) (list 0.0)))
            (vlax-3d-point (append (polar (cdr (assoc 10 (entget plyline))) oslope (if (= ttd "Down") (- 1) 1)) (list 0.0)))
          )
         
        ); end condition for curved plyline

        (t nil) 
        
      ); end cond

      ; un-highlight plyline and reset osmode
      (redraw plyline 4)
      (setvar "OSMODE" 0)
  
      ;convert plyline to vl object
      (setq plyline (vlax-ename->vla-object plyline))
 
      ; get feet value of polyline
      (setq pfeet (fix (/(vla-get-length plyline) 12)))
      ; get inches value rounded to nearest integer
      (setq pinch (fix(+(*(-(/(vla-get-length plyline)12)(fix(/(vla-get-length plyline)12)))12)0.5)))
        (cond 
          ((= pinch 12)
            (setq pinch 0)
            (setq pfeet (+ pfeet 1))
          )
          (t nil)
        ); end cond
     
      ; set block type
      (setq blkname (vl-string-trim " " (strcase (getstring T "\nEnter Type: "))))
          (if (= blkname "")
              (setq blkname "##"))
      
      ; set tag name
      (setq tagname (vl-string-trim " " (strcase (getstring T "\nEnter Tag: "))))
      
      ; set quantity of runs
      (setq ttq (sslength sset))
          (if (not ttq)
              (setq ttq 1))

      ; set blkname based on ttq with correct characters and check for duplicate blkname
      (if (= ttq 1)
          (setq blkname (strcat blkname "-" (itoa pfeet) "ft" (itoa pinch) "in" ))
          (setq blkname (strcat blkname "-" (itoa pfeet) "ft" (itoa pinch) "in(x" (itoa ttq) ")" ))
      ); end if
        (while (tblsearch "BLOCK" blkname)
               (setq blkname (strcat blkname "^"))
        ); end while

      ; create new selection set with entities from sset
      (setq n 0)
      (setq plyset (ssadd))
      (repeat (sslength sset)
              (ssadd (ssname sset n) plyset)
              (setq n (1+ n))
      )
      (setq n 0)

      ;;; ============================================= START REPEAT ============================================= ;;;
 
      ; repeat based on length plyset
      (repeat (sslength plyset)
        
        ; start conditionals for segments with or without bulges
        (cond
          
          ; condition for segment where bulge = 0
          ((= (cdr (assoc 42 (entget (ssname plyset n)))) 0.0)
           
            ; build variables
            (setq pt1   (polar 
                          (car (mapcar 'cdr (vl-remove-if '(lambda (x) (/= 10 (car x))) (entget (ssname plyset n)))))
                          (angle
                            (car (mapcar 'cdr (vl-remove-if '(lambda (x) (/= 10 (car x))) (entget (ssname plyset n)))))
                            (cadr (mapcar 'cdr (vl-remove-if '(lambda (x) (/= 10 (car x))) (entget (ssname plyset n)))))
                          )
                          3
                        )
                  pt2   (polar 
                          (cadr (mapcar 'cdr (vl-remove-if '(lambda (x) (/= 10 (car x))) (entget (ssname plyset n)))))
                          (angle
                            (cadr (mapcar 'cdr (vl-remove-if '(lambda (x) (/= 10 (car x))) (entget (ssname plyset n)))))
                            (car (mapcar 'cdr (vl-remove-if '(lambda (x) (/= 10 (car x))) (entget (ssname plyset n)))))
                          )
                          3
                        )
                  n     (1+ n)
            ); end setq
           
                  (entmake (list  (cons 0 "CIRCLE")
                                  (cons 8 "CDL-A-LTG-FIXTURES-BOLD")
                                  (cons 10 pt1)
                                  (cons 40 1.0)
                           ); end list
                  ); end entmake
                  (ssadd (entlast) sset)
                  (entmake (list  (cons 0 "CIRCLE")
                                  (cons 8 "CDL-A-LTG-FIXTURES-BOLD")
                                  (cons 10 pt2)
                                  (cons 40 1.0)
                           ); end list
                  ); end entmake
                  (ssadd (entlast) sset)
           
          ); end condition for bulge = 0
     
          ; condition for segment where bulge /= 0
          ((/= (cdr (assoc 42 (entget (ssname plyset n)))) 0.0)

            ; build variables
            (setq pt1     (car (mapcar 'cdr (vl-remove-if '(lambda (x) (/= 10 (car x))) (entget (ssname plyset n)))))
                  pt2     (cadr (mapcar 'cdr (vl-remove-if '(lambda (x) (/= 10 (car x))) (entget (ssname plyset n)))))
                  bulge   (cdr (assoc 42 (entget (ssname plyset n))))
                  bulrad  (/ (* (distance pt1 pt2) (1+ (* bulge bulge))) 4 (abs bulge))
                  bulcent (polar pt1 (+ (angle pt1 pt2) (- (/ pi 2) (* 2 (atan bulge))))(/ (* (distance pt1 pt2) (1+ (* bulge bulge))) 4 bulge))
                  n       (1+ n)
            ); end setq

            ; start conditionals for segments with or positive or negative bulges
            (cond
              
              ; condition for segment where bulge > 0
              ((> bulge 0)
                  (entmake (list  (cons 0 "CIRCLE")
                                  (cons 8 "CDL-A-LTG-FIXTURES-BOLD")
                                  (cons 10 pt1)
                                  (cons 40 1.0)
                           ); end list
                  ); end entmake
                  (vla-rotate (vlax-ename->vla-object (entlast)) (vlax-3d-point bulcent) (/ 3 bulrad))
                  (ssadd (entlast) sset)
              
                  (entmake (list  (cons 0 "CIRCLE")
                                  (cons 8 "CDL-A-LTG-FIXTURES-BOLD")
                                  (cons 10 pt2)
                                  (cons 40 1.0)
                           ); end list
                  ); end entmake
                  (vla-rotate (vlax-ename->vla-object (entlast)) (vlax-3d-point bulcent) (- (/ 3 bulrad)))
                  (ssadd (entlast) sset)
               
              ); end condition for segment where bulge > 0
              
              ; condition for segment where bulge < 0
              ((< bulge 0)
                  (entmake (list  (cons 0 "CIRCLE")
                                  (cons 8 "CDL-A-LTG-FIXTURES-BOLD")
                                  (cons 10 pt1)
                                  (cons 40 1.0)
                           );end list
                  );end entmake
                  (vla-rotate (vlax-ename->vla-object (entlast)) (vlax-3d-point bulcent) (- (/ 3 bulrad)))
                  (ssadd (entlast) sset)

                  (entmake (list  (cons 0 "CIRCLE")
                                  (cons 8 "CDL-A-LTG-FIXTURES-BOLD")
                                  (cons 10 pt2)
                                  (cons 40 1.0)
                           );end list
                  );end entmake
                  (vla-rotate (vlax-ename->vla-object (entlast)) (vlax-3d-point bulcent) (/ 3 bulrad))
                  (ssadd (entlast) sset)
              
              ); end condition for segment where bulge < 0
              
              (t nil)
              
            ); end conditionals for segments with or positive or negative bulges
          
          ); end condition for segment where bulge /= 0
          
          (t nil)
        
        ); end conditionals for segments with or without bulges
        
      ); end repeat

      ;;; ============================================= END REPEAT ============================================= ;;;
     
      ; reset variables for first segment
      (setq plyline (vlax-vla-object->ename plyline)
            pt1     (car (mapcar 'cdr (vl-remove-if '(lambda (x) (/= 10 (car x))) (entget plyline))))
            pt2     (cadr (mapcar 'cdr (vl-remove-if '(lambda (x) (/= 10 (car x))) (entget plyline))))
            ang1    (angle pt1 pt2)
            ang2    (angle pt2 pt1) )
     
      (cond
        ; plyline is straight condition
        ( (= (cdr (assoc 42 (entget plyline))) 0.0)
          (setq oslope (angle (cdr (assoc 10 (entget (ssname plyset 0)))) (cdr (assoc 10 (entget (ssname plyset 1))))))
          (setq ppt1 (polar (polar pt1 ang1 (/ (distance pt1 pt2) 2)) (- oslope pi) 5))
          (setq ppt2 (polar (polar pt1 ang1 (/ (distance pt1 pt2) 2)) (- oslope pi) 10))
          (if
            (and
              (>  (rtos ang1 2 4) (rtos (/ pi 2) 2 4))
              (<= (rtos ang1 2 4) (rtos (* 3 (/ pi 2)) 2 4))
            )
            (setq textrot (* (+ ang1 pi) (/ 180 pi)))
            (setq textrot (* ang1 (/ 180 pi)))
          )
        )
        ; plyline is an arc condition
        ( (/= (cdr (assoc 42 (entget plyline))) 0.0)
          (setq ppt1 (polar psag (- oslope pi) 5))
          (setq ppt2 (polar psag (- oslope pi) 10))
          (if
            (and
              (>  (rtos ang1 2 4) (rtos (/ pi 2) 2 4))
              (<= (rtos ang1 2 4) (rtos (* 3 (/ pi 2)) 2 4))
            )
            (setq textrot (* (+ ang1 pi) (/ 180 pi)))
            (setq textrot (* ang1 (/ 180 pi)))
          )
        )
        (t nil) 
      ); end cond
    
      ; build and insert block
      (command-s "-block" blkname pt1 sset "")
      (command "-insert" blkname pt1 "" "" "")
      (command "change" "last" "" "properties" "layer" "CDL-A-LTG-FIXTURES" "")
     
      ; extract WCS->ECS block transformation matrix
      (setq btmatrix (cdr (assoc 10 (entget (entlast)))))
     
      (command "-bedit" blkname)
      ; define ECS translation points for ppt1 ppt2
      (setq ecsppt1 (list (-(car ppt1)(car btmatrix)) (-(cadr ppt1)(cadr btmatrix)) '0.0))
      (setq ecsppt2 (list (-(car ppt2)(car btmatrix)) (-(cadr ppt2)(cadr btmatrix)) '0.0))
     
      ; insert block type text and add to selection set
      (command "text" "style" "CDL4.5-BOLD" "justify" "MC" ecsppt1 textrot (vl-string-subst "\"" "in" (vl-string-subst "'-" "ft" (vl-string-trim "^" blkname))))
      (command "change" "last" "" "properties" "annotative" "no" "layer" "CDL-A-LTG-ATTRIBUTE" "")
      (setpropertyvalue (entlast) "Height" 4.5)
      (setq param1 (entlast))
      (setq bset (ssget "_L"))
     
      ; insert block tag text and add to selection set if tagname /= nil
      (cond
        ( (/= tagname "")
          (command "text" "style" "CDL3" "justify" "MC" ecsppt2 textrot tagname)
          (command "change" "last" "" "properties" "annotative" "no" "layer" "CDL-A-LTG-ATTRIBUTE" "")
          (setpropertyvalue (entlast) "Height" 3)
          (setq param2 (entlast))
          (ssadd (entlast) bset)
        ); end condition
      ); end cond

      ; insert rotation parameter for param1 and add to bset/rset
      (command "bparameter" "rotation" ecsppt1 "10" (polar ecsppt1 (*(/ pi 180)textrot) 10) (polar ecsppt1 (*(/ pi 180)textrot) 12) "1")
      (ssadd (entlast) bset)
      (setq rset (ssadd (entlast)))
      (ssadd param1 rset)
      (sssetfirst nil (ssget "_L"))
      (command "baction" param1 "")

      ; insert move grip for param1 and add to bset
      (command "bparameter" "point" ecsppt1 (polar ecsppt1 ang1 5) "1")
      (ssadd (entlast) bset)
      (sssetfirst nil (ssget "_L"))
      (command "baction" "move" rset "")
      (setq rset nil)

      ; insert rotation parameter for param2 and add to bset/rset
      (cond
        ( (/= tagname "")
          (command "bparameter" "rotation" ecsppt2 "10" (polar ecsppt2 (*(/ pi 180)textrot) 10) (polar ecsppt2 (*(/ pi 180)textrot) 12) "1")
          (ssadd (entlast) bset)
          (setq rset (ssadd (entlast)))
          (ssadd param2 rset)
          (sssetfirst nil (ssget "_L"))
          (command "baction" param2 "")
        ); end condition
      ); end cond

      ; insert move grip for param2 and add to bset if tagname /= nil
      (cond
        ( (/= tagname "")
          (command "bparameter" "point" ecsppt2 (polar ecsppt2 ang1 10) "1")
          (ssadd (entlast) bset)
          (sssetfirst nil (ssget "_L"))
          (command "baction" "move" rset "")
        ); end condition
      ); end cond

      ; save and close block editor
      (command "bsave")
      (command "bclose")
     
    );;;;; ==================================  end "Array" condition   =================================== ;;;;
    
    (t nil) 
    
  ); end cond

; revert system variables
(setvar "OSMODE" oldsnap)
(setvar "CLAYER" oldlayer)  
(setvar "CMDECHO" oldecho)

; end undo mark
(vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))

(princ)

); end defun