(defun c:TB

( /
  *error*
  oldsnap oldlayer oldecho
  ename bname ecent elist plist
  ptmin ptmax tbox trim
  conrit contop conlef conbot conswl
  pt1 pt2 pt3 pt4 pt5 
  slopeh slopev
  attcent attvis bulge
  swleg con
  ang1 ang2 dist1 dist2 look1 flip1 pos1 brot
  bscal sscal
  side
)

; error function
(defun *error* ( msg )

  (redraw ename 4)
  (vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))
  (if oldsnap  (setvar "OSMODE" oldsnap))
  (if oldlayer (setvar "CLAYER" oldlayer))
  (if oldecho  (setvar "CMDECHO" oldecho))
  
  (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
  )
  (princ)
); end defun *error*

(vl-load-com)

; continuously prompt for block selection
(setvar "ERRNO" 0)
(while  (AND  (not ename) (/= (getvar "ERRNO") 52))
        (if   (AND (setq ename (car (entsel "\nSelect Block: "))) (/= (cdr (assoc 0 (entget ename))) "INSERT"))
              (setq ename nil)
        ); end if
)

; set block center and block rotation
(setq ecent (cdr (assoc 10 (entget ename)))
      brot  (vla-get-rotation (vlax-ename->vla-object ename)) 
      bscal (if (not (cdr (assoc 4 (entget (tblobjname "BLOCK" (vla-get-effectivename (vlax-ename->vla-object ename)))))))
                "1-4\""
                (cdr (assoc 4 (entget (tblobjname "BLOCK" (vla-get-effectivename (vlax-ename->vla-object ename))))))
            )
      bscal (cond
              ( (= bscal "1-32\"") 8.0          )    ;   8
              ( (= bscal "1-16\"") 4.0          )    ;   4
              ( (= bscal "3-32\"") (/ 8.0 3.0)  )    ; ~ 2.666
              ( (= bscal "1-8\"" ) 2.0          )    ;   2
              ( (= bscal "3-16\"") (/ 4.0 3.0)  )    ; ~ 1.333
              ( (= bscal "1-4\"" ) 1.0          )    ;   1
              ( (= bscal "3-8\"" ) (/ 2.0 3.0)  )    ; ~ 0.666
              ( (= bscal "1-2\"" ) 0.5          )    ;   0.5
              ( (= bscal "3-4\"" ) (/ 1.0 3.0)  )    ; ~ 0.333
              ( (= bscal "1.0\"" ) 0.25         )    ;   0.25
              ( (= bscal "1.5\"" ) (/ 1.0 6.0)  )    ; ~ 0.166
              ( (= bscal "3.0\"" ) (/ 1.0 12.0) )    ; ~ 0.083
              ( t nil )
            ); end cond
      sscal (if (tblsearch "BLOCK" "SWITCHLEG")
                (if (not (cdr (assoc 4 (entget (tblobjname "BLOCK" "SWITCHLEG")))))
                    "1-4\""
                    (cdr (assoc 4 (entget (tblobjname "BLOCK" "SWITCHLEG"))))
                )
                "1-4\""
            )
      sscal (cond
              ( (= sscal "1-32\"") 8.0          )    ;   8
              ( (= sscal "1-16\"") 4.0          )    ;   4
              ( (= sscal "3-32\"") (/ 8.0 3.0)  )    ; ~ 2.666
              ( (= sscal "1-8\"" ) 2.0          )    ;   2
              ( (= sscal "3-16\"") (/ 4.0 3.0)  )    ; ~ 1.333
              ( (= sscal "1-4\"" ) 1.0          )    ;   1
              ( (= sscal "3-8\"" ) (/ 2.0 3.0)  )    ; ~ 0.666
              ( (= sscal "1-2\"" ) 0.5          )    ;   0.5
              ( (= sscal "3-4\"" ) (/ 1.0 3.0)  )    ; ~ 0.333
              ( (= sscal "1.0\"" ) 0.25         )    ;   0.25
              ( (= sscal "1.5\"" ) (/ 1.0 6.0)  )    ; ~ 0.166
              ( (= sscal "3.0\"" ) (/ 1.0 12.0) )    ; ~ 0.083
              ( t nil )
            ); end cond
); end setq

;highlight block
(redraw ename 3)

; start undo mark
(vla-startundomark (vla-get-activedocument (vlax-get-acad-object)))

; save and update system variables
(setq oldsnap   (getvar "OSMODE")
      oldecho   (getvar "CMDECHO")
      oldlayer  (getvar "CLAYER") )
(setvar "CMDECHO" 0)
(setvar "OSMODE" 0)

; load required layers and linetypes
(if (= (tblsearch "ltype" "CENTER2") nil)
    (command "-linetype" "load" "CENTER2" "acad.lin" "") )
(if (= (tblsearch "layer" "CDL-A-LTG-WIRING" ) nil)
    (command "-layer" "Make" "CDL-A-LTG-WIRING" "color" "6" "" "ltype" "CENTER2" "" "") )
(if (= (tblsearch "layer" "CDL-A-LTG-WIPEOUT" ) nil)
    (command "-layer" "Make" "CDL-A-LTG-WIPEOUT" "color" "255" "" "ltype" "continuous" "" "") )

;start conditions for block types
(cond

  ; start "CFAN" condition
  ( (= (vla-get-effectivename (vlax-ename->vla-object ename)) "CFAN")
   
    (setq ang1    (- (getpropertyvalue ename "AcDbDynBlockPropertyAngle1") 0.314158)
          conrit  (polar ecent 0.0 3)
          pt5     (polar ecent 0.0 18)
          conswl  (polar pt5 pi 7) )
   
    (command "_INSERT" "SWLEG-" pt5 "" "" "")
    (command "_EXPLODE" "L")
    (setq swleg (vlax-ename->vla-object (entlast)))
    (vla-put-layer swleg "CDL-A-LTG-WIRING")
   
    (vla-rotate swleg (vlax-3d-point ecent) (+ brot ang1))
    
    (if
      (AND
        (>  (rtos (vla-get-rotation swleg) 2 4) (rtos (/ pi 2) 2 4))
        (<= (rtos (vla-get-rotation swleg) 2 4) (rtos (* 3 (/ pi 2)) 2 4))
      )
      (vla-rotate swleg (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))) pi)
    )
   
    (entmake (list  (cons 0 "LWPOLYLINE")
                    (cons 100 "AcDbEntity")
                    (cons 100 "AcDbPolyline")
                    (cons 8 "CDL-A-LTG-WIRING")
                    (cons 90 2)
                    (cons 70 0)
                    (cons 10 conrit)
                    (cons 42           
                      (if (AND
                            (>  (rtos (angle ecent (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))) 2 4) (rtos (/ pi 2.0) 2 4))
                            (<= (rtos (angle ecent (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))) 2 4) (rtos (* 3 (/ pi 2)) 2 4))
                          )
                        0.105104
                        -0.105104
                      )
                    )
                    (cons 10 conswl)
            ); end list
    ); end entmake
    (setq con (vlax-ename->vla-object (entlast)))
   
    (vla-rotate con (vlax-3d-point ecent) (+ brot ang1))
  
    (vla-scaleentity con    (vlax-3d-point ecent) bscal)
    (vla-scaleentity swleg  (vlax-3d-point ecent) bscal)
    (vla-scaleentity swleg  (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))) (/ 1.0 bscal))
    (if (not (= bscal 1.0)) (vla-resetblock swleg))
   
  ); end "CFAN" condition
  
  ; start "CJBOX" condition
  ( (= (vla-get-effectivename (vlax-ename->vla-object ename)) "CJBOX")
   
    (setq conrit  (polar ecent 0.0 3.25)
          pt5     (polar ecent (/ pi 2) 10)
          conswl  (polar (polar pt5 0.0 6.27674) (* (/ pi 2) 3) 2.13067) )
   
    (command "_INSERT" "SWLEG-" pt5 "" "" "")
    (command "_EXPLODE" "L")
    (setq swleg (vlax-ename->vla-object (entlast)))
    (vla-put-layer swleg "CDL-A-LTG-WIRING")
   
    (vla-rotate swleg (vlax-3d-point ecent) brot)
    
    (if
      (AND
        (>  (rtos (vla-get-rotation swleg) 2 4) (rtos (/ pi 2) 2 4))
        (<= (rtos (vla-get-rotation swleg) 2 4) (rtos (* 3 (/ pi 2)) 2 4))
      )
      (vla-rotate swleg (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))) pi)
    )
   
    (entmake (list  (cons 0 "LWPOLYLINE")
                    (cons 100 "AcDbEntity")
                    (cons 100 "AcDbPolyline")
                    (cons 8 "CDL-A-LTG-WIRING")
                    (cons 90 2)
                    (cons 70 0)
                    (cons 10 conrit)
                    (cons 42 0.7)
                    (cons 10 conswl)
            ); end list
    ); end entmake
    (setq con (vlax-ename->vla-object (entlast)))
   
    (vla-rotate con (vlax-3d-point ecent) brot)

    (setq side "Swap")
    
    (while side
      (initget "Flip")
      (setq side (getkword "\nFlip Side or Exit [Flip] <Exit>: "))
        (if side
          (progn
            (setq slopeh (angle (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))) ecent))
            (vla-move
              swleg
              (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))))
              (vlax-3d-point (polar (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))) slopeh 20))
            )
            (vla-mirror
              con
              (vlax-3d-point ecent)
              (vlax-3d-point (polar ecent brot 3.25))
            )
            (vla-delete con)
            (setq con (vlax-ename->vla-object (entlast)))
          ); end progn
        ); end if
    ); end while
   
    (vla-scaleentity con    (vlax-3d-point ecent) bscal)
    (vla-scaleentity swleg  (vlax-3d-point ecent) bscal)
    (vla-scaleentity swleg  (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))) (/ 1.0 bscal))
    (if (not (= bscal 1.0)) (vla-resetblock swleg))
   
  ); end "CJBOX" condition

  ; start "DUPLEX" condition
  ( (= (vla-get-effectivename (vlax-ename->vla-object ename)) "DUPLEX")

    (setq ang1    (getpropertyvalue ename "AcDbDynBlockPropertyAngle1")
          conbot  (polar ecent (/ pi 2) 3)
          pt5     (cond
                    ( (= (rtos ang1 2 4) (rtos (/ pi 2) 2 4))
                      (polar (polar ecent 0.0 8.75) (/ pi 2) 9.0)
                    )
                    ( (= (rtos ang1 2 4) (rtos (* (/ pi 2) 3) 2 4))
                      (polar (polar ecent pi 8.75) (/ pi 2) 9.0)
                    )
                    ( T
                      (polar (polar ecent (/ pi 2) 6) 0 12.55)
                    )
                  ); end cond
          conswl  (cond
                    ( (= (rtos ang1 2 4) (rtos (/ pi 2) 2 4))
                      (polar (polar pt5 (* 3 (/ pi 2)) 5.97487) pi 2.47487)
                    )
                    ( (= (rtos ang1 2 4) (rtos (* (/ pi 2) 3) 2 4))
                      (polar (polar pt5 (* 3 (/ pi 2)) 5.97487) 0.0 2.47487)
                    )
                    ( T
                      (polar (polar pt5 pi 5.63067) (* 3 (/ pi 2)) 2.77674)
                    )
                  ); end cond
    ); end setq
   
    (command "_INSERT" "SWLEG-" pt5 "" "" "")
    (command "_EXPLODE" "L")
    (setq swleg (vlax-ename->vla-object (entlast)))
    (vla-put-layer swleg "CDL-A-LTG-WIRING")
   
    (entmake  (list (cons 0 "LWPOLYLINE")
                    (cons 100 "AcDbEntity")
                    (cons 100 "AcDbPolyline")
                    (cons 8 "CDL-A-LTG-WIRING")
                    (cons 90 2)
                    (cons 70 0)
                    (cons 10 conbot)
                    (cons 42 (cond
                              ( (= (rtos ang1 2 4) (rtos (/ pi 2) 2 4))
                                0.278
                              )
                              ( (= (rtos ang1 2 4) (rtos (* (/ pi 2) 3) 2 4))
                                -0.278
                              )
                              ( T
                                0.218
                              )
                             ); end cond
                    ); end cons
                    (cons 10 conswl)
              ); end list
    ); end entmake
    (setq con (vlax-ename->vla-object (entlast)))

    (cond
      ( (= (rtos ang1 2 4) (rtos (/ pi 2) 2 4))
        (vla-rotate swleg (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))) (* (/ pi 2) 3))
      )
      ( (= (rtos ang1 2 4) (rtos (* (/ pi 2) 3) 2 4))
        (vla-rotate swleg (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))) (/ pi 2))
      )
      ( t nil )
    ); end cond
   
    (vla-rotate swleg (vlax-3d-point ecent) (+ brot ang1))

    (if
      (AND
        (>  (rtos (vla-get-rotation swleg) 2 4) (rtos (/ pi 2) 2 4))
        (<= (rtos (vla-get-rotation swleg) 2 4) (rtos (* 3 (/ pi 2)) 2 4))
      )
      (vla-rotate swleg (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))) pi)
    )

    (vla-rotate con (vlax-3d-point ecent) (+ brot ang1))

    (setq side "Swap")

    (while side
      (initget "Flip")
      (setq side (getkword "\nFlip Side or Exit [Flip] <Exit>: "))
        (if side
          (progn
            (setq slopeh  (angle 
                            (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))) 
                            (polar ecent (+ (+ brot ang1) (/ pi 2)) (if (OR 
                                                                          (= (rtos ang1 2 4) (rtos (/ pi 2) 2 4)) 
                                                                          (= (rtos ang1 2 4) (rtos (* (/ pi 2) 3) 2 4))
                                                                        )
                                                                        9 6
                                                                    ); end if
                            ); end polar
                          ); end angle
            ); end setq

            (vla-move
              swleg
              (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))))
              (vlax-3d-point (polar 
                                (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))) 
                                slopeh
                                (if (OR 
                                      (= (rtos ang1 2 4) (rtos (/ pi 2) 2 4)) 
                                      (= (rtos ang1 2 4) (rtos (* (/ pi 2) 3) 2 4))
                                    )
                                    17.5 25.1
                                ); end if
                             ); end polar
              ); end vlax-3d-point
            ); end vla-move
     
            (vla-mirror
              con
              (vlax-3d-point ecent)
              (vlax-3d-point (polar ecent (+ (+ brot ang1) (/ pi 2)) 6))
            )
            (vla-delete con)
            (setq con (vlax-ename->vla-object (entlast)))
          ); end progn
        ); end if
    ); end while

    (vla-scaleentity con    (vlax-3d-point ecent) bscal)
    (vla-scaleentity swleg  (vlax-3d-point ecent) bscal)
    (vla-scaleentity swleg  (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))) (/ 1.0 bscal))
    (if (not (= bscal 1.0)) (vla-resetblock swleg))
   
  ); end "DUPLEX" condition

  ; start "EXHAUST FAN / FP IGN" condition
  ( (OR (= (vla-get-effectivename (vlax-ename->vla-object ename)) "EXHAUST FAN") (= (vla-get-effectivename (vlax-ename->vla-object ename)) "FP IGN") )
   
    (setq attvis
      (vl-some '(lambda ( x ) 
                  (if (= "VISIBILITY1" (strcase (vla-get-propertyname x))) 
                      (vlax-get x 'value))
                )
                (vlax-invoke (vlax-ename->vla-object ename) 'getdynamicblockproperties)
      )
    )

    (setq ang1    (if (= (vla-get-effectivename (vlax-ename->vla-object ename)) "EXHAUST FAN")
                      (if (= attvis "Wall")
                          (getpropertyvalue ename "AcDbDynBlockPropertyAngle2")
                          (getpropertyvalue ename "AcDbDynBlockPropertyAngle1") )
                      0.0
                  )
          conrit  (polar ecent 0.0 4)
          pt5     (polar ecent (/ pi 2) (if (= attvis "Wall") 8 10))
          conswl  (polar (polar pt5 0.0 6.27674) (* 3 (/ pi 2)) 2.13067) )
      
    (command "_INSERT" "SWLEG-" pt5 "" "" "")
    (command "_EXPLODE" "L")
    (setq swleg (vlax-ename->vla-object (entlast)))
    (vla-put-layer swleg "CDL-A-LTG-WIRING")
      
    (entmake (list  (cons 0 "LWPOLYLINE")
                    (cons 100 "AcDbEntity")
                    (cons 100 "AcDbPolyline")
                    (cons 8 "CDL-A-LTG-WIRING")
                    (cons 90 2)
                    (cons 70 0)
                    (cons 10 conrit)
                    (cons 42 0.6865)
                    (cons 10 conswl)
              ); end list
    ); end entmake
    (setq con (vlax-ename->vla-object (entlast)))
    
    (vla-rotate swleg (vlax-3d-point ecent) (+ brot ang1))
   
    (if
      (AND
        (>  (rtos (vla-get-rotation swleg) 2 4) (rtos (/ pi 2) 2 4))
        (<= (rtos (vla-get-rotation swleg) 2 4) (rtos (* 3 (/ pi 2)) 2 4))
      )
      (vla-rotate swleg (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))) pi)
    )
    
    (vla-rotate con (vlax-3d-point ecent) (+ brot ang1))
   
    (setq side "Swap")
    
    (while side
      (initget "Flip")
      (setq side (getkword "\nFlip Side or Exit [Flip] <Exit>: "))
        (if side
          (progn
            (setq slopeh (angle (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))) ecent))
            (vla-move
              swleg
              (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))))
              (vlax-3d-point (polar (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))) slopeh (if (= attvis "Wall") 16 20)))
            )
            (vla-mirror
              con
              (vlax-3d-point ecent)
              (vlax-3d-point (polar ecent (+ brot ang1) 4))
            )
            (vla-delete con)
            (setq con (vlax-ename->vla-object (entlast)))
          ); end progn
        ); end if
    ); end while
  
    (vla-scaleentity con    (vlax-3d-point ecent) bscal)
    (vla-scaleentity swleg  (vlax-3d-point ecent) bscal)
    (vla-scaleentity swleg  (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))) (/ 1.0 bscal))
    (if (not (= bscal 1.0)) (vla-resetblock swleg))
   
  ); end "EXHAUST FAN / FP IGN" condition
  
  ; start "FP" condition
  ( (= (vla-get-effectivename (vlax-ename->vla-object ename)) "FP")
   
    (setq plist (vlax-safearray->list (vlax-variant-value (vla-getdynamicblockproperties (vlax-ename->vla-object ename)))))
    (foreach p plist
        (if (= (vlax-get-property p "PropertyName") "Lookup1") (setq look1 "Lookup1")) )
    (if (not look1)
        (progn (alert "\nNew 'FP' block only!") (exit)) )
   
    (setq ang1    (if (vl-catch-all-error-p (vl-catch-all-apply 'getpropertyvalue (list ename "AcDbDynBlockPropertyAngle1")))
                      0.0
                      (getpropertyvalue ename "AcDbDynBlockPropertyAngle1")
                  )
          look1   (if (vl-catch-all-error-p (vl-catch-all-apply 'getpropertyvalue (list ename "AcDbDynBlockPropertyLookup1")))
                      "Custom"
                      (getpropertyvalue ename "AcDbDynBlockPropertyLookup1")
                  )
    ); end setq
  
    (setq attvis
      (vl-some '(lambda ( x ) 
                  (if (= "VISIBILITY1" (strcase (vla-get-propertyname x))) 
                      (vlax-get x 'value))
                )
                (vlax-invoke (vlax-ename->vla-object ename) 'getdynamicblockproperties)
      )
    )
   
    (setq bname (tblobjname "BLOCK" (vla-get-effectivename (vlax-ename->vla-object ename))))
   
    (while
      (setq bname (entnext bname))
      (setq elist (cons bname elist))
    )
   
    (foreach e elist
      (if (= (cdr (assoc 1 (entget e))) "FP")
          (progn
            (setq pt1 
              (list
                (+ (car (cdr (assoc 10 (entget e))))(car ecent)) 
                (+ (cadr (cdr (assoc 10 (entget e))))(cadr ecent))
                (+ (caddr (cdr (assoc 10 (entget e))))(caddr ecent))
              )
            ); end setq
            (setq tbox (textbox (entget e)))
          ); end progn
      ); end if
    ); end foreach
    (setq pt2     (list
                    (+ (car pt1)(car (cadr tbox)))
                    (+ (cadr pt1)(cadr (cadr tbox)))
                    (+ (caddr pt1)(caddr (cadr tbox))) )
          pt3     (list (car pt1)(cadr pt2))
          pt4     (list (car pt2)(cadr pt1))
          conrit  (polar ecent 0.0 3)
          contop  (polar ecent (/ pi 2) (if (= attvis "HALF HOT (REC)") 2 3))
          conlef  (polar ecent pi 3)
          conbot  (polar ecent (* (/ pi 2) 3) (if (= attvis "HALF HOT (REC)") 2 3))
    ); end setq

    (cond
      ( (= look1 "RIGHT")
        (setq pt5     (polar (polar ecent (/ pi 2) 8.35) 0.0 9.6))
        (setq conswl  (polar (polar pt5 pi 6.88074) (* (/ pi 2) 3) 0.905867))
      )
      ( (= look1 "UP")
        (setq pt5     (polar (polar ecent 0.0 12.25) (/ pi 2) 7.25))
        (setq conswl  (polar (polar pt5 pi 3.95684) (* (/ pi 2) 3) 3.47006))
      )
      ( (= look1 "LEFT")
        (setq pt5     (polar (polar ecent (/ pi 2) 8.35) pi 9.6))
        (setq conswl  (polar (polar pt5 0.0 6.88074) (* (/ pi 2) 3) 0.905867))
      )
      ( (= look1 "DOWN")
        (setq pt5     (polar (polar ecent 0.0 12.25) (* (/ pi 2) 3) 7.25))
        (setq conswl  (polar (polar pt5 pi 3.95684) (/ pi 2) 3.47006))
      )
      ( (= look1 "Custom")
        (setq pt5     (polar 
                        (list
                          (+ (getpropertyvalue ename "AcDbDynBlockPropertyPosition1 X") (car ecent))
                          (+ (getpropertyvalue ename "AcDbDynBlockPropertyPosition1 Y") (cadr ecent))
                          (+ 0.0 (caddr ecent))
                        )
                        0.0
                        12
                      ); end polar
        ); end setq
      )
      ( t nil )
    ); end cond

    (command "_INSERT" "SWLEG-" pt5 "" "" "")
    (command "_EXPLODE" "L")
    (setq swleg (vlax-ename->vla-object (entlast)))
    (vla-put-layer swleg "CDL-A-LTG-WIRING")
   
    (vla-rotate swleg (vlax-3d-point ecent) brot)
   
    (if conswl
      (progn
        (entmake (list  (cons 0 "LWPOLYLINE")
                        (cons 100 "AcDbEntity")
                        (cons 100 "AcDbPolyline")
                        (cons 8 "CDL-A-LTG-WIRING")
                        (cons 90 2)
                        (cons 70 0)
                        (cons 10 
                              (cond
                                ( (OR (= look1 "RIGHT") (= look1 "LEFT"))
                                  contop
                                )
                                ( (OR (= look1 "UP") (= look1 "DOWN"))
                                  conrit
                                )
                                (t nil) 
                              ) )
                        (cons 42 
                              (cond
                                ( (OR (= look1 "RIGHT") (= look1 "DOWN"))
                                  -0.365
                                )
                                ( (OR (= look1 "UP") (= look1 "LEFT"))
                                  0.365
                                )
                                (t nil) 
                              ) )
                        (cons 10 conswl)
                  ); end list
        ); end entmake
        (setq con (vlax-ename->vla-object (entlast)))
      
        (vla-rotate con (vlax-3d-point ecent) brot)
      ); end progn
    ); end if

    (if conswl
      (progn
        (setq side "Swap")
        (while side
          (initget "Flip")
          (setq side (getkword "\nFlip Side or Exit [Flip] <Exit>: "))
            (if side
              (progn
                (cond
                  ( (OR (= look1 "RIGHT") (= look1 "LEFT"))
                  
                    (setq slopeh (angle (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))) (polar ecent brot (if (= look1 "RIGHT") 9.6 -9.6))))
                    (vla-move
                      swleg
                      (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))))
                      (vlax-3d-point (polar (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))) slopeh 16.7))
                    )
                    (vla-mirror
                      con
                      (vlax-3d-point ecent)
                      (vlax-3d-point (polar ecent brot (if (= look1 "RIGHT") 9.6 -9.6)))
                    )
                    (vla-delete con)
                    (setq con (vlax-ename->vla-object (entlast))) 
                  
                  ); end RIGHT or LEFT condition

                  ( (OR (= look1 "UP") (= look1 "DOWN"))
                  
                    (setq slopeh (angle (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))) (polar ecent (+ brot (/ pi 2)) (if (= look1 "UP") 7.25 -7.25))))
                    (vla-move
                      swleg
                      (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))))
                      (vlax-3d-point (polar (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))) slopeh 24.5))
                    )
                    (vla-mirror
                      con
                      (vlax-3d-point ecent)
                      (vlax-3d-point (polar ecent (+ brot (/ pi 2)) (if (= look1 "UP") 7.25 -7.25)))
                    )
                    (vla-delete con)
                    (setq con (vlax-ename->vla-object (entlast))) 
                  
                  ); end UP or DOWN condition
                  
                  ( t nil )
                  
                ); end cond
              ); end progn
            ); end if
        ); end while
      ); end progn
    ); end if

    (vla-scaleentity con    (vlax-3d-point ecent) bscal)
    (vla-scaleentity swleg  (vlax-3d-point ecent) bscal)
    (vla-scaleentity swleg  (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))) (/ 1.0 bscal))
    (if (not (= bscal 1.0)) (vla-resetblock swleg))
   
  ); end "FP" condition
  
  ; start "JBOX" condition
  ( (= (vla-get-effectivename (vlax-ename->vla-object ename)) "JBOX")
  
    (if (vl-catch-all-error-p (vl-catch-all-apply 'getpropertyvalue (list ename "AcDbDynBlockPropertyFlip state1")))
        (progn (alert "\nNew 'JBOX' block only!") (exit)) )

    (setq ang1    (getpropertyvalue ename "AcDbDynBlockPropertyAngle1")
          flip1   (getpropertyvalue ename "AcDbDynBlockPropertyFlip state1")
          conrit  (polar ecent 0.0 3)
          pt5     (polar (polar ecent (/ pi 2) (if (= flip1 1) -5.5 5.5)) 0.0 17.5)
          conswl  (polar (polar pt5 pi 5.25) (* 3 (/ pi 2)) (if (= flip1 1) -3.03109 3.03109)) )
   
    (command "_INSERT" "SWLEG-" pt5 "" "" "")
    (command "_EXPLODE" "L")
    (setq swleg (vlax-ename->vla-object (entlast)))
    (vla-put-layer swleg "CDL-A-LTG-WIRING")
   
    (entmake (list  (cons 0 "LWPOLYLINE")
                    (cons 100 "AcDbEntity")
                    (cons 100 "AcDbPolyline")
                    (cons 8 "CDL-A-LTG-WIRING")
                    (cons 90 2)
                    (cons 70 0)
                    (cons 10 conrit)
                    (cons 42 (if (= flip1 1) -0.3 0.3))
                    (cons 10 conswl)
            ); end list
    ); end entmake
    (setq con (vlax-ename->vla-object (entlast)))

    (vla-rotate swleg (vlax-3d-point ecent) brot)

    (if
      (AND
        (>  (rtos (vla-get-rotation swleg) 2 4) (rtos (/ pi 2) 2 4))
        (<= (rtos (vla-get-rotation swleg) 2 4) (rtos (* 3 (/ pi 2)) 2 4))
      )
      (vla-rotate swleg (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))) pi)
    )

    (vla-rotate con (vlax-3d-point ecent) brot)
   
    (setq side "Swap")
    (while side
      (initget "Flip")
      (setq side (getkword "\nFlip Side or Exit [Flip] <Exit>: "))
        (if side
          (progn
            (setq slopeh (angle (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))) (polar ecent (+ brot (/ pi 2)) (if (= flip1 1) -5.5 5.5))) )
            (vla-move
              swleg
              (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))))
              (vlax-3d-point (polar (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))) slopeh 35))
            )
            (vla-mirror
              con
              (vlax-3d-point ecent)
              (vlax-3d-point (polar ecent (+ brot (/ pi 2)) 1.5))
            )
            (vla-delete con)
            (setq con (vlax-ename->vla-object (entlast)))
          ); end progn
        ); end if
    ); end while

    (vla-scaleentity con    (vlax-3d-point ecent) bscal)
    (vla-scaleentity swleg  (vlax-3d-point ecent) bscal)
    (vla-scaleentity swleg  (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))) (/ 1.0 bscal))
    (if (not (= bscal 1.0)) (vla-resetblock swleg))

  ); end "JBOX" condition
  
  ;start "T / V" condition
  ( (OR (= (vla-get-effectivename (vlax-ename->vla-object ename)) "T") (= (vla-get-effectivename (vlax-ename->vla-object ename)) "V") )
   
    (setq attvis
      (vl-some '(lambda ( x ) 
                  (if (= "VISIBILITY1" (strcase (vla-get-propertyname x))) 
                      (vlax-get x 'value))
                )
                (vlax-invoke (vlax-ename->vla-object ename) 'getdynamicblockproperties)
      )
    )
   
    (setq conrit  (cond
                    ( (OR (= attvis "T###") (= attvis "V###"))
                      (polar ecent 0.0 8.0)
                    )
                    ( (OR (= attvis "T##") (= attvis "V##"))
                      (polar ecent 0.0 6.5)
                    )
                    ( (OR (= attvis "T#") (= attvis "V#"))
                      (polar ecent 0.0 5.0)
                    )
                    ( t nil )
                  ); end cond
          pt5     (polar conrit 0.0 12.5)
          conswl  (polar conrit 0.0 5.5) )

    (command "_INSERT" "SWLEG-" pt5 "" "" "")
    (command "_EXPLODE" "L")
    (setq swleg (vlax-ename->vla-object (entlast)))
    (vla-put-layer swleg "CDL-A-LTG-WIRING")
   
    (entmake (list  (cons 0 "LWPOLYLINE")
                    (cons 100 "AcDbEntity")
                    (cons 100 "AcDbPolyline")
                    (cons 8 "CDL-A-LTG-WIRING")
                    (cons 90 2)
                    (cons 70 0)
                    (cons 10 conrit)
                    (cons 42 -0.225)
                    (cons 10 conswl)
            ); end list
    ); end entmake
    (setq con (vlax-ename->vla-object (entlast)))
   
    (vla-rotate swleg (vlax-3d-point ecent) brot)
   
    (if
      (AND
        (>  (rtos (vla-get-rotation swleg) 2 4) (rtos (/ pi 2) 2 4))
        (<= (rtos (vla-get-rotation swleg) 2 4) (rtos (* 3 (/ pi 2)) 2 4))
      )
      (vla-rotate swleg (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))) pi)
    )
   
    (vla-rotate con (vlax-3d-point ecent) brot)

    (setpropertyvalue (vlax-vla-object->ename swleg) "AcDbDynBlockPropertyPosition1 X" 14.25)
    (setpropertyvalue (vlax-vla-object->ename swleg) "AcDbDynBlockPropertyPosition1 Y" 0.0)
    (setpropertyvalue (vlax-vla-object->ename swleg) "AcDbDynBlockPropertyPosition2 X" 17.0)
    (setpropertyvalue (vlax-vla-object->ename swleg) "AcDbDynBlockPropertyPosition2 Y" 0.0)

    (vla-scaleentity con    (vlax-3d-point ecent) bscal)
    (vla-scaleentity swleg  (vlax-3d-point ecent) bscal)
    (vla-scaleentity swleg  (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))) (/ 1.0 bscal))
    (if (not (= bscal 1.0)) (vla-resetblock swleg))
  
  ); end "T / V" condition
  
  ; start "ZC" condition
  ( (numberp (vl-string-search "ZC" (vla-get-effectivename (vlax-ename->vla-object ename))))
   
    (setq attvis  (vl-some '(lambda ( x ) 
                            (if (= "VISIBILITY1" (strcase (vla-get-propertyname x))) 
                                (vlax-get x 'value))
                            )
                            (vlax-invoke (vlax-ename->vla-object ename) 'getdynamicblockproperties)
                  )
          tbox    (list '(0.0532194 -0.115309 0.0) '(7.49803 4.432 0.0))
          pos1    (cond
                    ( (= attvis "RIGHT")
                      (list
                        (+ (getpropertyvalue ename "AcDbDynBlockPropertyPosition1 X") (car ecent))
                        (+ (getpropertyvalue ename "AcDbDynBlockPropertyPosition1 Y") (cadr ecent))
                        (+ 0.0 (caddr ecent))
                      )
                    )
                    ( (= attvis "UP")
                      (list
                        (+ (getpropertyvalue ename "AcDbDynBlockPropertyPosition2 X") (car ecent))
                        (+ (getpropertyvalue ename "AcDbDynBlockPropertyPosition2 Y") (cadr ecent))
                        (+ 0.0 (caddr ecent))
                      )
                    )
                    ( (= attvis "LEFT")
                      (list
                        (+ (getpropertyvalue ename "AcDbDynBlockPropertyPosition4 X") (car ecent))
                        (+ (getpropertyvalue ename "AcDbDynBlockPropertyPosition4 Y") (cadr ecent))
                        (+ 0.0 (caddr ecent))
                      )
                    )
                    ( (= attvis "DOWN")
                      (list
                        (+ (getpropertyvalue ename "AcDbDynBlockPropertyPosition3 X") (car ecent))
                        (+ (getpropertyvalue ename "AcDbDynBlockPropertyPosition3 Y") (cadr ecent))
                        (+ 0.0 (caddr ecent))
                      )
                    )
                    (t nil)
                  ); end cond
          pt5     (cond
                    ( (OR (= attvis "RIGHT") (= attvis "LEFT"))
                      (polar pos1 (/ pi 2) 8)
                    )
                    ( (OR (= attvis "UP") (= attvis "DOWN"))
                      (polar pos1 0.0 13.5)
                    )
                    (t nil)
                  ); end cond
    ); end setq

    (command "_INSERT" "SWLEG-" pt5 "" "" "")
    (command "_EXPLODE" "L")
    (setq swleg (vlax-ename->vla-object (entlast)))
    (vla-put-layer swleg "CDL-A-LTG-WIRING")
   
    (vla-rotate swleg (vlax-3d-point ecent) brot)
   
    (if
      (AND
        (>  (rtos (vla-get-rotation swleg) 2 4) (rtos (/ pi 2) 2 4))
        (<= (rtos (vla-get-rotation swleg) 2 4) (rtos (* 3 (/ pi 2)) 2 4))
      )
      (vla-rotate swleg (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))) pi)
    )
   
    (setq side "Swap")
    (while side
      (initget "Flip")
      (setq side (getkword "\nFlip Side or Exit [Flip] <Exit>: "))
        (if side
          (progn
            (setq slopeh (angle 
                            (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))
                            (polar ecent (+ brot (angle ecent pos1)) (distance ecent pos1)) )
            )
            (vla-move
              swleg
              (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))))
              (vlax-3d-point (if  (OR (= attvis "RIGHT") (= attvis "LEFT")) 
                                  (polar (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))) slopeh 16)
                                  (polar (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))) slopeh 27)
                             )
              )
            )
          ); end progn
        ); end if
    ); end while
   
  ); end "ZC" condition
  
  ; start "ZM" condition
  ( (= (vla-get-effectivename (vlax-ename->vla-object ename)) "ZM")
   
    (setq ang1    (getpropertyvalue ename "AcDbDynBlockPropertyAngle1")
          look1   (if (vl-catch-all-error-p (vl-catch-all-apply 'getpropertyvalue (list ename "AcDbDynBlockPropertyLookup1")))
                      "Custom"
                      (getpropertyvalue ename "AcDbDynBlockPropertyLookup1") 
                  )
          dist1   (- (getpropertyvalue ename "AcDbDynBlockPropertyDistance1") 12)
    )

    (cond
      ( (OR (= look1 "UP") (= look1 "DOWN"))

        (setq conrit  (polar (polar ecent 0.0 dist1) (/ pi 2) (if (= look1 "DOWN") -0.875 0.875))
              pt5     (polar (polar conrit 0.0 7) (/ pi 2) (if (= look1 "DOWN") -5.625 5.625))
              conswl  (polar (polar pt5 pi 3.5) (* 3 (/ pi 2)) (if (= look1 "DOWN") -3.5 3.5)) )

        (command "_INSERT" "SWLEG-" pt5 "" "" "")
        (command "_EXPLODE" "L")
        (setq swleg (vlax-ename->vla-object (entlast)))
        (vla-put-layer swleg "CDL-A-LTG-WIRING")

        (entmake (list  (cons 0 "LWPOLYLINE")
                        (cons 100 "AcDbEntity")
                        (cons 100 "AcDbPolyline")
                        (cons 8 "CDL-A-LTG-WIRING")
                        (cons 90 2)
                        (cons 70 0)
                        (cons 10 conrit)
                        (cons 42 (if (= look1 "DOWN") -0.38 0.38))
                        (cons 10 conswl)
                 ); end list
        ); end entmake
        (setq con (vlax-ename->vla-object (entlast)))

        (vla-rotate swleg (vlax-3d-point ecent) brot)

        (if
          (AND
            (>  (rtos (vla-get-rotation swleg) 2 4) (rtos (/ pi 2) 2 4))
            (<= (rtos (vla-get-rotation swleg) 2 4) (rtos (* 3 (/ pi 2)) 2 4))
          )
          (vla-rotate swleg (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))) pi)
        )

        (vla-rotate con (vlax-3d-point ecent) brot)
       
      )

      ( (OR (= look1 "RIGHT") (= look1 "LEFT"))

        (setq contop  (polar (polar ecent (/ pi 2) dist1) 0.0 (if (= look1 "LEFT") -0.875 0.875))
              pt5     (polar (polar contop 0.0 (if (= look1 "LEFT") -8 8)) (/ pi 2) 5.5)
              conswl  (polar (polar pt5 pi (if (= look1 "LEFT") -6.53109 6.53109)) (* 3 (/ pi 2)) 1.75) )

        (command "_INSERT" "SWLEG-" pt5 "" "" "")
        (command "_EXPLODE" "L")
        (setq swleg (vlax-ename->vla-object (entlast)))
        (vla-put-layer swleg "CDL-A-LTG-WIRING")

        (entmake (list  (cons 0 "LWPOLYLINE")
                        (cons 100 "AcDbEntity")
                        (cons 100 "AcDbPolyline")
                        (cons 8 "CDL-A-LTG-WIRING")
                        (cons 90 2)
                        (cons 70 0)
                        (cons 10 contop)
                        (cons 42 (if (= look1 "LEFT") 0.31 -0.31))
                        (cons 10 conswl)
                ); end list
        ); end entmake
        (setq con (vlax-ename->vla-object (entlast)))

        (vla-rotate swleg (vlax-3d-point ecent) brot)

        (if
          (AND
            (>  (rtos (vla-get-rotation swleg) 2 4) (rtos (/ pi 2) 2 4))
            (<= (rtos (vla-get-rotation swleg) 2 4) (rtos (* 3 (/ pi 2)) 2 4))
          )
          (vla-rotate swleg (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))) pi)
        )

        (vla-rotate con (vlax-3d-point ecent) brot)

      )

      ( (= look1 "Custom")
       
        (setq conrit  (polar (polar ecent 0.0 dist1) (/ pi 2) 0.875)
              pt5     (polar (polar conrit 0.0 7) (/ pi 2) 5.625)
              conswl  (polar (polar pt5 pi 3.5) (* 3 (/ pi 2)) 3.5) )
       
        (command "_INSERT" "SWLEG-" pt5 "" "" "")
        (command "_EXPLODE" "L")
        (setq swleg (vlax-ename->vla-object (entlast)))
        (vla-put-layer swleg "CDL-A-LTG-WIRING")
       
        (entmake (list  (cons 0 "LWPOLYLINE")
                        (cons 100 "AcDbEntity")
                        (cons 100 "AcDbPolyline")
                        (cons 8 "CDL-A-LTG-WIRING")
                        (cons 90 2)
                        (cons 70 0)
                        (cons 10 conrit)
                        (cons 42 0.38)
                        (cons 10 conswl)
                ); end list
        ); end entmake
        (setq con (vlax-ename->vla-object (entlast)))

        (vla-rotate swleg (vlax-3d-point ecent) (+ brot ang1))

        (if
          (AND
            (>  (rtos (vla-get-rotation swleg) 2 4) (rtos (/ pi 2) 2 4))
            (<= (rtos (vla-get-rotation swleg) 2 4) (rtos (* 3 (/ pi 2)) 2 4))
          )
          (vla-rotate swleg (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))) pi)
        )

        (vla-rotate con (vlax-3d-point ecent) (+ brot ang1))

      )

      (t nil)

    ); end cond

    (setq side "Swap")
   
    (while side
      (initget "Flip")
      (setq side (getkword "\nFlip Side or Exit [Flip] <Exit>: "))
        (if side
          (progn
            (setq slopeh 
                   (angle 
                     (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))
                     (polar ecent (+ brot (+ ang1 (/ pi 2)))
                                  (if (OR (= look1 "RIGHT") (= look1 "LEFT"))
                                    8.875
                                    6.5
                                  )
                     )
                   )
            )
            (vla-move
              swleg
              (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))))
              (vlax-3d-point 
                (polar  (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))
                        slopeh
                        (if (OR (= look1 "RIGHT") (= look1 "LEFT"))
                            (+ 11 (* dist1 2))
                            (+ 14 (* dist1 2))
                        )
                )
              )
            )
            (vla-mirror
              con
              (vlax-3d-point ecent)
              (vlax-3d-point (polar ecent (+ (+ brot ang1) (/ pi 2)) 1.75))
            )
            (vla-delete con)
            (setq con (vlax-ename->vla-object (entlast)))
          ); end progn
        ); end if
    ); end while
   
    (vla-scaleentity con    (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename con))))) bscal)
    (vla-scaleentity swleg  (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename con))))) bscal)
    (vla-scaleentity swleg  (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))) (/ 1.0 bscal))
    (if (not (= bscal 1.0)) (vla-resetblock swleg))
   
  ); end "ZM" condition
  
  ; start "ZP" condition
  ( (= (vla-get-effectivename (vlax-ename->vla-object ename)) "ZP")
   
    (setq dist1   (- (getpropertyvalue ename "AcDbDynBlockPropertyDistance1") (/ (getpropertyvalue ename "AcDbDynBlockPropertyDistance1") 8))   ; height
          dist2   (- (getpropertyvalue ename "AcDbDynBlockPropertyDistance2") (getpropertyvalue ename "AcDbDynBlockPropertyDistance1"))         ; width
          ang1    (getpropertyvalue ename "AcDbDynBlockPropertyAngle1")
          ang2    (angle ecent (polar ecent (+ brot (+ ang1 (/ pi 2))) dist1))
          conrit  (cond
                    ( (OR
                        (if
                          (AND
                            (>  (rtos ang2 2 4) (rtos (/ pi 4) 2 4))
                            (<= (rtos ang2 2 4) (rtos (* 3 (/ pi 4)) 2 4))
                          )
                          (setq attvis "UP")
                        ); end if
                        (if
                          (AND
                            (>  (rtos ang2 2 4) (rtos (* 5 (/ pi 4)) 2 4))
                            (<= (rtos ang2 2 4) (rtos (* 7 (/ pi 4)) 2 4))
                          )
                          (setq attvis "DOWN")
                        ); end if
                      ); end condition logic for up/down
                      (polar (polar ecent (/ pi 2) (if (= attvis "DOWN") (- dist1) dist1)) 0.0 dist2)
                    ); end up/down condition
                    ( (OR
                        (if
                          (OR
                            (>  (rtos ang2 2 4) (rtos (* 7 (/ pi 4)) 2 4))
                            (<= (rtos ang2 2 4) (rtos (/ pi 4) 2 4))
                          )
                          (setq attvis "RIGHT")
                        ); end if
                        (if
                          (AND
                            (>  (rtos ang2 2 4) (rtos (* 3 (/ pi 4)) 2 4))
                            (<= (rtos ang2 2 4) (rtos (* 5 (/ pi 4)) 2 4))
                          )
                          (setq attvis "LEFT")
                        )
                      ); end condition logic for right/left
                      (polar (polar ecent 0.0 (if (= attvis "LEFT") (- dist1) dist1)) (/ pi 2) dist2)
                     
                    ); end right/left condition
                    
                    (t nil)
                    
                  ); end cond
          pt5     (cond
                    ( (OR (= attvis "UP") (= attvis "DOWN"))
                      (polar (polar conrit 0.0 8.0) (/ pi 2) (if (= attvis "DOWN") -4.75 4.75))
                    )
                    ( (OR (= attvis "RIGHT") (= attvis "LEFT"))
                      (polar (polar conrit (/ pi 2) 5.5) 0.0 (if (= attvis "LEFT") -8.0 8.0))
                    )
                    (t nil)
                  ); end cond
          conswl  (cond
                    ( (OR (= attvis "UP") (= attvis "DOWN"))
                      (polar (polar pt5 pi 4.83939) (* 3 (/ pi 2)) (if (= attvis "DOWN") -3.23358 3.23358))
                    )
                    ( (OR (= attvis "RIGHT") (= attvis "LEFT"))
                      (polar (polar pt5 pi (if (= attvis "LEFT") -6.53109 6.53109)) (* 3 (/ pi 2)) 1.75)
                    )
                    (t nil)
                  ); end cond

    ); end setq
   
    (command "_INSERT" "SWLEG-" pt5 "" "" "")
    (command "_EXPLODE" "L")
    (setq swleg (vlax-ename->vla-object (entlast)))
    (vla-put-layer swleg "CDL-A-LTG-WIRING")

    (entmake  (list (cons 0 "LWPOLYLINE")
                    (cons 100 "AcDbEntity")
                    (cons 100 "AcDbPolyline")
                    (cons 8 "CDL-A-LTG-WIRING")
                    (cons 90 2)
                    (cons 70 0)
                    (cons 10 conrit)
                    (cons 42 (cond
                              ( (OR (= attvis "UP") (= attvis "LEFT"))
                                (if (= attvis "LEFT") 0.31 0.33)
                              )
                              ( (OR (= attvis "DOWN") (= attvis "RIGHT"))
                                (if (= attvis "RIGHT") -0.31 -0.33)
                              )
                              (t nil)
                            ); end cond
                    ); end cons
                    (cons 10 conswl)
              ); end list
    ); end entmake
    (setq con (vlax-ename->vla-object (entlast)))

    (vla-rotate swleg (vlax-3d-point ecent) (cond
                                              ( (= attvis "UP")
                                                (- ang2 (/ pi 2))
                                              )
                                              ( (= attvis "LEFT")
                                                (- ang2 pi)
                                              )
                                              ( (= attvis "DOWN")
                                                (- ang2 (* 3 (/ pi 2)))
                                              )
                                              ( (= attvis "RIGHT")
                                                (- ang2 (* 2 pi))
                                              )
                                              (t nil)
                                            ); end cond
    ); end vla-rotate

    (if
      (AND
        (>  (rtos (vla-get-rotation swleg) 2 4) (rtos (/ pi 2) 2 4))
        (<= (rtos (vla-get-rotation swleg) 2 4) (rtos (* 3 (/ pi 2)) 2 4))
      )
      (vla-rotate swleg (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))) pi)
    )

    (vla-rotate con (vlax-3d-point ecent) (cond
                                              ( (= attvis "UP")
                                                (- ang2 (/ pi 2))
                                              )
                                              ( (= attvis "LEFT")
                                                (- ang2 pi)
                                              )
                                              ( (= attvis "DOWN")
                                                (- ang2 (* 3 (/ pi 2)))
                                              )
                                              ( (= attvis "RIGHT")
                                                (- ang2 (* 2 pi))
                                              )
                                              (t nil)
                                            ); end cond
    ); end vla-rotate

    (setq side "Swap")
   
    (while side
      (initget "Flip")
      (setq side (getkword "\nFlip Side or Exit [Flip] <Exit>: "))
        (if side
          (progn
            (setq slopeh
                   (angle
                     (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))
                     (polar ecent ang2
                                  (if (OR (= attvis "RIGHT") (= attvis "LEFT"))
                                      (+ dist1 8.0)
                                      (+ dist1 4.75)
                                  )
                     )
                   )
            )
            (vla-move
              swleg
              (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))))
              (vlax-3d-point
                (polar  (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))
                        slopeh
                        (if (OR (= attvis "RIGHT") (= attvis "LEFT"))
                            (+ 11 (* dist2 2))
                            (+ 16 (* dist2 2))
                        )
                )
              )
            )
            (vla-mirror
              con
              (vlax-3d-point ecent)
              (vlax-3d-point (polar ecent ang2 3))
            )
            (vla-delete con)
            (setq con (vlax-ename->vla-object (entlast)))
          ); end progn
        ); end if
    ); end while
   
    (vla-scaleentity con    (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename con))))) bscal)
    (vla-scaleentity swleg  (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename con))))) bscal)
    (vla-scaleentity swleg  (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))) (/ 1.0 bscal))
    (if (not (= bscal 1.0)) (vla-resetblock swleg))
   
  ); end "ZP" condition
  
  ; start "ZW" condition
  ( (= (vla-get-effectivename (vlax-ename->vla-object ename)) "ZW")
  
    (setq ang1    (getpropertyvalue ename "AcDbDynBlockPropertyAngle1")
          look1   (if (vl-catch-all-error-p (vl-catch-all-apply 'getpropertyvalue (list ename "AcDbDynBlockPropertyLookup1")))
                      "Custom"
                      (getpropertyvalue ename "AcDbDynBlockPropertyLookup1") 
                  ) 
    )

    (cond
      ( (OR (= look1 "UP") (= look1 "DOWN"))

        (setq conbot  (polar ecent (/ pi 2) 2.5)
              pt5     (polar (polar ecent (/ pi 2) 5.5) 0 13.75)
              conswl  (polar (polar pt5 pi 5.63067) (* 3 (/ pi 2)) 2.77674) )

        (command "_INSERT" "SWLEG-" pt5 "" "" "")
        (command "_EXPLODE" "L")
        (setq swleg (vlax-ename->vla-object (entlast)))
        (vla-put-layer swleg "CDL-A-LTG-WIRING")

        (entmake (list  (cons 0 "LWPOLYLINE")
                        (cons 100 "AcDbEntity")
                        (cons 100 "AcDbPolyline")
                        (cons 8 "CDL-A-LTG-WIRING")
                        (cons 90 2)
                        (cons 70 0)
                        (cons 10 conbot)
                        (cons 42 0.245)
                        (cons 10 conswl)
                 ); end list
        ); end entmake
        (setq con (vlax-ename->vla-object (entlast)))

        (vla-rotate swleg (vlax-3d-point ecent) (+ brot ang1))

        (if
          (AND
            (>  (rtos (vla-get-rotation swleg) 2 4) (rtos (/ pi 2) 2 4))
            (<= (rtos (vla-get-rotation swleg) 2 4) (rtos (* 3 (/ pi 2)) 2 4))
          )
          (vla-rotate swleg (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))) pi)
        )

        (vla-rotate con (vlax-3d-point ecent) (+ brot ang1))

        (if (= look1 "DOWN")
            (progn
              (setq slopeh (angle (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))) (polar ecent (- brot (/ pi 2)) 5.5)) )
              (vla-move
                swleg
                (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))))
                (vlax-3d-point (polar (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))) slopeh 27.5))
              )
              (vla-mirror
                con
                (vlax-3d-point ecent)
                (vlax-3d-point (polar ecent (- brot (/ pi 2)) 5.5))
              )
              (vla-delete con)
              (setq con (vlax-ename->vla-object (entlast)))
            ); end progn
        ); end if
       
      )

      ( (OR (= look1 "RIGHT") (= look1 "LEFT"))

        (setq conbot  (polar ecent 0.0 2.5)
              pt5     (polar (polar ecent 0.0 8.5) (/ pi 2) 9.75)
              conswl  (polar (polar pt5 pi 5.63067) (* 3 (/ pi 2)) 2.77674) )

        (command "_INSERT" "SWLEG-" pt5 "" "" "")
        (command "_EXPLODE" "L")
        (setq swleg (vlax-ename->vla-object (entlast)))
        (vla-put-layer swleg "CDL-A-LTG-WIRING")

        (entmake (list  (cons 0 "LWPOLYLINE")
                        (cons 100 "AcDbEntity")
                        (cons 100 "AcDbPolyline")
                        (cons 8 "CDL-A-LTG-WIRING")
                        (cons 90 2)
                        (cons 70 0)
                        (cons 10 conbot)
                        (cons 42 -0.245)
                        (cons 10 conswl)
                ); end list
        ); end entmake
        (setq con (vlax-ename->vla-object (entlast)))

        (vla-rotate swleg 
          (vlax-3d-point ecent)
          (if (= look1 "LEFT")
              (+ brot pi)
              brot
          ) )

        (if
          (AND
            (>  (rtos (vla-get-rotation swleg) 2 4) (rtos (/ pi 2) 2 4))
            (<= (rtos (vla-get-rotation swleg) 2 4) (rtos (* 3 (/ pi 2)) 2 4))
          )
          (vla-rotate swleg (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))) pi)
        )

        (vla-rotate con
          (vlax-3d-point ecent)
          (if (= look1 "LEFT")
              (+ brot pi)
              brot 
          ) )
       
        (if (= look1 "LEFT")
            (progn
              (setq slopeh (angle (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))) (polar ecent (- (- brot (/ pi 2)) (/ pi 2)) 8.5)) )
              (vla-move
                swleg
                (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))))
                (vlax-3d-point (polar (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))) slopeh 19.5))
              )
              (vla-mirror
                con
                (vlax-3d-point ecent)
                (vlax-3d-point (polar ecent (- (- brot (/ pi 2)) (/ pi 2)) 8.5))
              )
              (vla-delete con)
              (setq con (vlax-ename->vla-object (entlast)))
            ); end progn
        ); end if

      )

      ( (= look1 "Custom")
       
        (setq conbot  (polar ecent (/ pi 2) 2.5)
              pt5     (polar (polar ecent (/ pi 2) 5.5) 0 13.75)
              conswl  (polar (polar pt5 pi 5.63067) (* 3 (/ pi 2)) 2.77674) )
       
        (command "_INSERT" "SWLEG-" pt5 "" "" "")
        (command "_EXPLODE" "L")
        (setq swleg (vlax-ename->vla-object (entlast)))
        (vla-put-layer swleg "CDL-A-LTG-WIRING")
       
        (entmake (list  (cons 0 "LWPOLYLINE")
                        (cons 100 "AcDbEntity")
                        (cons 100 "AcDbPolyline")
                        (cons 8 "CDL-A-LTG-WIRING")
                        (cons 90 2)
                        (cons 70 0)
                        (cons 10 conbot)
                        (cons 42 0.245)
                        (cons 10 conswl)
                ); end list
        ); end entmake
        (setq con (vlax-ename->vla-object (entlast)))
       
        (vla-rotate swleg (vlax-3d-point ecent) (+ brot ang1))

        (if
          (AND
            (>  (rtos (vla-get-rotation swleg) 2 4) (rtos (/ pi 2) 2 4))
            (<= (rtos (vla-get-rotation swleg) 2 4) (rtos (* 3 (/ pi 2)) 2 4))
          )
          (vla-rotate swleg (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))) pi)
        )
       
        (vla-rotate con (vlax-3d-point ecent) (+ brot ang1))
       
      )

      (t nil)

    ); end cond

    (setq side "Swap")
   
    (while side
      (initget "Flip")
      (setq side (getkword "\nFlip Side or Exit [Flip] <Exit>: "))
        (if side
          (progn
            (setq slopeh 
                   (angle 
                     (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))) 
                     (polar ecent (+ brot (- (- ang1 (/ pi 2)) pi)) 
                                  (if (OR (= look1 "RIGHT") (= look1 "LEFT"))
                                    8.5
                                    5.5
                                  )
                     )
                   )
            )
            (vla-move
              swleg
              (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))))
              (vlax-3d-point 
                (polar (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))) 
                slopeh
                (if (OR (= look1 "RIGHT") (= look1 "LEFT"))
                      19.5
                      27.5
                )
                )
              )
            )
            (vla-mirror
              con
              (vlax-3d-point ecent)
              (vlax-3d-point (polar ecent (+ (+ brot ang1) (/ pi 2)) 5.5))
            )
            (vla-delete con)
            (setq con (vlax-ename->vla-object (entlast)))
          ); end progn
        ); end if
    ); end while
   
    (vla-scaleentity con    (vlax-3d-point ecent) bscal)
    (vla-scaleentity swleg  (vlax-3d-point ecent) bscal)
    (vla-scaleentity swleg  (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))) (/ 1.0 bscal))
    (if (not (= bscal 1.0)) (vla-resetblock swleg))
   
  ); end "ZW" condition
  
  ; start "ALL OTHER" conditions
  ( T

    (setq attvis
      (vl-some '(lambda ( x ) 
                  (if (= "VISIBILITY1" (strcase (vla-get-propertyname x))) 
                      (vlax-get x 'value))
                )
                (vlax-invoke (vlax-ename->vla-object ename) 'getdynamicblockproperties)
      )
    )

    (setq bname (tblobjname "BLOCK" (vla-get-effectivename (vlax-ename->vla-object ename))))

    (while
      (setq bname (entnext bname))
      (setq elist (cons bname elist))
    )

    (foreach e elist
      (if (= (cdr (assoc 8 (entget e))) "CDL-A-LTG-ATTRIBUTE")
          (progn
            (setq plist
              (cons
                (list
                  (+ (car (cdr (assoc 10 (entget e))))(car ecent)) 
                  (+ (cadr (cdr (assoc 10 (entget e))))(cadr ecent))
                  (+ (caddr (cdr (assoc 10 (entget e))))(caddr ecent))
                )
                plist
              );end cons
            );end setq
            (setq tbox (textbox (entget e)))
          ); end progn
      ); end if
    ); end foreach

    ; remove any entity that is on housing layer or does not have area property from elist
    (foreach e elist
        (if (OR
              (= (cdr (assoc 8 (entget e))) "CDL-A-LTG-HOUSING")
              (vl-catch-all-error-p (vl-catch-all-apply 'getpropertyvalue (list e "AREA")))
            )
            (setq elist (vl-remove e elist))
        )
    )

    ; set trim as the entity in elist with the largest area
    (setq trim (car elist))
    (foreach e elist
      (if (> (vla-get-area (vlax-ename->vla-object e)) (vla-get-area (vlax-ename->vla-object trim)))
          (setq trim e)
      )
    )
    
    ;;; USE 'MAX' TO FIND LARGEST AREA?
    ;    (apply 'max
    ;      (foreach e elist
    ;        (setq testlist (append (list (vla-get-area (vlax-ename->vla-object e))) testlist))
    ;      )
    ;    )
    ;;;

    (vla-getboundingbox (vlax-ename->vla-object trim) 'ptmin 'ptmax)
    (setq ptmin   (vlax-safearray->list ptmin)
          ptmax   (vlax-safearray->list ptmax)
          conrit  (polar ptmax (angle ptmax (list (car ptmax) (cadr ptmin))) (/ (distance ptmax (list (car ptmax) (cadr ptmin))) 2))
          contop  (polar ptmax (angle ptmax (list (car ptmin) (cadr ptmax))) (/ (distance ptmax (list (car ptmin) (cadr ptmax))) 2))
          conlef  (polar ptmin (angle ptmin (list (car ptmin) (cadr ptmax))) (/ (distance ptmin (list (car ptmin) (cadr ptmax))) 2))
          conbot  (polar ptmin (angle ptmin (list (car ptmax) (cadr ptmin))) (/ (distance ptmin (list (car ptmax) (cadr ptmin))) 2))
    )
      
    (setq conrit  (list (+ (car conrit) (car ecent)) (+ (cadr conrit) (cadr ecent)) (+ (caddr conrit) (caddr ecent)))
          contop  (list (+ (car contop) (car ecent)) (+ (cadr contop) (cadr ecent)) (+ (caddr contop) (caddr ecent)))
          conlef  (list (+ (car conlef) (car ecent)) (+ (cadr conlef) (cadr ecent)) (+ (caddr conlef) (caddr ecent)))
          conbot  (list (+ (car conbot) (car ecent)) (+ (cadr conbot) (cadr ecent)) (+ (caddr conbot) (caddr ecent)))
    )

    (setq pt1 (car plist))
    (foreach p plist
      (cond
        ((= attvis "RIGHT")
            (if (> (car p) (car pt1))
                (setq pt1 p)
            )
        )
        ((= attvis "UP")
            (if (> (cadr p) (cadr pt1))
                (setq pt1 p)
            )
        )
        ((= attvis "LEFT")
            (if (< (car p) (car pt1))
                (setq pt1 p)
            )
        )
        ((= attvis "DOWN")
            (if (< (cadr p) (cadr pt1))
                (setq pt1 p)
            )
        )
        (t nil) 
      ); end cond
    ); end foreach
    (setq pt2
        (list
          (+ (car pt1)(car (cadr tbox)))
          (+ (cadr pt1)(cadr (cadr tbox)))
          (+ (caddr pt1)(caddr (cadr tbox)))
        )
    )
    (setq pt3 (list (car pt1)(cadr pt2)))
    (setq pt4 (list (car pt2)(cadr pt1)))

    (cond
      ( (= attvis "RIGHT")
        (setq pt5     (polar (polar ecent (setq slopev (angle ecent contop)) (setq dist1 (+ (distance ecent contop) 5.75))) (angle ecent conrit) 9.75))
        (setq conswl  (polar (polar pt5 (setq slopeh (angle conrit ecent)) 6.88074) (angle contop ecent) 0.905867))
      )
      ( (= attvis "UP")
        (setq attcent (polar (polar pt1 (angle pt1 pt3) (/ (distance pt1 pt3) 2)) (angle pt1 pt4) (/ (distance pt1 pt4) 2)))
        (setq pt5     (polar (polar pt2 (setq slopev (angle pt2 pt4)) (/ (distance pt2 pt4) 2)) (setq slopeh (angle pt1 pt4)) (setq dist1 9.5)))
        (setq conswl  (polar (polar pt5 (angle pt2 pt3) 4.83939) (angle pt3 pt1) 3.23358))
        (setq dist1   (distance pt5 attcent))
      )
      ( (= attvis "LEFT")
        (setq pt5     (polar (polar ecent (setq slopev (angle ecent contop)) (setq dist1 (+ (distance ecent contop) 5.75))) (angle ecent conlef) 9.75))
        (setq conswl  (polar (polar pt5 (setq slopeh (angle conlef ecent)) 6.88074) (angle contop ecent) 0.905867))
      )
      ( (= attvis "DOWN")
        (setq attcent (polar (polar pt1 (angle pt1 pt3) (/ (distance pt1 pt3) 2)) (angle pt1 pt4) (/ (distance pt1 pt4) 2)))
        (setq pt5     (polar (polar pt4 (setq slopev (angle pt4 pt2)) (/ (distance pt4 pt2) 2)) (setq slopeh (angle pt1 pt4)) (setq dist1 9.5)))
        (setq conswl  (polar (polar pt5 (angle pt2 pt3) 4.83939) (angle pt1 pt3) 3.23358))
        (setq dist1   (distance pt5 attcent))
      )
      (t nil)
    ); end cond

    (command "_INSERT" "SWLEG-" pt5 "" "" "")
    (command "_EXPLODE" "L")
    (setq swleg (vlax-ename->vla-object (entlast)))
    (vla-put-layer swleg "CDL-A-LTG-WIRING")
    
    (vla-rotate swleg (vlax-3d-point ecent) brot)

    (entmake (list  (cons 0 "LWPOLYLINE")
                    (cons 100 "AcDbEntity")
                    (cons 100 "AcDbPolyline")
                    (cons 8 "CDL-A-LTG-WIRING")
                    (cons 90 2)
                    (cons 70 0)
                    (cons 10 
                          (cond
                            ( (OR (= attvis "RIGHT") (= attvis "LEFT"))
                              contop
                            )
                            ( (OR (= attvis "UP") (= attvis "DOWN"))
                              conrit
                            )
                            (t nil) 
                          ) )
                    (cons 42 
                          (cond
                            ( (OR (= attvis "RIGHT") (= attvis "DOWN"))
                              (setq bulge -0.365)
                            )
                            ( (OR (= attvis "UP") (= attvis "LEFT"))
                              (setq bulge 0.365)
                            )
                            (t nil) 
                          ) )
                    (cons 10 conswl)
              ); end list
    ); end entmake
    (setq con (vlax-ename->vla-object (entlast)))
    
    (vla-rotate con (vlax-3d-point ecent) brot)

    (setq side "Swap")
    (while side
      (initget "Flip")
      (setq side (getkword "\nFlip Side or Exit [Flip] <Exit>: "))
        (if side
          (progn
            (cond
              ( (OR (= attvis "RIGHT") (= attvis "LEFT"))
               
                (if (minusp slopev)
                    (setq slopev (+ slopev pi))
                    (setq slopev (- slopev pi))
                )
                (vla-move
                  swleg
                  (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))))
                  (vlax-3d-point (setq pt5 (polar (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))) (+ brot slopev) (* dist1 2))))
                )
                (if (minusp bulge)
                    (setq bulge (abs bulge))
                    (setq bulge (- bulge))
                )
                (entmake (list  (cons 0 "LWPOLYLINE")
                                (cons 100 "AcDbEntity")
                                (cons 100 "AcDbPolyline")
                                (cons 8 "CDL-A-LTG-WIRING")
                                (cons 90 2)
                                (cons 70 0)
                                (cons 10 (polar ecent (+ brot slopev) (/ (distance contop conbot) 2)))
                                (cons 42 bulge)
                                (cons 10 (polar (polar pt5 (+ brot slopeh) 6.88074) (- (+ brot slopev) pi) 0.905867))
                          ); end list
                )
                (vla-delete con)
                (setq con (vlax-ename->vla-object (entlast)))
               
              ); end RIGHT or LEFT condition

              ( (OR (= attvis "UP") (= attvis "DOWN"))

                (if (minusp slopeh)
                    (setq slopeh (+ slopeh pi))
                    (setq slopeh (- slopeh pi))
                )
                (vla-move
                  swleg
                  (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))))
                  (vlax-3d-point (setq pt5 (polar (cdr (assoc 10 (entget (vlax-vla-object->ename swleg)))) (+ brot slopeh) (* dist1 2))))
                )
                (if (minusp bulge)
                    (setq bulge (abs bulge))
                    (setq bulge (- bulge))
                )
                (entmake (list  (cons 0 "LWPOLYLINE")
                                (cons 100 "AcDbEntity")
                                (cons 100 "AcDbPolyline")
                                (cons 8 "CDL-A-LTG-WIRING")
                                (cons 90 2)
                                (cons 70 0)
                                (cons 10 (polar ecent (+ brot slopeh) (/ (distance conrit conlef) 2)))
                                (cons 42 bulge)
                                (cons 10 (polar (polar pt5 (+ brot (- slopeh pi)) 4.83939) (+ brot slopev) 3.23358))
                          ); end list
                )
                (vla-delete con)
                (setq con (vlax-ename->vla-object (entlast)))
               
              ); end UP or DOWN condition
              
              (t nil) 
              
            ); end cond
          ); end progn
        ); end if
    ); end while
    
    (vla-scaleentity con    (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename con))))) bscal)
    (vla-scaleentity swleg  (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename con))))) bscal)
    (vla-scaleentity swleg  (vlax-3d-point (cdr (assoc 10 (entget (vlax-vla-object->ename swleg))))) (/ 1.0 bscal))
    (if (not (= bscal 1.0)) (vla-resetblock swleg))
    
  ); end "ALL OTHER" conditions

); end cond

; restore system variables
(redraw ename 4)
(setvar "OSMODE" oldsnap)
(setvar "CLAYER" oldlayer)
(setvar "CMDECHO" oldecho)

;end undo mark
(vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))

(princ)

); end defun