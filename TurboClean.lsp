(defun c:clean ( / *error* oldpick sset bent n bc blist)

  ; error function
  (defun *error* ( msg )

    (vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))
    (command-s "undo" "")
    (if oldpick  (setvar "PICKSTYLE" oldpick))

    (if (not (member msg '("Function cancelled" "quit / exit abort")))
        (princ (strcat "\nError: " msg))
    )
    (princ)
  ); end defun *error*
  
  ; load visual lisp extensions
  (vl-load-com)

  ; start undo mark
  (vla-startundomark (vla-get-activedocument (vlax-get-acad-object)))

  ; save and update system variables
  (setq oldpick   (getvar "PICKSTYLE"))
  (setvar "PICKSTYLE" 0)

  ; set layer 0
  (command "-layer" "make" 0 "")
  
  ; purge / thaw all layers / set text style / set all bylayer
  (command  "-purge" "all" "*" "no"
            "-layer" "thaw" "*" ""
            "-style" "Standard" "SIMPLEX" "" "" "" "" "" ""
            "setbylayer" "all" "" "" ""
  ); end command
  
  ; update properties for all layers
  (vlax-for layer (vla-get-layers (vla-get-activedocument (vlax-get-acad-object)))
    (vlax-put-property layer "LayerOn" -1)
    (vlax-put-property layer "Lock" 0)
    (vlax-put-property layer "Color" 7)
    (vlax-put-property layer "Linetype" "Continuous")
    (vlax-put-property layer "Lineweight" -3)
  ); end vlax-for

  ; delete layouts
  (vlax-for layout (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object)))
    (if	(/= (vla-get-name layout) "Model")
        (vla-delete layout)))
  ; delete xrefs
  (vlax-for xref (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object)))
    (if	(= :vlax-true (vla-get-isxref xref))
        (vla-detach xref)))
  ; delete images
  (if (dictsearch (namedobjdict) "ACAD_IMAGE_DICT")
    (vlax-map-collection (vla-item (vla-get-dictionaries (vla-get-activedocument (vlax-get-acad-object))) "ACAD_IMAGE_DICT") 'vla-delete))
  ; delete pdf underlays
  (if (dictsearch (namedobjdict) "ACAD_PDFDEFINITIONS")
    (vlax-map-collection (vla-item (vla-get-dictionaries (vla-get-activedocument (vlax-get-acad-object))) "ACAD_PDFDEFINITIONS") 'vla-delete))
  ; delete layer states
  (if (= (vla-get-hasextensiondictionary (vla-get-layers (vla-get-activedocument (vlax-get-acad-object)))) :vlax-true)
    (vlax-map-collection (vla-item (vla-getextensiondictionary (vla-get-layers (vla-get-activedocument (vlax-get-acad-object)))) "ACAD_LAYERSTATES") 'vla-delete))
  
  ; check for Candelaria blocks and replace
    ; APPLIANCES
    ; BEDS
    ; ELEVATORS - SAVARIA-ECLIPSE
    ; MECHANICAL
    ; PLUMBING FIXTURES
    ; SITE UTILITIES
  
  ; set dimension colors to ByLayer
  (if (ssget "_X" '((0 . "DIMENSION")))
    (progn
      (setq sset (ssget "_X" '((0 . "DIMENSION"))))
      (setq n 0)
      (repeat (sslength sset)
        (vlax-put-property (vlax-ename->vla-object (ssname sset n)) "DimensionLineColor" acByLayer)
        (vlax-put-property (vlax-ename->vla-object (ssname sset n)) "TextColor" acByLayer)
        (if (vlax-property-available-p (vlax-ename->vla-object (ssname sset n)) "ExtensionLineColor")
            (vlax-put-property (vlax-ename->vla-object (ssname sset n)) "ExtensionLineColor" acByLayer) )
        (setq n (1+ n))
      ); end repeat
    ); end progn
  ); end if
  
  ; set leader colors to ByLayer
  (if (ssget "_X" '((0 . "LEADER")))
    (progn
      (setq sset (ssget "_X" '((0 . "LEADER"))))
      (setq n 0)
      (repeat (sslength sset)
        (vlax-put-property (vlax-ename->vla-object (ssname sset n)) "DimensionLineColor" acByLayer)
        (setq n (1+ n))
      ); end repeat
    ); end progn
  ); end if
  
  ; set polyline width to 0 / set linetype scale to 1.0 / erase all points
  (if (ssget "_X" '((0 . "LWPOLYLINE")))
      (command "pedit" "multiple" (ssget "_X" '((0 . "LWPOLYLINE"))) "" "width" "0" ""))
  (if (ssget "_X" '((0 . "ARC,CIRCLE,ELLIPSE,LINE,LWPOLYLINE,SPLINE")))
      (command "chprop" (ssget "_X" '((0 . "ARC,CIRCLE,ELLIPSE,LINE,LWPOLYLINE,SPLINE"))) "" "ltscale" "1" ""))
  (if (ssget "_X" '((0 . "POINT")))
      (command "erase" (ssget "_X" '((0 . "POINT"))) ""))
  (if (ssget "_X" '((0 . "WIPEOUT")))
      (command "erase" (ssget "_X" '((0 . "WIPEOUT"))) ""))
  
  ;"chprop" (ssget "_X" '((0 . "HATCH"))) "" "color" "9" ""

  (if (ssget "_X" '((0 . "INSERT")))
      (progn
        (setq sset (ssget "_X" '((0 . "INSERT"))) 
              n    0
        )
        (repeat (sslength sset)
                (setq blist 
                    (if (member (vla-get-effectivename (vlax-ename->vla-object (ssname sset n))) blist)
                        blist
                        (cons (vla-get-effectivename (vlax-ename->vla-object (ssname sset n))) blist)
                    )
                )
                (setq n (1+ n)) 
              ); end repeat
        
        (initget "Yes No")
        (setq bc (getkword (strcat "\nClean (" (rtos (length blist) 2 0) ") Blocks? [~" (rtos (* (length blist) 0.35) 2 0) " seconds] [Yes/No] <No>: ")))
        (if (not bc)
            (setq bc "No"))
        
        (if (= bc "Yes")
            (progn

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DOES NOT WORK FOR CANDELARIA BLOCK 'TABLES' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

              (setq blist (vl-remove "TABLES" blist))

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DOES NOT WORK FOR CANDELARIA BLOCK 'TABLES' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              
              (foreach b blist
                (setq bent (tblobjname "BLOCK" b))
                (while  (setq bent (entnext bent))
                        (cond
                          ; delete wipeouts, points, text
                          ( (or 
                              (= (cdr (assoc 0 (entget bent))) "WIPEOUT") 
                              (= (cdr (assoc 0 (entget bent))) "POINT")
;                              (= (cdr (assoc 0 (entget bent))) "MTEXT")
;                              (= (cdr (assoc 0 (entget bent))) "TEXT")
                            ); end or
                            (vla-erase (vlax-ename->vla-object bent))
                          )
                          ; update line properties for subentities
                          ( (or (= (cdr (assoc 0 (entget bent))) "ARC")
                                (= (cdr (assoc 0 (entget bent))) "CIRCLE")
                                (= (cdr (assoc 0 (entget bent))) "ELLIPSE")
                                (= (cdr (assoc 0 (entget bent))) "LINE")
                                (= (cdr (assoc 0 (entget bent))) "LWPOLYLINE")
                                (= (cdr (assoc 0 (entget bent))) "SPLINE")
                            ); end or
                            ; delete lines with phantom2 linetype or defpoints layer
                            (if (or (= (vla-get-linetype (vlax-ename->vla-object bent)) "PHANTOM2")
                                    (= (cdr (assoc 8 (entget bent))) "Defpoints")
                                    (= (cdr (assoc 8 (entget bent))) "defpoints")
                                ); end or
                                (vla-erase (vlax-ename->vla-object bent))
                                (progn
                                  ; set lwpolyline constant width to 0
                                  (if (and (= (cdr (assoc 0 (entget bent))) "LWPOLYLINE") (/= (cdr (assoc 43 (entget bent))) 0.0))
                                      (vla-put-constantwidth (vlax-ename->vla-object bent) 0.0)
                                  )
                                  (setpropertyvalue bent "Color" acByLayer)
                                  (vla-put-linetype (vlax-ename->vla-object bent) "ByLayer")
                                  (vla-put-linetypescale (vlax-ename->vla-object bent) 1.0)
                                  (vla-put-lineweight (vlax-ename->vla-object bent) -3)
                                ); end progn
                            ); end if
                          )
                          ( t nil )
                        ); end cond
                  ); end while
              ); end foreach
              (foreach b blist (command "bedit" b "bsave" "bclose"))
            ); end progn
        ); end if
      ); end progn
  ); end if

  ; overkill / purge all / purge regapps / audit
  (command  "-overkill" "all" "" "I" "N" "O" "0.000001" "P" "D" "N" "B" "Y" "Y" "T" "Y" "E" "N" "A" "Y" "D"
            "-purge" "all" "*" "no"
            "-purge" "regapps" "*" "no"
            "audit" "yes"
  ); end command 

  ; set point mode 3
  (setvar "PDMODE" 3)
  (setvar "PICKSTYLE" oldpick)
  (command "zoom" "extents")

  ; end undo mark
  (vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))

  (princ)

); end defun

;;  === Command String ===
;;  ^C^C-LAYER;ON;*;;_.-LAYER;THAW;*;;RE;-LAYER;M;0;;-STYLE;STANDARD;SIMPLEX;;;;;;;SETBYLAYER;ALL;;;;LAD;-XR;D;*;-IMAGE;D;*;
;;  -LA;C;7;*;L;CONTINUOUS;*;LW;DEFAULT;*;;-PURGE;A;*;N;-PURGE;A;*;N;-PURGE;RE;*;N;AUDIT;Y;PDMODE;3;LSD;ZOOM;E;