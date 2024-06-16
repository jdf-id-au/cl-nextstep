(in-package :cg)

;; =======================================================
;; CoreGraphics frameworks 
;; =======================================================

(defmacro cgfloat (x) `(float ,x 1.0d0)) ; 1.0d0 exemplifies type

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defun cgify-name (symbol)
    "Convert 'lisp-name into \"CGContextLispName\"."
    (let* ((segs (uiop:split-string (string symbol) :separator '(#\-)))
           (title (loop for seg in segs
                        collect (cond ((string= seg "CTM") "CTM")
                                      ((string= seg "EO") "EO")
                                      (t (string-capitalize seg))))))
      (format nil "CGContext~{~a~}" title)))
  
  (defun interpret-args (args)
    "Convert list of args, with optional interleaved type keywords
(default :double), into defun-args (and cffi-args)."
  (let ((next-type :double)
        (defun-args (list))
        (cffi-args (list)))
    (dolist (arg args)
      (if (keywordp arg)
          (setf next-type arg)
          (progn
            (setf defun-args (nconc defun-args `(,arg)))
            (setf cffi-args
                  (nconc cffi-args
                         (case next-type
                           (:double `(:double (cgfloat ,arg)))
                           (:rect `((:struct ns:rect) ,arg))
                           (otherwise `(,next-type ,arg)))))
            (setf next-type :double))))
    (values defun-args cffi-args))))

;; (cgify-name 'scale-ctm)
;; => "CGContextScaleCTM"

(defmacro defcall (symbol &optional args)
  "Simplify cffi Core Graphics defuns."
  (multiple-value-bind (defun-args cffi-args) (interpret-args args)
    `(defun ,symbol (context ,@defun-args)
       (cffi:foreign-funcall ,(cgify-name symbol)
                             :pointer context
                             ,@cffi-args))))

;; (macroexpand-1 '(defcall stroke-rect-with-width (:rect rect width)))
;;  => (DEFUN STROKE-RECT-WITH-WIDTH (CONTEXT RECT WIDTH)
;;   (CFFI:FOREIGN-FUNCALL "CGContextStrokeRectWithWidth" :POINTER CONTEXT
;;                         (:STRUCT CL-NEXTSTEP:RECT) RECT :DOUBLE
;;                         (CGFLOAT WIDTH)))
;; T

(defcall save-g-state) ; API change from `save-gstate`
(defcall restore-g-state)
(defcall scale-ctm (sx sy))
(defcall translate-ctm (tx ty))
(defcall rotate-ctm (radians))

;; CGAffineTransform 구조체 필요..
;; (defun context-concat-ctm (context transform)
;;   (cffi:foreign-funcall "CGContextConcatCTM" :pointer context  transform))

;; (defun context-get-ctm (context)
;;   (let ((ctm (make-gcable-record #>CGAffineTransform)))
;;     (#_CGContextGetCTM ctm context)
;;     ctm))

(defcall set-line-width (width))
(defun set-line-cap (context cap)
  (let ((code (case cap
                (:butt 0) ;; kCGLineCapButt
                (:square 2) ;; kCGLineCapSquare
                (:round 1) ;;  kCGLineCapRound
                (otherwise cap))))
    (cffi:foreign-funcall "CGContextSetLineCap" :pointer context :int code)))
(defun set-line-join (context join)
  (let ((code (ecase join
                (:miter 0) ;; kCGLineJoinMiter
                (:round 1) ;; kCGLineJoinRound
                (:bevel 2) ;; kCGLineJoinBevel
                (otherwise join))))
    (cffi:foreign-funcall "CGContextSetLineJoin" :pointer context :int code)))
(defcall set-miter-limit (limit))

;; (defun context-set-line-dash (context phase lengths)
;;   (let ((n (length lengths)))
;;     (%stack-block ((p (* n (record-length #>CGFloat))))
;;       (dotimes (i n)
;;         (setf (paref p (:array #>CGFloat) i) (cgfloat (elt lengths i))))
;;       (#_CGContextSetLineDash context (cgfloat phase) p n))))

(defcall set-flatness (flatness))
(defcall set-alpha (alpha))

(defparameter *blend-mode-alist*
  '((:normal . 0)
    (:multiply . 1)
    (:screen . 2)
    (:overlay . 3)
    (:darken . 4)
    (:lighten . 5)
    (:color-dodge . 6)
    (:color-burn . 7)
    (:soft-light . 8)
    (:hard-light . 9)
    (:difference . 10)
    (:exclusion . 11)
    (:hue . 12)
    (:saturation . 13)
    (:color . 14)
    (:luminosity . 15)
    (:clear . 16)
    (:copy . 17)
    (:source-in . 18)
    (:source-out . 19)
    (:source-atop . 20)
    (:destination-over . 21)
    (:destination-in . 22)
    (:destination-out . 23)
    (:destination-atop . 24)
    (:xor . 25)
    (:plus-darker . 26)
    (:plus-lighter . 27)))

(defun set-blend-mode (context mode)
  (let ((code (or (cdr (assoc mode *blend-mode-alist*))
                  mode)))
    (cffi:foreign-funcall "CGContextSetBlendMode" :pointer context :int code)))

(defcall begin-path)
(defcall monve-to-point (x y))
(defcall add-line-to-point (x y))
(defcall add-curve-to-point (cp1x cp1y cp2x cp2y x y))
(defcall add-quad-curve-to-point (cpx cpy x y))
(defcall close-path)

;;; Path construction convenience functions
#|
CGContextAddRect
CGContextAddRects
CGContextAddLines
CGContextAddEllipseInRect
|#

(defcall add-arc (x y radius start-angle end-angle :int clockwise))

#|
CGContextAddArcToPoint
CGContextAddPath
|#

;;; Path stroking
#|
CGContextReplacePathWithStrokedPath
|#

;;; Path information functions
#|
CGContextIsPathEmpty
CGContextGetPathCurrentPoint
CGContextGetPathBoundingBox
CGContextCopyPath
CGContextPathContainsPoint
|#

;;; Path drawing functions
(defun draw-path (context mode)
  (let ((code (case mode
                (:fill 0) ;; kCGPathFill
                (:eofill 1) ;; kCGPathEOFill
                (:stroke 2) ;; kCGPathStroke
                (:fill-stroke 3) ;; kCGPathFillStroke
                (:eofill-stroke 4) ;; kCGPathEOFillStroke
                (otherwise mode))))
    (cffi:foreign-funcall "CGContextDrawPath" :pointer context :int code)))

(defcall fill-path)
(defcall eo-fill-path)
(defcall stroke-path)
(defcall fill-rect (:rect rect))

;; (defun fill-rects (context rects count)
;;   (cffi:foreign-funcall "CGContextFillRects" :pointer context (:struct cg:rect) rects :int count))

(defcall stroke-rect (:rect rect))
(defcall stroke-rect-with-width (:rect rect width))
(defcall clear-rect (:rect rect))
(defcall fill-ellipse-in-rect (:rect rect))
(defcall stroke-ellipse-in-rect (:rect rect))

;; (defun stroke-line-segments (context points count)
;;   (cffi:foreign-funcall "CGContextStrokeLineSegments" :pointer  context points count))

;;; Clipping functions
#|
CGContextClip
CGContextEOClip
CGContextClipToMask
CGContextGetClipBoundingBox
|#

;;; Clipping convenience functions
#|
CGContextClipToRect
CGContextClipToRects
|#

(defcall set-fill-color-with-color (:pointer color))
(defcall set-stroke-color-with-color (:pointer color))
(defcall set-fill-color-space (:pointer space))
(defcall set-stroke-color-space (:pointer space))

;;; Color functions
#|
CGContextSetFillColor
CGContextSetStrokeColor
|#

;;; Pattern functions
#|
CGContextSetFillPattern
CGContextSetStrokePattern
CGContextSetPatternPhase
|#

;;; Color convenience functions
(defun set-gray-fill-color (context gray &optional (alpha 1))
  (cffi:foreign-funcall "CGContextSetGrayFillColor" :pointer context :double (cgfloat gray) :double (cgfloat alpha)))

(defun set-gray-stroke-color (context gray &optional (alpha 1))
  (cffi:foreign-funcall "CGContextSetGrayStrokeColor" :pointer context :double (cgfloat gray) :double (cgfloat alpha)))


(defun set-rgb-fill-color (context red green blue &optional (alpha 1))
  (cffi:foreign-funcall "CGContextSetRGBFillColor" :pointer context
						   :double (float red 1.0d0)
						   :double (float green 1.0d0)
						   :double (float blue 1.0d0)
						   :double (float alpha 1.0d0)))

(defun set-rgb-stroke-color (context red green blue &optional (alpha 1))
  (cffi:foreign-funcall "CGContextSetRGBStrokeColor" :pointer context
						   :double (float red 1.0d0)
						   :double (float green 1.0d0)
						   :double (float blue 1.0d0)
						   :double (float alpha 1.0d0)))

(defun set-cmyk-fill-color (context cyan magenta yellow black &optional (alpha 1))
  (cffi:foreign-funcall "CGContextSetCMYKFillColor" :pointer context :double (cgfloat cyan) :double (cgfloat magenta)
			:double (cgfloat yellow) :double  (cgfloat black) :double (cgfloat alpha)))

(defun set-cmyk-stroke-color (context cyan magenta yellow black &optional(alpha 1))
  (cffi:foreign-funcall "CGContextSetCMYKStrokeColor" :pointer context :double (cgfloat cyan) :double (cgfloat magenta)
			:double (cgfloat yellow) :double (cgfloat black) :double (cgfloat alpha)))


;;; Rendering intent
#|
CGContextSetRenderingIntent
|#

;;; Image functions
#|
CGContextDrawTiledImage
CGInterpolationQuality
CGContextSetInterpolationQuality
|#
(defcall draw-image (:rect rect :pointer cg-image))

;;; Shadow support
#|
CGContextSetShadowWithColor
CGContextSetShadow
|#

;; (defun context-set-shadow (context dx dy blur)
;;   (rlet ((offset #>CGSize :width (cgfloat dx) :height (cgfloat dy)))
;;     (#_CGContextSetShadow context offset (cgfloat blur))))

;;; Gradient and shading functions
#|
CGContextDrawLinearGradient
CGContextDrawRadialGradient
CGContextDrawShading
|#

;;; Text functions
(defcall set-character-spacing (spacing))
(defcall set-text-position (x y))

;; (defun context-get-text-position (context)
;;   (let ((pt (make-gcable-record #>CGPoint)))
;;     (#_CGContextGetTextPosition pt context)))

#|
CGContextSetTextMatrix
CGContextGetTextMatrix
|#
(defun set-text-drawing-mode (context mode)
  (let ((code (case mode
                (:fill 0) ;; kCGTextFill
                (:stroke 1) ;; kCGTextStroke
                (:fill-stroke 2) ;; kCGTextFillStroke
                (:invisible 3) ;; kCGTextInvisible
                (:fill-clip 4) ;; kCGTextFillClip
                (:stroke-clip 5) ;; kCGTextStrokeClip
                (:fill-stroke-clip 6) ;; kCGTextFillStrokeClip
                (:clip 7) ;; kCGTextClip
                (otherwise mode))))
    (cffi:foreign-funcall "CGContextSetTextDrawingMode" :pointer context :int code)))

;; (defun create-font (name)
;;   (let* ((cf-name (ns:make-cf-string name)))
;;     (unwind-protect (cffi:foreign-funcall "CGFontCreateWithFontName"
;; 					  :pointer cf-name
;; 					  :pointer)
;;       (ns:cf-release cf-name))))

(defcall set-font (:pointer font))
(defcall set-font-size (size))

(defun select-font (context font-name size encoding)
  (let ((code (case encoding
		(:macroman 1) ;; #$kCGEncodingMacRoman
		(:font-specific 0) ;; #$kCGEncodingFontSpecific
		(otherwise encoding))))
    (cffi:foreign-funcall "CGContextSelectFont" :pointer context :string font-name :double (cgfloat size) :int code)))
;; #|
;; CGContextShowGlyphsAtPositions
;; |#

;; ;;; Text convenience functions
(defun show-text (context string)
  (let ((n (length (babel:string-to-octets string))))
    (cffi:foreign-funcall "CGContextShowText" :pointer context :string string :int n)))

(defun show-text-at-point (context x y string)
  (let ((n (length (babel:string-to-octets string))))
    (cffi:foreign-funcall "CGContextShowTextAtPoint" :pointer context :double (cgfloat x) 
			  :double (cgfloat y) :string string :int n)))

#|
CGContextShowGlyphs
CGContextShowGlyphsAtPoint
CGContextShowGlyphsWithAdvances
|#

;;; PDF functions
#|
CGContextDrawPDFPage
|#

;;; Output page functions
#|
CGContextBeginPage
CGContextEndPage
|#

#|
CGContextFlush
CGContextSynchronize
|#

;;; Antialiasing functions
#|
CGContextSetShouldAntialias
CGContextSetAllowsAntialiasing
|#

;;; Font display functions
#|
CGContextSetShouldSmoothFonts
CGContextSetAllowsFontSmoothing
CGContextSetShouldSubpixelPositionFonts
CGContextSetAllowsFontSubpixelPositioning
CGContextSetShouldSubpixelQuantizeFonts
CGContextSetAllowsFontSubpixelQuantization
|#

;;; Transparency layer support
#|
CGContextBeginTransparencyLayer
CGContextBeginTransparencyLayerWithRect
CGContextEndTransparencyLayer
|#

;;; User space to device space transformations
#|
CGContextGetUserSpaceToDeviceSpaceTransform
CGContextConvertPointToDeviceSpace
CGContextConvertPointToUserSpace
CGContextConvertSizeToDeviceSpace
CGContextConvertSizeToUserSpace
CGContextConvertRectToDeviceSpace
CGContextConvertRectToUserSpace
|#


;;; CGPath

#|
CGPathCreateMutable
CGPathCreateCopy
CGPathCreateCopyByTransformingPath
CGPathCreateMutableCopy
CGPathCreateMutableCopyByTransformingPath
CGPathCreateWithRect
CGPathCreateWithEllipseInRect
CGPathCreateCopyByDashingPath
CGPathCreateCopyByStrokingPath
CGPathEqualToPath

|#

;; (defun path-move-to-point (path transform x y)
;;   (when (null transform) (setq transform +null-ptr+))
;;   (#_CGPathMoveToPoint path transform (cgfloat x) (cgfloat y)))

;; (defun path-add-line-to-point (path transform x y)
;;   (when (null transform) (setq transform +null-ptr+))
;;   (#_CGPathAddLineToPoint path transform (cgfloat x) (cgfloat y)))

#|
CGPathAddQuadCurveToPoint
CGPathAddCurveToPoint
CGPathCloseSubpath
CGPathAddRect
CGPathAddRects
CGPathAddLines
CGPathAddEllipseInRect
CGPathAddRelativeArc
CGPathAddArc
CGPathAddArcToPoint
CGPathAddPath
CGPathIsEmpty
CGPathIsRect
CGPathGetCurrentPoint
CGPathGetBoundingBox
CGPathGetPathBoundingBox
CGPathContainsPoint
|#
