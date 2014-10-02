#lang racket/gui

(require
 sgl/gl
 sgl/gl-vectors)

(define (resize w h)
  (glViewport 0 0 w h)
  #t
)

(define t 0.0)

(define (draw-opengl)
  (glClearColor 0.0 0.0 0.0 0.0)
  (glClear GL_COLOR_BUFFER_BIT)
  (glColor3d 1.0 1.0 1.0)

  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (glOrtho 0.0 1.0 0.0 1.0 -1.0 1.0)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)

  (define a (* 0.25 (cos t)))
  (define b (* 0.75 (cos t)))
  (set! t (+ t 0.01))
  
  (glBegin GL_QUADS)
  (glVertex3d a a 0.0)
  (glVertex3d b a 0.0)
  (glVertex3d b b 0.0)
  (glVertex3d a b 0.0)
  (glEnd)
)


(define my-canvas%
  (class* canvas% ()
    (inherit with-gl-context swap-gl-buffers)

   (define/override (on-paint)
      (with-gl-context
        (lambda ()
          (draw-opengl)
          (swap-gl-buffers)
        )
      )
    )

    (define/override (on-size width height)
      (with-gl-context
        (lambda ()
          (resize width height)
        )
      )
    )

    (super-instantiate () (style '(gl)))
  )
)

(define win (new frame% (label "OpenGl Test") (min-width 200) (min-height 200)))
(define gl  (new my-canvas% (parent win)))
(send win show #t)
