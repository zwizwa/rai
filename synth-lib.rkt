#lang s-exp "stream.rkt"
(require "stream-lib.rkt"
         "stream-meta.rkt")
(provide (all-defined-out))

;; Audio Synth and Effect Tools.


;; Differentiated Parabolic Waveform
;; md5://81ef26b98b858bfc7ea351850b7f8872
(define (saw-d1 i)
  (- (* 2 (phasor i 0 1)) 1))

;; FIXME: when interpolating the amplitude, the divisions can go in a
;; `hold' form, outside of the main loop.
(define (saw-d2 i)
  (let* ((x  (saw-d1 i))
         (d  (diff (* x x)))
         (n  (/ d i))) ;; normalize
    n))

;; Same, but cubic + twice diff
(define (saw-d3 i)
  (let* ((x  (saw-d1 i))
         (x3 (* (- x 1) x (+ x 1)))
         (d  (diff (diff x3)))
         (n  (/ d (* i i)))) ;; normalize
    n))

;; Aliasing supersaw.
(define (supersaw freq spread)
  (mix (i (nb_saws 7))
       ()
       (let* ((i   (cast Float i))
              (n   (cast Float nb_saws))
              (dx  (- (* 2 (/ i n)) 1))
              (cube (* dx dx dx)))
         (phasor (* freq (+ 1 (* spread cube)))
                 -1 1))))

;; Envelope primitives.
;; All rate parameters are 1-p where p is a real pole.
;; See http://zwizwa.be/-/math/20130503-144443
;; for rate parameter mapping.

;; (plot-t 100 env-AD '(1 0 0 0 0 0 0 0 1 0 0 0 0 0) .7 .5)

(define (env-AD (level setpoint rate prev-gate) (gate attack decay))
  (let*
      (;; Maximum attack value.  Make this closer to 1 for more punch.
       (a-max 0.9)
       (prev-gate<gate (< prev-gate gate))
       (a-max<level    (< a-max level))
       
       (next-level
        (+ level (* rate (- setpoint level))))
       (next-rate
        (if prev-gate<gate  attack  ;; 0->1  start new cycle
        (if a-max<level     decay   ;; attack done, start decay
                            rate))) ;; keep coef
       (next-setpoint
        (if prev-gate<gate  1            ;; 0->1  start new cycle
        (if a-max<level     0            ;; attack done, start decay
                            setpoint)))) ;; keep coef
    (values next-level
            next-setpoint
            next-rate
            gate
            ;; out.  1 sample delayed to pipeline computations
            level)))

(define (env-AR (level) (gate attack release))
  (let*
      ((rate       (if (< 0 gate) attack release))
       (next-level (+ level (* rate (- gate level)))))
    (values next-level
            level)))


(define (env-ADSR (level setpoint rate prev-gate)
                  (gate attack decay sustain release))
  (let*
      (;; Maximum attack value.  Make this closer to 1 for more punch.
       (a-max 0.9)
       (0<gate         (< 0 gate))
       (prev-gate<gate (< prev-gate gate))
       (a-max<level    (< a-max level))
    
       (next-level
        (+ level (* rate (- setpoint level))))
       (next-rate
        (if 0<gate
            ;; gate on
            (if prev-gate<gate  attack ;; 0->1  start new cycle
            (if a-max<level     decay  ;; attack done, start decay
                                rate)) ;; keep coef
            ;; gate off
            release))
        
       (next-setpoint
        (if 0<gate
            ;; gate on
            (if prev-gate<gate  1          ;; 0->1  start new cycle
            (if a-max<level     sustain    ;; attack done, start decay
                                setpoint)) ;; keep coef
            ;; gate off
            0)))

    (values next-level
            next-setpoint
            next-rate
            gate
            ;; out.  1 sample delayed to pipeline computations
            level)))


;; Saturation functions, defined on [-1,1] with flat points at +- 1.
;; Scaled such that -1,0,+1 are fixed points, to facilitate iteration.

;; These functions are derived from integrals of bump functions
;; f'_n(x) = (n-1)/n (1-x^2)^n where the flatness condition is easily
;; expressed as the multiplicity of zeros of the deriviative of the
;; saturation function at +-1.

;; `sat' and `sat2' have 1 and 2 zeros resp. in the derivative at +-1.

;; After n iterations of the integrated bumps, the orders are 2^n-1
;; and 3^n-1 resp.  Iteration gives high polynomial order without the
;; expense of having to evaluate a lot of terms.

;; Because of extreme flatness after iterating a couple of times, the
;; range can practically be extended beyond -1 and 1.  Just plot the
;; function and this will be pretty clear.

(define (sat1 x)
  (* 1/2 x (- 3 (^ x 2))))

(define (sat2 x)
  (let ((x^2 (^ x 2)))
    (* x (poly x^2 (vector 15/8 -5/4 3/8)))))

;; Clipping saturation function.
(define (clip1 x) (clip x -1 1))


;; Symplectic Euler approximation of analog SVF filter, with stability
;; guard on both variables

;; s1 = band pass
;; s2 = low pass


(define (svf-update s1 s2 i f q)
  (let* ((_s2 (+ s2 (* f s1)))
         (_s1 (- s1 (* f (+ (* q s1) _s2 i)))))
    (values _s1 _s2)))

;; Multiple iterations.
(define (svf-n  (s1 s2) (i f q n sat))
  (let-values (((s1 s2) (iterate n
                           svf-update
                           s1 s2 i f q)))
    (let ((s1 (sat s1))
          (s2 (sat s2)))
      (values s1 s2
              s1 s2))))

;; State saturation.
(define (sat x)
  (let ((x (clip1 x)))
    (* x (- 1 (/ (^ x 2) 3)))))


(define (svf-lp i f q)
  (let-values (((bp lp) (svf-n i f q 1
                               ;; svf-sat
                               clip1
                               )))
    lp))

;; Filters derived from complex one-pole with real input.
;; The pole(s) are c+-js.
(define (pole-p-update x y i c s)
  (let ((_x (+ (* (- 1 c) i) (- (* c x) (* s y))))
        (_y (+ (* c y) (* s (- x i)))))
    (values _x _y)))

(define (pole-p (x y) (i c s))
  (let-values (((x y) (pole-p-update x y i c s)))
    (values x y
            x y)))

(define (pole-lp i cos sin mag)
  (let-values (((x y) (pole-p i (* mag cos) (* mag sin))))
    x))



;; Approximation of 2^x.
;; These are for "human control" to compute logarithmic knob scales.
;; Don't use them for anything that needs to be very accurate.


;; This 2nd order approximation on [0,1] is designed for a piecewize
;; computation where derivatives 0 and 1 line up, i.e.:
;;    f(0)=1, f(1)=2, f'(0)=f'(1)=2/3

;; However, it is used here in an iterative range-extending algorithm,
;; so the design parameters could be changed to e.g. a minimax
;; approach.

;; ( In practice this doesn't seem to matter much.  At least to my
;; human ears, "scale accuracy" of perception is quite low.  If the
;; scale is roughly logarithmic/exponential, any slightly deviating
;; curvature seems to be readily compensated for. )

(define (~2^x-01 x)
  (poly x (vector 1 2/3 1/3)))

;; Convention: the ~ prefix means approximation.

;; Extend the argument range from 0->1 to 0->2^n
;; based on the relation 2^x = (2^(x / 2^n))^(2^n)
(define (~2^x-range x n)
  (let* ((2^n      (^ 2 n))
         (x/2^n    (/ x 2^n))
         ;; Compute approximation with scaled down argument.
         (2^_x/2^n (~2^x-01 x/2^n)))
    ;; Scale up range again using successive squaring.
    (iterate n ^ 2^_x/2^n 2)))

;; It looks like we have a lucky coincidence.  Most logarithmic
;; control needs about 4 to 5 orders of magnitude.  We have 2^{2^4} =
;; 2^16 ~ 10^4.8 which is just perfect.  The relative accuracy is
;; about 5%.  The log of the full range produces a curve that looks
;; pretty much straight to me.

(define (~2^x x) (~2^x-range x 4))

(define (log2 x) (/ (log x) (log 2)))

;; Mapping MIDI controllers to log range.  The design range is max/min
;; ratio of 10^4.8 = 2^{2^4}, but it seems to gracefully degrade both
;; for higher ratios and for negative scales.

;; The name T refers to the sample period (scale value for
;; frequencies), but is a generic scaling factor.

(define (p-log scale x min max T)
  (let* ((exp-scale (* (/ 1 scale) (log2 (/ max min)))))
    (* (* min T) (~2^x (* exp-scale x)))))


;; Parameter interpolation (de-zipping) for:
;; - single logarithmic frequency param, e.g. for linear phasor
;; - log freq complex phasor
;; - log freq phasor with log damping


;; Log param conversion + linear interpolation with additional
;; control-rate smoothing = 1/2 step reduction (sounds better).
(define (p-log/i (state)
                 (scale x min max ts) ;; input
                 (t T)                ;; time coordinates
                 (step))              ;; per sample increment
  (let*
      ((step
        ;; Compute step increment only at start of time block.
        (hold 
         (let* ((target (p-log scale x min max ts))
                (step (/ (- target state) T)))
           ;; Some extra per block smoothing.
           (* 1/2 step))))
       ;; Linear ramp.
       (state (+ state step)))
    (values state state)))

(define (p-phasor/i-control scale   ;; input interpolation scale
                            c s     ;; actual last state
                            a       ;; desired last angle
                            x       ;; control val (0-scale)
                            min max ;; log scale min/max
                            1/fs    ;; sample time step in seconds
                            T)      ;; time window

  ;; We want to interpolate the complex phasor path using successive
  ;; rotation.  The rotation angle is computed based on the previous
  ;; angle and the current setpoint.
  (let*-values
      (((2pi)   (* 2 3.141592653589793))
       ;; Amplitude will drift if not compensated.
       ((c)     (- c .00001)) ;; startup HACK
       ((c s)   (normalize-2D c s))
       
       ;; The angle da between actual and setpoint will drift if not compensated.
       ;; c + i s / c0 + i s0 = e ^ i da ~= 1 + i da  (accurate, da small)
       ((c0 s0) (~cos/sin a))
       ((_ da)  (cdiv-phasor  c s  c0 s0))

       ;; ((_) (debug da))

       ;; Compute current angle a0, end angle a1, incremental rotation phasor.
       ((a0)    (+ a da))
       ((a1)    (p-log scale x (* 2pi min) (* 2pi max) 1/fs))
       ((c_ s_) (~cos/sin (/ (- a1 a0) T))))

    (values c s a1   ;; Normalized state + setpoint angle.
            c_ s_))) ;; Loop-local variable for linear phase interpolation.




(define (p-phasor/i ((c 1) (s 0) a)
                    (scale x min max 1/fs)
                    (t T))
  (let*-values
      ;; Control rate (at start of signal block)
      ;; Compute loop code and normalized state.
      (((ch sh ah c_ s_)
        (hold 
         (p-phasor/i-control
          scale
          c s a x min max 1/fs T)))
       ;; Update state variable only at start of block.
       ((c s a) (setup
                 (values ch sh ah)
                 (values c  s  a)))
       ;; Signal rate is incremental rotation using the angle computed
       ;; at control rate.
       ((c s) (cmul c s c_ s_)))
    (values c s a
            c s)))

         
    
;; Approximate normalization for a^2 + b^2 close to 1
;; Exact coef: r = (a^2 + b^2)^1/2, a -> r a,  b -> r b
;; Since this is close to 1 we can use 1st order Taylor:
;; r = 1 / (1 - x)^1/2  ~=  1 + 1/2 x   with  x = 1 - (a^2 + b^2)

;; Maybe using a -> a + (r1 * a) with r1 = 1/2 x is more stable?
;; Don't think so, since x does not have many significant bits, it's
;; probably OK to hide it in the LSBs of r.

(define (normalize-2D a b)
  (let* ((a2 (* a a))
         (b2 (* b b))
         (r (- 3/2 (* 1/2 (+ a2 b2)))))
    (values (* r a)
            (* r b))))

;; Complex square.
(define (csquare c s)
  (let ((c (- (* c c) (* s s)))
        (s (* 2 (* c s))))
    (values c s)))

(define (cmul r0 i0
              r1 i1)
  (values 
   (- (* r0 r1) (* i0 i1))
   (+ (* r1 i0) (* r0 i1))))

;; Divide phasors by multiplying complex conjugate.
(define (cdiv-phasor r0 i0
                     r1 i1)
  (values 
   (+ (* r0 r1) (* i0 i1))
   (- (* r1 i0) (* r0 i1))))
  
    

;; Approximation of cosine, sine, based on taylor expansion followed
;; by normalization, followed by iterated squaring.
(define (~cos/sin-n theta
                    n1  ;; Taylor expansion approximation order (FIXME: unit?)
                    n2  ;; Nb of normalization iterations
                    n3) ;; Nb of iterated squarings
  (let* ((theta (/ theta (^ 2 n3)))
         (c (t-cos theta n1))
         (s (t-sin theta n1)))
    (let*-values
        (((c s) (iterate n2 normalize-2D c s))
         ((c s) (iterate n3 csquare c s)))
      (values c s))))
   
;; Main approximation used for frequency parameter -> pole mapping.
;; The complex number c+is approximates magnitude 1 from below:
;; necessary for stability constraints.

;; To verify, plot angle, magnitude of against the range [-pi, pi].
;; Magnitude is very accurate in the most important low frequency
;; range, with maximum deviation 0.99994 at +- pi.
(define (~cos/sin theta)
  (~cos/sin-n theta 1 1 2))

(define (~cos t) (let-values (((c s) (~cos/sin t))) c))
(define (~sin t) (let-values (((c s) (~cos/sin t))) s))


;; Complex log.  Input needs to be of unit norm.

(define (log-phasor-step c s)
  (normalize-2D (* 1/2 (+ 1 c))
                (* 1/2 s )))

;; This only works when c>0.
(define (log-phasor c s n)
  (let-values (((c s) (iterate n log-phasor-step c s)))
    (* s (^ 2 n))))

        
;; Stretched exponential to compute logarithmic scale with linear and
;; inverse linear endpoints.
;; zwizwa.be://-/math/20130503-144443
;;
;; Input is [-1,1], output is [0, \infty].
;; Mid point is 1, so output can be scaled to desired midpoint.
;;
;; Approximation around 0 is x -> exp(2 n x), where the n scaling is
;; obtained using successive squaring.
;;
;; Note if x is small this is a good approx for an exponential.
(define (exp/s-1 x)
  (/ (+ 1 x)
     (- 1 x)))
(define (exp/s-n x n)
  (let* ((se (exp/s-1 x))
         (out (iterate n (^ se 2))))
    out))

;; This is a "good enough" scale for time constant parameters, using a
;; single post squaring step.  The "flat" range is about 3 decades
;; over 70% of the range, with 3 more at either side.  To check,
;; perform a log plot over [-1,1].
;; (define (exp/s x) (exp/s-n x 1))



;; Asymmetric stretched exponential gain knob.  This maps [0,1] ->
;; [0,1], with linear behaviour around 0 and exponential behaviour
;; around 1.  Works well for an audio gain knob.
(define (exp/sa-1 x) (/ x (- 2 x)))

(define (doc:exp/sa-1 x) (* 20 (log (exp/sa-1 x))))



;; Attack/Decay/Release time constant dials.
;;
;; This function makes sure the 2 values derived from the [0,1]
;; control parameter range are the same:
;;
;; - Convert a [0,1] parameter range (VST API) to fs-normalized values.
;; - Export the GUI meta parameters in milliseconds units using the `slog' scale.
;;
;; Output value is a fs-normalized real pole p, represented as 1-p.
;;
;; It also inserts a `hold' operation, to make sure the interpolation
;; curve is only computed once per control-rate update.  Rate changes
;; don't need to be de-zipped as they only produce discontinuous
;; derivatives.
;;
;;
;; The curve used is a stretched exponential without output squaring.
;; zwizwa.be://-/math/20130503-144443
;;
;; From experiments, squaring doesn't seem necessary.  Picking a good
;; center value for the rate knob is more important.
;;
(define (p-rate-ms range in name center-ms fs)
  (params range
          ((in slog name "ms" center-ms 1))
          (hold
           (let* ;; Normalize units.
               ((n_in     (- (* in (/ 2 range)) 1))
                (n_center (* (/ center-ms 1000) fs)))
             (1-pole/s n_in n_center)))))

;; Compute real pole magnitude based on stretched exponential.  Same
;; as 1 / (1 + a ((1+x) / (1-x))), rearranged to need only 1 division.
;;
;; a : center scale time constant
;; x : -1,1 scale range
(define (1-pole/s x a)
  (let* ((1-x (- 1 x))   ;; (*)
         (1+x (+ 1 x)))
    (/ 1-x (+ 1-x (* a 1+x)))))

;; Validation: this test function maps [-1,1] -> actual decay time.
;; plot as (lin/log pole-1/s-test -.99 .99) and judge the curve.
(define (doc:1-pole/s x)
  (let* ((typical-decay 10000)
         (pole (- 1 (1-pole/s x typical-decay))))
    (/ -1 (log pole))))





(define (db->gain db) (^ 10 (/ db 20)))

;; Generic gain knob with parameterized internal log function.
(define (p-gain-db/? p-log range in name min-db max-db)
  (params range
          ((in lin name "dB" min-db max-db))
          (let*
              ((min (db->gain min-db))
               (max (db->gain max-db)))
            (p-log range in min max 1))))
            
;; Dezipped and sampled gain knobs.
(define (p-gain-db/i r i n min max) (p-gain-db/? p-log/i r i n min max))
(define (p-gain-db   r i n min max) (p-gain-db/? p-log   r i n min max))


(define (p-percent range in name)
  (params range
          ((in lin name "%" 0 100))
          in))


(define (p-freq-Hz/i range f name min max 1/fs)
  (params range
          ((f log name "Hz" min max))
          (p-log/i range f min max 1/fs)))




;; Delay lines


;; Delay abstraction.

;; Note: for parallel delay, it seems simplest to add a pipeline delay
;; for computing the feedback operation.  This allows abstraction of
;; the delay operator as a separate stream operator, avoiding its
;; awkward dl-shift / dl-shift interface (which will probably change
;; once I know how to do this better..)


(define (delay/fixed (line) (in time))
  (let ((out (dl-ref line time)))
    (values (dl-shift line in)
            out)))

(define (delay/fixed-fb (line) (in time fb))
  (let* ((out (dl-ref line time))
         (del-in (+ in (* fb out))))
    (values (dl-shift line del-in)
            del-in)))


;; Variable delay line.  Cross-fading param update, once per control
;; rate block.  `max' needs to be a literal, as it determines the
;; maximal delay.

(define (delay/var (d0 line) (x d max) (t T))
  (let* (;; Control rate
         (1/T (hold (/ 1 T)))
         (d1  (hold (clip d 1 max)))
         (_   (hold (dl-ref line max)))  ;; dummy read sets max
         ;; Signal rate: linear interpolation between 2 delay taps.
         (x0  (dl-ref line d0))
         (x1  (dl-ref line d1))
         (frac  (* t 1/T))
         (mixed (+ (* x0 (- 1 frac))
                   (* x1 frac))))
    (values d1
            (dl-shift line x)
            mixed)))


;; Householder reflection for FDN.
(define (householder vec-in)
  (let* ((2/N (/ 2 (length vec-in)))
         (sum (vector-sum vec-in))
         (spread (* 2/N sum))
         (vec-out (map (lambda (x) (- x spread)) vec-in)))
    vec-out))


;; Core FDN routine.

;; The mixer equation is pipelined to keep the delays parallel, making
;; them easier to abstract behind a single stream op.  (FIXME: Need to
;; fix a language problem: can't perform householder on d first
;; because size it not known! )

(define (fdn (d) (x dts filter))
  (let* ((d (map delay/fixed d dts))
         (d (householder d))
         (d (map filter d))
         (d (map (lambda (d) (+ x d)) d)))
    (values d d)))


;; 1-st order filter for delay line feedback.
;; Gain settable at DC and NY.
(define (fdn-filt-gen x lo hi)
  (let* ((x1 (z^-1 x))
         (dc (+ x x1))
         (ny (- x x1)))
    (+
     (* (* 1/2 lo) dc)
     (* (* 1/2 hi) ny))))



;; Example: fixed decay filter.
(define (fdn-filt x)
  (fdn-filt-gen x .99 .7))
;; Example: Stereo FDN with 4 delay lines and fixed decay.
(define (fdn4 x dts)
  (let-values (((a b c d)
                (unpack (fdn x dts fdn-filt))))
    (values (+ a b)
            (+ c d))))
;; Example: fdn4 with fixed delay lines.
(define (fdn-dense x)
  (fdn4 x (vector  702
                  1097
                  3239
                  7997)))
                  

;; Misc tools
(define (id x) x)



            
  
