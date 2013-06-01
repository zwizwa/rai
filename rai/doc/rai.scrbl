#lang scribble/doc
@(require scribble/manual
          scribble/eval)

@(define-syntax-rule ($ id) (racket id))

@title{RAI}

RAI makes it possible to write VST/VSTi, Pd, and Jack Audio DSP code
in Scheme.

To install, run the following command.  See the Racket
@hyperlink["http://docs.racket-lang.org/pkg/" "package manager"]
documentation for more information.
@verbatim{
raco pkg install github://github.com/zwizwa/rai/master
}



@section{Introduction}

Racket Abstract Interpretation (RAI), is a code analysis and
generation system built around a domain specific language (DSL).  

The DSL is a purely functional dataflow language aimed at
describing digital signal processing (DSP) algorithms, and is embedded
in the @hyperlink["http://racket-lang.org" "Racket"] programming
language as @codeblock|{#lang s-exp rai/stream}|

The system allows for multiple abstract interpretations (AI) of
programs expressed in the DSL.  Some interpretations included are




@itemlist[

@item{Scheme code for interactive exploration}

@item{Dependency-free C code to run on small target systems}

@item{linear(ized) transfer function for frequency analysis}

@item{Automatic Differentiation}

@item{Racklog program for logic analysis}

@item{Symbolic mathematical expressions}

]

DSL programs are represented as abstract syntax, parameterized by a
structure describing the implementation of the language primitives.
Writing a new interpretation involves the execution of a program
parameterized by such a description, often in some form of context.

Program interpretations can be constructed to be composable, i.e. an
iterpretation can transform a RAI program into another RAI program.



@section{Time and Space}

The language consists mostly of two parts, reflecting the qualitative
difference of time and space dimensions.

@itemlist[

@item{An arithmetic core that can be given both a scalar and a stream
semantics, in that basic operators operate on scalar values or
infinite stream values, possibly collected in finite, nested
@emph{spatial} arrays.}

@item{A a collection of primitives that allows the creation of
@emph{causal temporal} relations, requiring a stream interpretation of
the input/output data.}

]

The design of the temporal primitives is motivated by applications from
the domain of musical signal processing (music DSP).  Such applications

@itemlist[

@item{rely heavily on the composition of output feedback systems such
as linear IIR filters and other state machines, and}

@item{often use a parameter update scheme that executes at a lower
sampling rate, i.e. the @emph{control rate}.}  ]

The semantics of the full stream processing language is quite close to
that of @hyperlink["http://faust.grame.fr" "Faust"].


@section{Memory--less Operations}

@itemlist[
@item{Arithmetic operations:
@$[+]
@$[-]
@$[*]
@$[/]
@$[sin]
@$[cos]
@$[exp]
@$[log]
...
}
@item{Finite array traversal with accumulator and array output: @$[loop]}
@item{Scheme language forms:
@$[define]
@$[lambda]
@$[let]
@$[let*]
@$[let-values]
@$[let*-values]
@$[values]
...
}
@item{Conditional Choice: @$[<]}
@item{Standard Scheme forms for @emph{unrolled} Metaprogramming}
]

@subsection{Composition}

Composing functions is the same as in Scheme:
@codeblock|{
#lang s-exp rai/stream
(define (square x)
  (* x x))
(define (cube x)
  (let ((sq (square x)))
    (* sq x)))
}|

@subsection{Iteration}

The @$[loop] construct is a combination of @$[map] style list to list
and @$[fold] style list to scalar operations.  This primitive closely
resembles the @$[feedback] mechanism for the time dimension.

As an illustration consider a program that takes an input array and
computes the accumulated array.

@codeblock|{
#lang s-exp rai/stream
(define (sum xs)
  (let-values*
     (((accu-out array-out)  ;; accumulators and output arrays
       (loop (i (n 3))       ;; loop index and array length
         ((accu 0))          ;; accumulators
         ((x xs))            ;; input element binding
         (let* ((sum (+ acc x))
                (out sum))
            (values sum      ;; accumulator out
                    out))))) ;; array element out
     array-out))
}|



@subsection{Conditional Choice}

There is no support for conditional code paths.  All choice is limited
to conditional data selection.  This means the language semantics can
be lifted in a straightforward way over SIMD vectors.

@section{Causal Stream Operations}

@itemlist[
@item{Unit delay output-feedback form: @$[feedback]}
@item{Support for variable length delay lines.}
@item{Control-rate operators: @$[hold] @$[setup]}
]

@subsection{Unit Delay Feedback}

Output feedback is specified using the @$[feedback] language
primitive, or a small layer of syntactic sugar using the @$[define]
form.  

The feedback operator takes a pure operator (function) and transforms
it into a causal stream operator.  This abstraction corresponds to a
canonical
@hyperlink["http://en.wikipedia.org/wiki/State_space_representation"
"State space"] representation.

Here is how to express a discrete integrator using the special
@$[define] form.

@codeblock|{
#lang s-exp rai/stream
(define (integrator (s) (x))
  (let ((sum (+ s x)))
     (values sum     ;; state update
             sum)))  ;; system output
}|

The state inputs are grouped separately from the ordinary inputs.  The
output of the function is the concatenation of state output and
ordinary outputs.


It can be interpreted both on the stream level, where input and output
streams correspond to shifted versions of each other, or at the scalar
level where the next time step's state vector and current output is
computed based on the current state and input.



@subsection{Delay Lines}

In music DSP applications, a frequently used form of output feedback
system is the @emph{delay line}.  An example of such a system would be

@codeblock|{
#lang s-exp rai/stream
(define (delay (s0 s1 s2 s3) (x))
  (let* ((from-delay s3)
         (to-delay (+ x (* 1/2 from-delay))))
     (values to-delay s0 s1 s2  ;; state updates
             to-delay)))        ;; output
}|

This is a 4-tap delay line.  Notice the pattern in the state output:
all state variables are shifted by one, and the head of the line is
updated with a new input, in this case a mix of input and delay output
feedback.

In typical applications, the size of the delay state can grow very
large.  It makes sense to introduce an abstraction to make
specification easier, and to also allow the extra annotation to guide
a more efficient implementation.  The approach taken in RAI is to
abstract the delay line state as an indexable array, and to limit the
construction of an updated delay state to the shift operation.

@codeblock|{
#lang s-exp rai/stream
(define (delay (s) (x))
  (let* ((from-delay (dl-ref s 3))  ;; delay state vector indexed read
         (to-delay (+ x (* 1/2 from-delay))))
     (values (dl-shift s to-delay)  ;; delay state vector update
             to-delay)))            ;; output
}|

@subsection{Control-rate operators}

The @$[hold] and @$[setup] operators implement a limited form of
subsampling, addressing very specific need in the target domain of
audio DSP.  In the future subsampling could be implemented in a more
general way.

The subsampling factor is determined by the @emph{block rate} of the
system, meaning the number of audio samples processed in a single
block for the VST, Pd and Jack interfaces.

The @$[hold] operator is a @emph{sample and hold} operator.  It will
sample a value at the first time instance of a sample block and hold
it for subsequent samples.  The @$[setup] operator is similar, in that
it will pass through its first argument at the first time instance,
and its second argument at any other.

The combination of both allows the implementation of per-block
computations, e.g. the computation of an expensive function evaluation
running at control rate, driving a cheaper interpolation scheme
running at audio rate.  A good example of this is the computation of
@emph{log scale} control parameters in a sound synthesizer or effect,
such as frequency or volume controls.  These controls are expensive to
compute compared to e.g. digital filters, but they do not need very
high bandwidth so can run at a lower rate.


@section{Unrolled Metaprogramming}

In some cases it is desirable to specify an algorithm at a higher
level using data structures and associated operations that are not
supported on a target platform.  E.g. in RAI the C target only
supports operations on (nested) linear arrays, which is a
straightforward translation of the @$[loop] construct.

In this case, ideally one would want to relate the higher level data
structures and manipulations to some optimized combination of loops
over arrays.  A good example of this is the way in which loop fusion
is performed in the
@hyperlink["http://dsl4dsp.inf.elte.hu/feldspar/index.html"
"Feldspar"] language.  Such an approach requires a certain amount of
trickery to be implemented.

There is another way to perform powerful code generation when one
drops to manipulation of scalar values, which relate to variable nodes
in a generated program.  This approach could be called the generation
of @emph{flat} or @emph{unrolled} code.

The RAI language contains primitives for array construction and
deconstruction that allows this form of flexible code generation to be
combined with the C target's array datastructures.  The automatic
array unpacking is performed in the @$[map] operator.


Keep in mind that while this can lead to efficient implementations if
the unrolling exposes optimization opportunities, in general the
unrolling can be quite inefficient concerning code size if it is
applied to large collections of data.




@section{Examples}

@subsection{Frequency analysis and C code generation}

A typical use case is to define a filter, plot its transfer function,
and when satisfied, generate some C code.

The filter can be defined in a separate module file or a submodule
form exporting an identifier referring to the abstract program.  This
program can then be passed to an analysis or code generation function
such as @$[ai-spectrum] or @$[ai-array-c] below.  The former can be
used in conjunction with Racket's @$[plot] facility.  The latter
produces a C syntax file containing a C function, structure
definitions for input, output and state, and preprocessor macros that
abstract meta data to facilitate integration in a C host framework.


@interaction[
(module test rai/stream
  (provide svf integrator)
  (define (integrator (s) (x))
    (let ((s (+ s x)))
      (values s s)))
  (define (svf (s1 s2) (in))
    (let* ((f 0.1)
           (q 0.3)
           (s2 (+ s2 (* f s1)))
           (s1 (- s1 (* f (+ (* q s1) s2 in)))))
      (values s1 s2
              s2))))

(require 'test)

(require rai/ai-freq)
(require plot)
(define (plot-filter f)
  (parameterize
    ((plot-x-transform log-transform)
     (plot-y-transform log-transform)
     (plot-x-ticks (log-ticks))
     (plot-y-ticks (log-ticks)))
  (plot (function 
          (compose magnitude
                   (ai-spectrum f))
          .0001 .5))))
(plot-filter integrator)
(plot-filter svf)

(require rai/ai-array-c)
(display (ai-array-c svf #:nsi 1))
]


@section{TODO / Known Issues}
@itemize[

@item{Boolean operators should be split into computation of condition
as integer bitmasks, following the way it is implemented on most SIMD
architectures.}

@item{State threading for delay lines do does not mix with the
@$[loop] construct.  Fixing this requires a better understanding of
how to mix constants appearing on two levels: in type
parameterizations and code. }

@item{Clean up support for subsampling and block processing in the
time dimension.}

]


@;References: Faust, FeldSpar, final-tagless
