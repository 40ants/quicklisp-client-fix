This patch fixes two problems:

* Is that quicklisp is unable to load primary system's dependencies, which are subsystems of other primary system not mentioned in the quicklisp distribution's metadata. Issue https://github.com/quicklisp/quicklisp-client/pull/139
* When asdf system is known to ASDF, quicklisp client ignores it's dependencies and again, if the system depends on a subsystem of other primary system, ASDF can't load it and quicklisp client too.

## How to reproduce the problem

For example, I have `reblocks-ui-docs` ASDF system. One of it's subsystems depends on `reblocks/doc/example` subsystem of other package-inferred system available from Quicklisp.

Checkout this repository somewhere:

```
git clone https://github.com/40ants/reblocks-ui /tmp/reblocks-ui
```

Then start lisp and add this path to `asdf:*central-registry*`:

```lisp
CL-USER> (push "/tmp/reblocks-ui/" asdf:*central-registry*)
```

Try to quickload the system:

```
CL-USER> (ql:quickload "reblocks-ui-docs")
```

During quickload `reblocks-ui-docs` ASDF tries to load `reblocks/doc/example` and fails. QL client handles the condition, but can't find it in the metadata:

```lisp
System "reblocks/doc/example" not found
   [Condition of type QUICKLISP-CLIENT:SYSTEM-NOT-FOUND]

Restarts:
 0: [CONTINUE] Try again
 1: [ABORT] Give up on "reblocks/doc/example"
 2: [ABORT] Give up on "reblocks-ui-docs"
 3: [REGISTER-LOCAL-PROJECTS] Register local projects and try again.
 4: [RETRY] Retry SLY mREPL evaluation request.
 5: [*ABORT] Return to SLY's top level.
 --more--

Backtrace:
 0: ((LABELS QUICKLISP-CLIENT::RECURSE :IN QUICKLISP-CLIENT::COMPUTE-LOAD-STRATEGY) "reblocks/doc/example")
 1: (QL-DIST::CALL-WITH-CONSISTENT-DISTS #<FUNCTION (LAMBDA NIL :IN QUICKLISP-CLIENT::COMPUTE-LOAD-STRATEGY) {1004074BFB}>)
 2: (QUICKLISP-CLIENT::COMPUTE-LOAD-STRATEGY "reblocks/doc/example")
 3: (QUICKLISP-CLIENT::AUTOLOAD-SYSTEM-AND-DEPENDENCIES "reblocks/doc/example" :PROMPT NIL)
 4: (QUICKLISP-CLIENT::AUTOLOAD-SYSTEM-AND-DEPENDENCIES "reblocks-ui-docs" :PROMPT NIL)
 5: ((:METHOD QL-IMPL-UTIL::%CALL-WITH-QUIET-COMPILATION (T T)) #<unused argument> #<FUNCTION (FLET QUICKLISP-CLIENT::QL :IN QUICKLISP-CLIENT:$
 6: ((:METHOD QL-IMPL-UTIL::%CALL-WITH-QUIET-COMPILATION :AROUND (QL-IMPL:SBCL T)) #<QL-IMPL:SBCL {10051FD7F3}> #<FUNCTION (FLET QUICKLISP-CLI$
 7: ((:METHOD QUICKLISP-CLIENT:QUICKLOAD (T)) "reblocks-ui-docs" :PROMPT NIL :SILENT NIL :VERBOSE NIL) [fast-method]
 8: (QL-DIST::CALL-WITH-CONSISTENT-DISTS #<FUNCTION (LAMBDA NIL :IN QUICKLISP-CLIENT:QUICKLOAD) {100135CCDB}>)
 9: (SB-INT:SIMPLE-EVAL-IN-LEXENV (QUICKLISP-CLIENT:QUICKLOAD "reblocks-ui-docs") #<NULL-LEXENV>)
10: (EVAL (QUICKLISP-CLIENT:QUICKLOAD "reblocks-ui-docs"))
11: ((LAMBDA NIL :IN SLYNK-MREPL::MREPL-EVAL-1))
```

If we look at load strategy for `reblocks/doc/example`, then we'll see that quicklisp client have no idea how to load it:

```lisp
CL-USER> (time (ignore-errors (quicklisp-client::compute-load-strategy "reblocks/doc/example")))
Evaluation took:
  0.028 seconds of real time
  0.026195 seconds of total run time (0.026195 user, 0.000000 system)
  92.86% CPU
  12 forms interpreted
  52,479,390 processor cycles
  7,386,224 bytes consed

NIL
#<QUICKLISP-CLIENT:SYSTEM-NOT-FOUND {1004978233}>
```

## With my fix

But with my fix quicklisp client will attempt to find a primary system `reblocks` in the dist metadata and load strategy for `reblocks/doc/example` will look like this:

```lisp
CL-USER> (time (ignore-errors (quicklisp-client::compute-load-strategy "reblocks/doc/example")))
Evaluation took:
  0.024 seconds of real time
  0.023418 seconds of total run time (0.016144 user, 0.007274 system)
  95.83% CPU
  642 forms interpreted
  47,100,320 processor cycles
  3,574,560 bytes consed

#<QUICKLISP-CLIENT::LOAD-STRATEGY "reblocks/doc/example" (2 asdf, 103 quicklisp)>
CL-USER> (describe *)
#<QUICKLISP-CLIENT::LOAD-STRATEGY "reblocks/doc/example" (2 asdf, 103 ..
  [standard-object]

Slots with :INSTANCE allocation:
  NAME                           = "reblocks/doc/example"
  ASDF-SYSTEMS                   = (#<ASDF/SYSTEM:SYSTEM "uiop"> #<ASDF/SYSTEM:SYSTEM "asdf">)
  QUICKLISP-SYSTEMS              = (#<QL-DIST:SYSTEM yason / yason-20230214-git / #1=quicklisp #2=2023-10..
```

This way the `reblocks-ui-docs` and be quickloaded just fine as well as `reblocks/doc/example`!

## How to install

If you want to autoload the patch, do two things:

1. Clone the repository to `~/.quicklisp-client-fix/`.
2. Add this code to your `~/.sbclrc` or `~/.roswell/init.lisp`:

   ```
   (let ((fix-filename (make-pathname :directory '(:absolute :home ".quicklisp-client-fix")
                                      :name "quicklisp-fix"
                                      :type "lisp")))
     (let ((quicklisp-found #+quicklisp t
                            #-quicklisp nil))
       (cond
         ((not quicklisp-found)
          (warn "Quicklisp is not available, skipping fix loading.~%"))
         ((probe-file fix-filename)
          (handler-bind ((warning #'muffle-warning))
            (load fix-filename)))
         (t
          (warn "Quicklisp fix was not found at ~S.~%" fix-filename)))))
```
