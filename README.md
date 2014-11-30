hipster-simulator
=================

A cellular automaton that emulates the clothing choices of the anticonformist.

### How to
Make a new town by stashing it in a variable somewhere:

    (defparameter *your-town* (make-town 20)) ; Makes a town with population 20

Then you can evolve your town by using `(tick! *your-town*)`.

Optionally you can opt to use the macro `with-town-iterator`
to automatically set up a loop that runs for each generation of the simulation:

    (with-town-iterator (town-var town-object :times 30 ; Run the simulation for 30 ticks
                                              :tick-time :after ; Tick after the code has run
                                              :copyp t ; Tick on a copy of the object, so as to run nondestructively
                                              )
       ;; write code here to run for every generation of the simulation
       )
