This directory contains several different implementations of a very simple
two player game along with multiple instances of a single model that are
capable of playing against each other in that game (they only make random
moves for demonstration purposes).  Those different implementations provide
examples of different interfacing mechanisms which are available for any
task interface construction - not just multiple model games.

Details on the game and the different interface approachs used can be found
in the slides in the multiple-models-and-task-interactions.pdf file which 
is also located in this directory.

The model implementations assume that the ACT-R Environment is being used
to display the real windows and sets the :needs-mouse parameter to nil to
make the task easier to interact with as a person.  The tasks will also
run using the native GUI elements of ACL, CCL, and LispWorks, but will
require setting the :needs-mouse parameter to t in the models.  However,
care should be taken when doing so because if the mouse is moved while
the model is also using it a button press could miss the target (that
can't happen with the virtual mouse cursor used with the Environment
interface).  Similarly, when the model makes a key press with native
GUI interfaces the correct window must have the focus, so changing that
while the models are interacting is not advised.
