import windowed as window

# a layer of indirection is required to treat windowed.py as a module.
# as JsPyBridge imports local files by evaling them
# and we want to share one window between the buttons and the canvas

buttons = window.buttons
run = window.run
isDone = window.isDone