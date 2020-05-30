wm title . {Steam Sky - Error info}
wm geometry . 800x600+[expr ([winfo vrootwidth .] - 800) / 2]+[expr ([winfo vrootheight .] - 600) / 2]
grid [ttk::label .generalinfo -text {Oops, something bad happens and the game has crashed. Game should save your progress, but better verify this yourself. Also, please, remember what you were doing before the crash and report this problem at} -wraplength 790] -columnspan 2
grid [ttk::button .reportlink -text {https://github.com/thindil/steamsky/issues} -style Toolbutton] -columnspan 2
grid [ttk::label .generalinfo2 -text {or if you prefer, on one of the game community options:}] -columnspan 2
grid [ttk::button .reportlink2 -text {https://thindil.itch.io/steam-sky}] -columnspan 2
grid [ttk::label .generalinfo3 -text {and attach (if possible) file 'error.log'}] -columnspan 2
