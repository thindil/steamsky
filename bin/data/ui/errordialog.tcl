wm title . {Steam Sky - Error info}
wm geometry . 800x600+[expr ([winfo vrootwidth .] - 800) / 2]+[expr ([winfo vrootheight .] - 600) / 2]
pack [ttk::label .generalinfo -text {Oops, something bad happens and the game has crashed. Game should save your progress, but better verify this yourself. Also, please, remember what you were doing before the crash and report this problem at} -wraplength 790]
pack [ttk::button .reportlink -text {https://github.com/thindil/steamsky/issues} -command {OpenLink https://github.com/thindil/steamsky/issues} -style Toolbutton]
pack [ttk::label .generalinfo2 -text {or if you prefer, on one of the game community options:}]
pack [ttk::button .reportlink2 -text {https://thindil.itch.io/steam-sky} -command {OpenLink https://thindil.itch.io/steam-sky} -style Toolbutton]
pack [ttk::label .generalinfo3 -text {and attach (if possible) file 'error.log'}]
pack [ttk::button .close -text {Close} -command {exit}]
ttk::labelframe .technical -text {Technical information:}
pack [ttk::scrollbar .technical.scroll -orient vertical -command [list .technical.text yview]] -side right -fill y
pack [text .technical.text -wrap word -yscrollcommand [list .technical.scroll set]] -fill both -expand true -side top
pack .technical -fill both -expand true
