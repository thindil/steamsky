ttk::frame .mainmenu
pack [ttk::label .mainmenu.logo -text {Steam Sky} -font {Rye 70}]
pack [ttk::label .mainmenu.version -text {Version 5.1 development}]
pack [ttk::button .mainmenu.newgame -text {New game}]
pack [ttk::button .mainmenu.loadgame -text {Load game}]
pack [ttk::button .mainmenu.halloffame -text {Hall of Fame}]
pack [ttk::button .mainmenu.news -text {News}]
pack [ttk::button .mainmenu.about -text {About}]
pack [ttk::button .mainmenu.quit -text {Quit} -command exit]
