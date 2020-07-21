ttk::frame .paned.tradeframe
set tradecanvas [canvas .paned.tradeframe.canvas -yscrollcommand [list .paned.tradeframe.scrolly set] -xscrollcommand [list .paned.tradeframe.scrollx set]]
grid $tradecanvas -sticky nwes
grid [ttk::scrollbar .paned.tradeframe.scrollx -orient horizontal -command [list $tradecanvas xview]] -row 1 -column 0 -columnspan 2 -sticky we
grid [ttk::scrollbar .paned.tradeframe.scrolly -orient vertical -command [list $tradecanvas yview]] -row 0 -column 1 -sticky ns
set tradeframe [ttk::frame $tradecanvas.trade]
# Type of items to show
grid [ttk::frame $tradeframe.options]
grid [ttk::label $tradeframe.options.typelabel -text {Type:}]
grid [ttk::combobox $tradeframe.options.type -state readonly] -column 1 -row 0
bind $tradeframe.options.type <<ComboboxSelected>> {ShowTrade [$tradeframe.options.type get]}
grid [ttk::entry $tradeframe.options.search -validate key] -column 2 -row 0
# Trade list
grid [ttk::frame $tradeframe.trade] -sticky nwes
set tradeview [ttk::treeview $tradeframe.trade.view -columns [list name type durability price profit owned available] -show headings -yscrollcommand [list $tradeframe.trade.scrolly set]]
$tradeview heading name -text {Name}
$tradeview heading type -text {Type}
$tradeview heading durability -text {Durability}
$tradeview heading price -text {Price}
$tradeview heading profit -text {Profit}
$tradeview heading owned -text {Owned}
$tradeview heading available -text {Available}
grid $tradeview -sticky nwes
bind $tradeview <<TreeviewSelect>> ShowTradeItemInfo
grid [ttk::scrollbar $tradeframe.trade.scrolly -orient vertical -command [list $tradeview yview]] -row 0 -column 1 -sticky ns
# Item info
set itemframe [ttk::frame $tradeframe.item]
grid [ttk::labelframe $itemframe.info -text {Item Info:}]
grid [text $itemframe.info.text -wrap char -height 10 -width 40]
# Item actions
grid [ttk::label $itemframe.shipmoney]
grid [ttk::label $itemframe.shipspace]
grid [ttk::label $itemframe.basemoney]
grid [ttk::frame $itemframe.buyframe]
grid [ttk::button $itemframe.buyframe.buy -text {Buy} -command {TradeItem buy}]
grid [ttk::label $itemframe.buyframe.amountlbl] -column 1 -row 0
grid [ttk::spinbox $itemframe.buyframe.amount -from 1 -validate key] -column 2 -row 0
grid [ttk::label $itemframe.buyframe.orlbl -text {or}] -column 3 -row 0
grid [ttk::button $itemframe.buyframe.buymax -text {Buy max} -command {TradeItem buymax}] -column 4 -row 0
grid [ttk::frame $itemframe.sellframe]
grid [ttk::button $itemframe.sellframe.sell -text {Sell} -command {TradeItem sell}]
grid [ttk::label $itemframe.sellframe.amountlbl] -column 1 -row 0
grid [ttk::spinbox $itemframe.sellframe.amount -from 1 -validate key] -column 2 -row 0
grid [ttk::label $itemframe.sellframe.orlbl -text {or}] -column 3 -row 0
grid [ttk::button $itemframe.sellframe.sellmax -text {Sell all} -command {TradeItem sellall}] -column 4 -row 0
grid [ttk::label $itemframe.sellframe.error -style Headerred.TLabel] -columnspan 4
grid $itemframe -row 1 -column 1 -sticky nwes
