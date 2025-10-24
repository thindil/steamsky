# Copyright 2025 Bartek thindil Jasicki
#
# This file is part of Steam Sky.
#
# Steam Sky is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Steam Sky is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Steam Sky.  If not, see <http://www.gnu.org/licenses/>.

import contracts, nuklear/nuklear_sdl_renderer
import ../config
import coreui, themes

proc addPagination*(page: var Positive; row: Positive) {.raises: [], tags: [],
    contractual.} =
  ## Add the buttons previous and next to a table
  ##
  ## * page - the current page in the table
  ## * row  - the number of the last row in the table
  ##
  ## Returns modified parameter page
  var cols: Natural = 0
  if page > 1:
    if row < gameSettings.listsLimit + 1:
      cols = 1
    else:
      cols = 2
  elif row == gameSettings.listsLimit + 1:
    cols = 1
  if cols > 0:
    setLayoutRowDynamic(height = 30, cols = cols)
    if page > 1:
      if row < gameSettings.listsLimit + 1:
        if gameSettings.showTooltips:
          addTooltip(bounds = getWidgetBounds(), text = "Previous page")
        labelButton(title = "Previous"):
          page.dec
      else:
        if gameSettings.showTooltips:
          addTooltip(bounds = getWidgetBounds(), text = "Previous page")
        labelButton(title = "Previous"):
          page.dec
        if gameSettings.showTooltips:
          addTooltip(bounds = getWidgetBounds(), text = "Next page")
        labelButton(title = "Next"):
          page.inc
    elif row == gameSettings.listsLimit + 1:
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(), text = "Next page")
      labelButton(title = "Next"):
        page.inc

type
  HeaderCode*[T] = proc (sortAsc, sortDesc: T;
      dialog: var GameDialog) {.raises: [], contractual.}
    ## Code executed when the table's header was pressed
  HeaderData*[T] = object
    ## Used to store data about the table's header
    ##
    ## * label    - the label to show on the header
    ## * sortAsc  - the sorting ascending value when the player's click on the
    ##              header
    ## * sortDesc - the sorting descending value when the player's click on the
    ##              header
    label*: string
    sortAsc*: T
    sortDesc*: T

proc addHeader*(headers: openArray[HeaderData]; ratio: openArray[cfloat];
    tooltip: string; code: HeaderCode; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Add the header to the table
  ##
  ## * headers    - the list of headers to add
  ## * ratio      - the list of width for each column in the table
  ## * tooltip    - the name of things to sort, like items, etc. Will be added to
  ##                the headers' tooltips. If empty, disables tooltips for the
  ##                header.
  ## * headerCode - the code executed when a header was clicked
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  setLayoutRowStatic(height = 30, cols = headers.len, ratio = ratio)
  for header in headers:
    if gameSettings.showTooltips and tooltip.len > 0:
      addTooltip(bounds = getWidgetBounds(),
          text = "Press mouse button to sort the " & tooltip & ".")
    labelButton(title = header.label):
      code(sortAsc = header.sortAsc, sortDesc = header.sortDesc,
          dialog = dialog)

type
  ButtonCode* = proc(data: int; dialog: var GameDialog) {.raises: [], contractual.}
    ## Code executed when the button was pressed

proc addButton*(label, tooltip: string; data: int; code: ButtonCode;
    dialog: var GameDialog; color: ColorsNames = tableTextColor) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Add a button to the table
  ##
  ## * label   - the text to show on the button
  ## * tooltip - the text to show as a tooltip for the button
  ## * data    - the data passed to the code executed after clikcking the
  ##             button
  ## * code    - the code executed when the button was clicked
  ## * dialog  - the current in-game dialog displayed on the screen
  ## * color   - the color of the text on the button. Can be empty, use then
  ##             the default color for table buttons.
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(), text = tooltip)
  if color != tableTextColor:
    setButtonStyle(field = textNormal, color = theme.colors[color])
  labelButton(title = label):
    code(data = data, dialog = dialog)
  if color != tableTextColor:
    setButtonStyle(field = textNormal, color = theme.colors[tableTextColor])

proc addProgressBar*(tooltip: string; value, maxValue, data: int;
    code: ButtonCode; dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Add a progress bar to the table
  ##
  ## * tooltip  - the text to show as a tooltip for the progess bar
  ## * value    - the current value of the progress bar
  ## * maxValue - the maximum value of the progress bar
  ## * data     - the data passed to the code executed after clikcking the
  ##              button
  ## * code     - the code executed when the progress bar was clicked
  ## * dialog   - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  let bounds: NimRect = getWidgetBounds()
  if gameSettings.showTooltips:
    addTooltip(bounds = bounds, text = tooltip)
  var
    val: int = value
    color: ColorsNames = greenColor
  let percent: float = val.float / maxValue.float
  if percent in 0.26..0.74:
    color = yellowColor
  elif percent < 0.26:
    color = redColor
  changeStyle(field = progressbar, color = theme.colors[color]):
    progressBar(value = val, maxValue = maxValue, modifyable = false)
  if mouseClicked(id = (if gameSettings.rightButton: Buttons.right else: left),
      rect = bounds):
    code(data = data, dialog = dialog)

proc addCheckButton*(tooltip: string; checked: var bool) {.raises: [], tags: [], contractual.} =
  ## Add a check button to the table
  ##
  ## * tooltip  - the text to show as a tooltip for the check button
  ## * checked  - if true, the button is checked, otherwise false
  ##
  ## Returns modified parameter checked. It is modified when the player check
  ## or uncheck the button.
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(), text = tooltip)
  checkbox(label = "", checked = checked)
