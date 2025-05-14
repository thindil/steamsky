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
import coreui

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
  HeaderData*[T] = object
    label*: string
    sortAsc*: T
    sortDesc*: T
    ratio*: cfloat

proc addHeader*(headers: openArray[HeaderData]; tooltip: string;
    code: HeaderCode; dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Add the header to the table
  ##
  ## * headers    - the list of headers to add
  ## * tooltip    - the name of things to sort, like items, etc. Will be added to
  ##                the headers' tooltips
  ## * headerCode - the code executed when a header was clicked
  var ratio: seq[cfloat]
  for header in headers:
    ratio.add(y = header.ratio)
  setLayoutRowStatic(height = 30, cols = headers.len, ratio = ratio)
  for header in headers:
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Press mouse button to sort the " & tooltip & ".")
    labelButton(title = header.label):
      code(sortAsc = header.sortAsc, sortDesc = header.sortDesc,
          dialog = dialog)
