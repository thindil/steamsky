# Copyright © 2026 Bartek Jasicki
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met*:
# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
# 3. Neither the name of the copyright holder nor the
# names of its contributors may be used to endorse or promote products
# derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY COPYRIGHT HOLDERS AND CONTRIBUTORS ''AS IS'' AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES *(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT *(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

## Provides code related to Nuklear styles

import contracts
import nk_context, nk_types

# ---------------------
# Procedures parameters
# ---------------------
using
  ctx: PContext

# ------------------
# High level bindings
# ------------------
const defaultColorStyle: array[StyleColors, NkColor] = [
  textColor: NkColor(r: 175, g: 175, b: 175, a: 255), windowColor: NkColor(
    r: 45, g: 45, b: 45, a: 255), headerColor: NkColor(r: 40, g: 40, b: 40,
    a: 255), borderColor: NkColor(r: 65, g: 65, b: 65, a: 255),
    buttonColor: NkColor(r: 65, g: 65, b: 65, a: 255),
    buttonHoverColor: NkColor(r: 40, g: 40, b: 40, a: 255),
    buttonActiveColor: NkColor(r: 35, g: 35, b: 35, a: 255),
    toggleColor: NkColor(r: 100, g: 100, b: 100, a: 255),
    toggleHoverColor: NkColor(r: 120, g: 120, b: 120, a: 255),
    toggleCursorColor: NkColor(r: 45, g: 45, b: 45, a: 255),
    selectColor: NkColor(r: 45, g: 45, b: 45, a: 255),
    selectActiveColor: NkColor(r: 35, g: 35, b: 35, a: 255),
    sliderColor: NkColor(r: 38, g: 38, b: 38, a: 255),
    sliderCursorColor: NkColor(r: 100, g: 100, b: 100, a: 255),
    sliderCursorHoverColor: NkColor(r: 120, g: 120, b: 120, a: 255),
    sliderCursorActiveColor: NkColor(r: 150, g: 150, b: 150, a: 255),
    propertyColor: NkColor(r: 38, g: 38, b: 38, a: 255), editColor: NkColor(
    r: 38, g: 38, b: 38, a: 255), editCursorColor: NkColor(r: 175, g: 175,
    b: 175, a: 255), comboColor: NkColor(r: 45, g: 45, b: 45, a: 255),
    chartColor: NkColor(r: 120, g: 120, b: 120, a: 255),
    colorChartColor: NkColor(r: 45, g: 45, b: 45, a: 255),
    colorChartHighlightColor: NkColor(r: 255, g: 0, b: 0, a: 255),
    scrollbarColor: NkColor(r: 40, g: 40, b: 40, a: 255),
    scrollbarCursorColor: NkColor(r: 100, g: 100, b: 100, a: 255),
    scrollbarCursorHoverColor: NkColor(r: 120, g: 120, b: 120, a: 255),
    scrollbarCursorActiveColor: NkColor(r: 150, g: 150, b: 150, a: 255),
    tabHeaderColor: NkColor(r: 40, g: 40, b: 40, a: 255), knobColor: NkColor(
    r: 38, g: 38, b: 38, a: 255), knobCursorColor: NkColor(r: 100, g: 100,
    b: 100, a: 255), knobCursorHoverColor: NkColor(r: 120, g: 120, b: 120,
    a: 255), knobCursorActiveColor: NkColor(r: 150, g: 150, b: 150, a: 255),
    buttonTextColor: NkColor(r: 175, g: 175, b: 175, a: 255),
    buttonHoverTextColor: NkColor(r: 175, g: 175, b: 175, a: 255),
    buttonActiveTextColor: NkColor(r: 175, g: 175, b: 175, a: 255),
    editTextColor: NkColor(r: 175, g: 175, b: 175, a: 255),
    comboTextColor: NkColor(r: 175, g: 175, b: 175, a: 255),
    tooltipColor: NkColor(r: 45, g: 45, b: 45, a: 255),
    tooltipBorderColor: NkColor(r: 65, g: 65, b: 65, a: 255),
    groupBorderColor: NkColor(r: 65, g: 65, b: 65, a: 255),
    headerTextColor: NkColor(r: 175, g: 175, b: 175, a: 255),
    groupTextColor: NkColor(r: 175, g: 175, b: 175, a: 255),
    selectActiveTextColor: NkColor(r: 175, g: 175, b: 175, a: 255),
    propertyTextColor: NkColor(r: 175, g: 175, b: 175, a: 255),
    popupColor: NkColor(r: 45, g: 45, b: 45, a: 255), popupBorderColor: NkColor(
    r: 65, g: 65, b: 65, a: 255), progressbarColor: NkColor(r: 100, g: 100,
    b: 100, a: 255), progressbarBorderColor: NkColor(r: 38, g: 38, b: 38, a: 255)]

proc nkStyleFromTable*(table: array[StyleColors,
    NkColor] = defaultColorStyle) {.raises: [], tags: [], contractual.} =
  ## Set the Nuklear style colors from the table
  ##
  ## * table - the colors table which will be set
  # default text
  context.style.text.color = table[textColor]
  context.style.text.padding = Vec2(x: 0, y: 0)
  context.style.text.colorFactor = 1.0
  context.style.text.disabledFactor = nkWidgetDisabledFactor

  # default button
  context.style.button.normal = StyleItem(iType: itemColor, data: StyleItemData(
      itype: itemColor, color: table[buttonColor]))
  context.style.button.hover = StyleItem(iType: itemColor, data: StyleItemData(
      itype: itemColor, color: table[buttonHoverColor]))
  context.style.button.active = StyleItem(iType: itemColor, data: StyleItemData(
      itype: itemColor, color: table[buttonActiveColor]))
  context.style.button.borderColor = table[StyleColors.borderColor]
  context.style.button.textBackground = table[buttonColor]
  context.style.button.textNormal = table[buttonTextColor]
  context.style.button.textHover = table[buttonHoverTextColor]
  context.style.button.textActive = table[buttonActiveTextColor]
  context.style.button.padding = Vec2(x: 2.0, y: 2.0)
  context.style.button.imagePadding = Vec2(x: 0.0, y: 0.0)
  context.style.button.touchPadding = Vec2(x: 0.0, y: 0.0)
  context.style.button.userData = Handle(handleType: handleInt, intValue: 0)
  context.style.button.alignment = centered
  context.style.button.border = 1.0
  context.style.button.rounding = 4.0
  context.style.button.colorFactorText = 1.0
  context.style.button.colorFactorBackground = 1.0
  context.style.button.disabledFactor = nkWidgetDisabledFactor
  context.style.button.drawBegin = nil
  context.style.button.drawEnd = nil

  # contextual button
  context.style.contextualButton.normal = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[windowColor]))
  context.style.contextualButton.hover = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[buttonHoverColor]))
  context.style.contextualButton.active = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[buttonActiveColor]))
  context.style.contextualButton.borderColor = table[windowColor]
  context.style.contextualButton.textBackground = table[windowColor]
  context.style.contextualButton.textNormal = table[buttonTextColor]
  context.style.contextualButton.textHover = table[buttonHoverTextColor]
  context.style.contextualButton.textActive = table[buttonActiveTextColor]
  context.style.contextualButton.border = 0.0
  context.style.contextualButton.rounding = 0.0
  context.style.contextualButton.colorFactorText = 1.0
  context.style.contextualButton.colorFactorBackground = 1.0
  context.style.contextualButton.disabledFactor = nkWidgetDisabledFactor
  context.style.contextualButton.drawBegin = nil
  context.style.contextualButton.drawEnd = nil

  # menu button
  context.style.menuButton.normal = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[windowColor]))
  context.style.menuButton.hover = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[windowColor]))
  context.style.menuButton.active = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[windowColor]))
  context.style.menuButton.borderColor = table[windowColor]
  context.style.menuButton.textBackground = table[windowColor]
  context.style.menuButton.textNormal = table[textColor]
  context.style.menuButton.textHover = table[textColor]
  context.style.menuButton.textActive = table[textColor]
  context.style.menuButton.padding = Vec2(x: 2.0, y: 2.0)
  context.style.menuButton.touchPadding = Vec2(x: 0.0, y: 0.0)
  context.style.menuButton.userData = Handle(handleType: handleInt, intValue: 0)
  context.style.menuButton.alignment = centered
  context.style.menuButton.border = 0.0
  context.style.menuButton.rounding = 1.0
  context.style.menuButton.colorFactorText = 1.0
  context.style.menuButton.colorFactorBackground = 1.0
  context.style.menuButton.disabledFactor = nkWidgetDisabledFactor
  context.style.menuButton.drawBegin = nil
  context.style.menuButton.drawEnd = nil

  # checkbox toggle
  context.style.checkbox.normal = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[toggleColor]))
  context.style.checkbox.hover = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[toggleHoverColor]))
  context.style.checkbox.active = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[toggleHoverColor]))
  context.style.checkbox.cursorNormal = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[toggleCursorColor]))
  context.style.checkbox.cursorHover = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[toggleCursorColor]))
  context.style.checkbox.userData = Handle(handleType: handleInt, intValue: 0)
  context.style.checkbox.textBackground = table[windowColor]
  context.style.checkbox.textNormal = table[textColor]
  context.style.checkbox.textHover = table[textColor]
  context.style.checkbox.textActive = table[textColor]
  context.style.checkbox.padding = Vec2(x: 2.0, y: 2.0)
  context.style.checkbox.touchPadding = Vec2(x: 0.0, y: 0.0)
  context.style.checkbox.borderColor = NkColor(r: 0, g: 0, b: 0, a: 0)
  context.style.checkbox.border = 0.0
  context.style.checkbox.spacing = 4
  context.style.checkbox.colorFactor = 1.0
  context.style.checkbox.disabledFactor = nkWidgetDisabledFactor

  # options toggle
  context.style.option.normal = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[toggleColor]))
  context.style.option.hover = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[toggleHoverColor]))
  context.style.option.active = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[toggleHoverColor]))
  context.style.option.cursorNormal = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[toggleCursorColor]))
  context.style.option.cursorHover = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[toggleCursorColor]))
  context.style.option.userData = Handle(handleType: handleInt, intValue: 0)
  context.style.option.textBackground = table[windowColor]
  context.style.option.textNormal = table[textColor]
  context.style.option.textHover = table[textColor]
  context.style.option.textActive = table[textColor]
  context.style.option.padding = Vec2(x: 3.0, y: 3.0)
  context.style.option.touchPadding = Vec2(x: 0.0, y: 0.0)
  context.style.option.borderColor = NkColor(r: 0, g: 0, b: 0, a: 0)
  context.style.option.border = 0.0
  context.style.option.spacing = 4
  context.style.option.colorFactor = 1.0
  context.style.option.disabledFactor = nkWidgetDisabledFactor

  # selectable
  context.style.selectable.normal = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[selectColor]))
  context.style.selectable.hover = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[selectColor]))
  context.style.selectable.pressed = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[selectColor]))
  context.style.selectable.normalActive = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[selectActiveColor]))
  context.style.selectable.hoverActive = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[selectActiveColor]))
  context.style.selectable.pressedActive = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[selectActiveColor]))
  context.style.selectable.textNormal = table[textColor]
  context.style.selectable.textHover = table[textColor]
  context.style.selectable.textPressed = table[textColor]
  context.style.selectable.textNormalActive = table[selectActiveTextColor]
  context.style.selectable.textHoverActive = table[selectActiveTextColor]
  context.style.selectable.textPressedActive = table[selectActiveTextColor]
  context.style.selectable.padding = Vec2(x: 2.0, y: 2.0)
  context.style.selectable.imagePadding = Vec2(x: 2.0, y: 2.0)
  context.style.selectable.touchPadding = Vec2(x: 0.0, y: 0.0)
  context.style.selectable.userData = Handle(handleType: handleInt, intValue: 0)
  context.style.selectable.rounding = 0.0
  context.style.selectable.colorFactor = 1.0
  context.style.selectable.disabledFactor = nkWidgetDisabledFactor
  context.style.selectable.drawBegin = nil
  context.style.selectable.drawEnd = nil

  # slider
  context.style.slider.normal = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: NkColor(r: 0, g: 0, b: 0, a: 0)))
  context.style.slider.hover = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: NkColor(r: 0, g: 0, b: 0, a: 0)))
  context.style.slider.active = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: NkColor(r: 0, g: 0, b: 0, a: 0)))
  context.style.slider.barNormal = table[sliderColor]
  context.style.slider.barHover = table[sliderColor]
  context.style.slider.barActive = table[sliderColor]
  context.style.slider.barFilled = table[sliderCursorColor]
  context.style.slider.cursorNormal = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[sliderCursorColor]))
  context.style.slider.cursorHover = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[sliderCursorHoverColor]))
  context.style.slider.cursorActive = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[sliderCursorActiveColor]))
  context.style.slider.incSymbol = triangleRight
  context.style.slider.decSymbol = triangleLeft

proc defaultStyle*() {.raises: [], tags: [], contractual.} =
  ## Reset the UI colors to the default Nuklear setting
  proc nk_style_default(ctx) {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  nk_style_default(ctx = ctx)

