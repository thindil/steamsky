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

  proc itemHide(): StyleItem {.raises: [], tags: [], contractual.} =
    ## Set a style item to be hidden
    ##
    ## Returns StyleItem object with setting for hidden item
    return StyleItem(iType: itemColor, data: StyleItemData(itype: itemColor,
        color: NkColor(r: 0, g: 0, b: 0, a: 0)))

  # slider
  context.style.slider.normal = itemHide()
  context.style.slider.hover = itemHide()
  context.style.slider.active = itemHide()
  context.style.slider.barNormal = table[sliderColor]
  context.style.slider.barHover = table[sliderColor]
  context.style.slider.barActive = table[sliderColor]
  context.style.slider.barFilled = table[sliderCursorColor]
  context.style.slider.cursorNormal = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[sliderCursorColor]))
  context.style.slider.cursorHover = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[
      sliderCursorHoverColor]))
  context.style.slider.cursorActive = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[
      sliderCursorActiveColor]))
  context.style.slider.incSymbol = triangleRight
  context.style.slider.decSymbol = triangleLeft
  context.style.slider.cursorSize = Vec2(x: 16, y: 16)
  context.style.slider.padding = Vec2(x: 2, y: 2)
  context.style.slider.spacing = Vec2(x: 2, y: 2)
  context.style.slider.userData = Handle(handleType: handleInt, intValue: 0)
  context.style.slider.showButtons = false
  context.style.slider.barHeight = 8
  context.style.slider.rounding = 0
  context.style.slider.colorFactor = 1.0
  context.style.slider.disabledFactor = nkWidgetDisabledFactor
  context.style.slider.drawBegin = nil
  context.style.slider.drawEnd = nil

  # slider buttons
  context.style.slider.incButton.normal = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: NkColor(r: 40, g: 40, b: 40, a: 255)))
  context.style.slider.incButton.hover = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: NkColor(r: 42, g: 42, b: 42, a: 255)))
  context.style.slider.incButton.active = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: NkColor(r: 44, g: 44, b: 44, a: 255)))
  context.style.slider.incButton.borderColor = NkColor(r: 65, g: 65, b: 65, a: 255)
  context.style.slider.incButton.textBackground = NkColor(r: 40, g: 40, b: 40, a: 255)
  context.style.slider.incButton.textNormal = NkColor(r: 175, g: 175, b: 175, a: 255)
  context.style.slider.incButton.textHover = NkColor(r: 175, g: 175, b: 175, a: 255)
  context.style.slider.incButton.textActive = NkColor(r: 175, g: 175, b: 175, a: 255)
  context.style.slider.incButton.padding = Vec2(x: 8.0, y: 8.0)
  context.style.slider.incButton.touchPadding = Vec2(x: 0.0, y: 0.0)
  context.style.slider.incButton.userData = Handle(handleType: handleInt, intValue: 0)
  context.style.slider.incButton.alignment = centered
  context.style.slider.incButton.border = 1.0
  context.style.slider.incButton.rounding = 0.0
  context.style.slider.incButton.colorFactorText = 1.0
  context.style.slider.incButton.colorFactorBackground = 1.0
  context.style.slider.incButton.disabledFactor = nkWidgetDisabledFactor
  context.style.slider.incButton.drawBegin = nil
  context.style.slider.incButton.drawEnd = nil
  context.style.slider.decButton = context.style.slider.incButton

  # knob
  context.style.knob.normal = itemHide()
  context.style.knob.hover = itemHide()
  context.style.knob.active = itemHide()
  context.style.knob.knobNormal = table[knobColor]
  context.style.knob.knobHover = table[knobColor]
  context.style.knob.knobActive = table[knobColor]
  context.style.knob.cursorNormal = table[knobCursorColor]
  context.style.knob.cursorHover = table[knobCursorHoverColor]
  context.style.knob.cursorActive = table[knobCursorActiveColor]
  context.style.knob.knobBorderColor = table[StyleColors.borderColor]
  context.style.knob.border = 1.0
  context.style.knob.padding = Vec2(x: 2.0, y: 2.0)
  context.style.knob.spacing = Vec2(x: 2.0, y: 2.0)
  context.style.knob.cursorWidth = 2
  context.style.knob.colorFactor = 1.0
  context.style.knob.disabledFactor = nkWidgetDisabledFactor
  context.style.knob.userData = Handle(handleType: handleInt, intValue: 0)
  context.style.knob.drawBegin = nil
  context.style.knob.drawEnd = nil

  # progressbar
  context.style.progress.normal = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[
      progressbarBorderColor]))
  context.style.progress.hover = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[sliderColor]))
  context.style.progress.active = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[sliderColor]))
  context.style.progress.cursorNormal = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[progressbarColor]))
  context.style.progress.cursorHover = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[sliderCursorColor]))
  context.style.progress.cursorActive = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[sliderCursorColor]))
  context.style.progress.borderColor = NkColor(r: 0, g: 0, b: 0, a: 0)
  context.style.progress.cursorBorderColor = NkColor(r: 0, g: 0, b: 0, a: 0)
  context.style.progress.userData = Handle(handleType: handleInt, intValue: 0)
  context.style.progress.padding = Vec2(x: 4.0, y: 4.0)
  context.style.progress.rounding = 0.0
  context.style.progress.border = 0.0
  context.style.progress.cursorRounding = 0.0
  context.style.progress.cursorBorder = 0.0
  context.style.progress.colorFactor = 1.0
  context.style.progress.disabledFactor = nkWidgetDisabledFactor
  context.style.progress.drawBegin = nil
  context.style.progress.drawEnd = nil

  # scrollbars
  context.style.scrollH.normal = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[scrollbarColor]))
  context.style.scrollH.hover = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[scrollbarColor]))
  context.style.scrollH.active = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[scrollbarColor]))
  context.style.scrollH.cursorNormal = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[scrollbarCursorColor]))
  context.style.scrollH.cursorHover = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[
      scrollbarCursorHoverColor]))
  context.style.scrollH.cursorActive = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[
      scrollbarCursorActiveColor]))
  context.style.scrollH.decSymbol = circleSolid
  context.style.scrollH.incSymbol = circleSolid
  context.style.scrollH.userData = Handle(handleType: handleInt, intValue: 0)
  context.style.scrollH.borderColor = table[scrollbarColor]
  context.style.scrollH.cursorBorderColor = table[scrollbarColor]
  context.style.scrollH.padding = Vec2(x: 0.0, y: 0.0)
  context.style.scrollH.showButtons = false
  context.style.scrollH.border = 0.0
  context.style.scrollH.rounding = 0.0
  context.style.scrollH.borderCursor = 0.0
  context.style.scrollH.roundingCursor = 0.0
  context.style.scrollH.colorFactor = 1.0
  context.style.scrollH.disabledFactor = nkWidgetDisabledFactor
  context.style.scrollH.drawBegin = nil
  context.style.scrollH.drawEnd = nil
  context.style.scrollV = context.style.scrollH

  # scrollbars buttons
  context.style.scrollH.incButton.normal = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: NkColor(r: 40, g: 40, b: 40, a: 255)))
  context.style.scrollH.incButton.hover = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: NkColor(r: 42, g: 42, b: 42, a: 255)))
  context.style.scrollH.incButton.active = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: NkColor(r: 44, g: 44, b: 44, a: 255)))
  context.style.scrollH.incButton.borderColor = NkColor(r: 65, g: 65, b: 65, a: 255)
  context.style.scrollH.incButton.textBackground = NkColor(r: 40, g: 40, b: 40, a: 255)
  context.style.scrollH.incButton.textNormal = NkColor(r: 175, g: 175, b: 175, a: 255)
  context.style.scrollH.incButton.textHover = NkColor(r: 175, g: 175, b: 175, a: 255)
  context.style.scrollH.incButton.textActive = NkColor(r: 175, g: 175, b: 175, a: 255)
  context.style.scrollH.incButton.padding = Vec2(x: 4.0, y: 4.0)
  context.style.scrollH.incButton.touchPadding = Vec2(x: 0.0, y: 0.0)
  context.style.scrollH.incButton.userData = Handle(handleType: handleInt, intValue: 0)
  context.style.scrollH.incButton.alignment = centered
  context.style.scrollH.incButton.border = 1.0
  context.style.scrollH.incButton.rounding = 0.0
  context.style.scrollH.incButton.colorFactorText = 1.0
  context.style.scrollH.incButton.colorFactorBackground = 1.0
  context.style.scrollH.incButton.disabledFactor = nkWidgetDisabledFactor
  context.style.scrollH.incButton.drawBegin = nil
  context.style.scrollH.incButton.drawEnd = nil
  context.style.scrollH.decButton = context.style.scrollH.incButton
  context.style.scrollV.incButton = context.style.scrollH.incButton
  context.style.scrollV.decButton = context.style.scrollH.incButton

  # edit
  context.style.edit.normal = StyleItem(iType: itemColor, data: StyleItemData(
      itype: itemColor, color: table[editColor]))
  context.style.edit.hover = StyleItem(iType: itemColor, data: StyleItemData(
      itype: itemColor, color: table[editColor]))
  context.style.edit.active = StyleItem(iType: itemColor, data: StyleItemData(
      itype: itemColor, color: table[editColor]))
  context.style.edit.cursorNormal = table[editCursorColor]
  context.style.edit.cursorHover = table[editCursorColor]
  context.style.edit.cursorTextNormal = table[editColor]
  context.style.edit.cursorTextHover = table[editColor]
  context.style.edit.borderColor = table[StyleColors.borderColor]
  context.style.edit.textNormal = table[editTextColor]
  context.style.edit.textHover = table[editTextColor]
  context.style.edit.textActive = table[editTextColor]
  context.style.edit.selectedNormal = table[editTextColor]
  context.style.edit.selectedHover = table[editTextColor]
  context.style.edit.selectedTextNormal = table[editColor]
  context.style.edit.selectedTextHover = table[editColor]
  context.style.edit.scrollbarSize = Vec2(x: 10, y: 10)
  context.style.edit.scrollbar = context.style.scrollH
  context.style.edit.padding = Vec2(x: 4, y: 4)
  context.style.edit.rowPadding = 2
  context.style.edit.cursorSize = 4
  context.style.edit.border = 1
  context.style.edit.rounding = 0
  context.style.edit.colorFactor = 1.0
  context.style.edit.disabledFactor = nkWidgetDisabledFactor

  # property
  context.style.property.normal = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[propertyColor]))
  context.style.property.hover = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[propertyColor]))
  context.style.property.active = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[propertyColor]))
  context.style.property.borderColor = table[StyleColors.borderColor]
  context.style.property.labelNormal = table[StyleColors.textColor]
  context.style.property.labelHover = table[StyleColors.textColor]
  context.style.property.labelActive = table[StyleColors.textColor]
  context.style.property.symLeft = triangleLeft
  context.style.property.symRight = triangleRight
  context.style.property.userData = Handle(handleType: handleInt, intValue: 0)
  context.style.property.padding = Vec2(x: 4, y: 4)
  context.style.property.border = 1
  context.style.property.rounding = 10
  context.style.property.drawBegin = nil
  context.style.property.drawEnd = nil
  context.style.property.colorFactor = 1.0
  context.style.property.disabledFactor = nkWidgetDisabledFactor

  # property buttons
  context.style.property.decButton.normal = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[propertyColor]))
  context.style.property.decButton.hover = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[propertyColor]))
  context.style.property.decButton.active = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[propertyColor]))
  context.style.property.decButton.borderColor = NkColor(r: 0, g: 0, b: 0, a: 0)
  context.style.property.decButton.textBackground = table[propertyColor]
  context.style.property.decButton.textNormal = table[textColor]
  context.style.property.decButton.textHover = table[textColor]
  context.style.property.decButton.textActive = table[textColor]
  context.style.property.decButton.padding = Vec2(x: 0.0, y: 0.0)
  context.style.property.decButton.touchPadding = Vec2(x: 0.0, y: 0.0)
  context.style.property.decButton.userData = Handle(handleType: handleInt, intValue: 0)
  context.style.property.decButton.alignment = centered
  context.style.property.decButton.border = 0.0
  context.style.property.decButton.rounding = 0.0
  context.style.property.decButton.colorFactorText = 1.0
  context.style.property.decButton.colorFactorBackground = 1.0
  context.style.property.decButton.disabledFactor = nkWidgetDisabledFactor
  context.style.property.decButton.drawBegin = nil
  context.style.property.decButton.drawEnd = nil
  context.style.property.incButton = context.style.property.decButton

  # property edit
  context.style.property.edit.normal = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[propertyColor]))
  context.style.property.edit.hover = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[propertyColor]))
  context.style.property.edit.active = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[propertyColor]))
  context.style.property.edit.borderColor = NkColor(r: 0, g: 0, b: 0, a: 0)
  context.style.property.edit.cursorNormal = table[propertyTextColor]
  context.style.property.edit.cursorHover = table[propertyTextColor]
  context.style.property.edit.cursorTextNormal = table[editColor]
  context.style.property.edit.cursorTextHover = table[editColor]
  context.style.property.edit.textNormal = table[propertyTextColor]
  context.style.property.edit.textHover = table[propertyTextColor]
  context.style.property.edit.textActive = table[propertyTextColor]
  context.style.property.edit.selectedNormal = table[propertyTextColor]
  context.style.property.edit.selectedHover = table[propertyTextColor]
  context.style.property.edit.selectedTextNormal = table[editColor]
  context.style.property.edit.selectedTextHover = table[editColor]
  context.style.property.edit.padding = Vec2(x: 0.0, y: 0.0)
  context.style.property.edit.cursorSize = 8
  context.style.property.edit.border = 0
  context.style.property.edit.rounding = 0
  context.style.property.edit.colorFactor = 1.0
  context.style.property.edit.disabledFactor = nkWidgetDisabledFactor

  # chart
  context.style.chart.background = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[chartColor]))
  context.style.chart.borderColor = table[StyleColors.borderColor]
  context.style.chart.selectedColor = table[colorChartHighlightColor]
  context.style.chart.color = table[colorChartColor]
  context.style.chart.padding = Vec2(x: 4.0, y: 4.0)
  context.style.chart.border = 0
  context.style.chart.rounding = 0
  context.style.chart.colorFactor = 1.0
  context.style.chart.disabledFactor = nkWidgetDisabledFactor
  context.style.chart.showMarkers = true

  # combo
  context.style.combo.normal = StyleItem(iType: itemColor, data: StyleItemData(
      itype: itemColor, color: table[comboColor]))
  context.style.combo.hover = StyleItem(iType: itemColor, data: StyleItemData(
      itype: itemColor, color: table[comboColor]))
  context.style.combo.active = StyleItem(iType: itemColor, data: StyleItemData(
      itype: itemColor, color: table[comboColor]))
  context.style.combo.borderColor = table[StyleColors.borderColor]
  context.style.combo.labelNormal = table[comboTextColor]
  context.style.combo.labelHover = table[comboTextColor]
  context.style.combo.labelActive = table[comboTextColor]
  context.style.combo.symNormal = triangleDown
  context.style.combo.symHover = triangleDown
  context.style.combo.symActive = triangleDown
  context.style.combo.contentPadding = Vec2(x: 4.0, y: 4.0)
  context.style.combo.buttonPadding = Vec2(x: 0.0, y: 4.0)
  context.style.combo.spacing = Vec2(x: 4.0, y: 0.0)
  context.style.combo.border = 1
  context.style.combo.rounding = 0
  context.style.combo.colorFactor = 1.0
  context.style.combo.disabledFactor = nkWidgetDisabledFactor

  # combo button
  context.style.combo.button.normal = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[comboColor]))
  context.style.combo.button.hover = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[comboColor]))
  context.style.combo.button.active = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[comboColor]))
  context.style.combo.button.borderColor = NkColor(r: 0, g: 0, b: 0, a: 0)
  context.style.combo.button.textBackground = table[comboColor]
  context.style.combo.button.textNormal = table[textColor]
  context.style.combo.button.textHover = table[textColor]
  context.style.combo.button.textActive = table[textColor]
  context.style.combo.button.padding = Vec2(x: 2.0, y: 2.0)
  context.style.combo.button.touchPadding = Vec2(x: 0.0, y: 0.0)
  context.style.combo.button.userData = Handle(handleType: handleInt, intValue: 0)
  context.style.combo.button.alignment = centered
  context.style.combo.button.border = 0.0
  context.style.combo.button.rounding = 0.0
  context.style.combo.button.colorFactorText = 1.0
  context.style.combo.button.colorFactorBackground = 1.0
  context.style.combo.button.disabledFactor = nkWidgetDisabledFactor
  context.style.combo.button.drawBegin = nil
  context.style.combo.button.drawEnd = nil

  # tab
  context.style.tab.background = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[tabHeaderColor]))
  context.style.tab.borderColor = table[StyleColors.borderColor]
  context.style.tab.text = table[textColor]
  context.style.tab.symMinimize = triangleRight
  context.style.tab.symMaximize = triangleDown
  context.style.tab.padding = Vec2(x: 4.0, y: 4.0)
  context.style.tab.spacing = Vec2(x: 4.0, y: 4.0)
  context.style.tab.indent = 10.0
  context.style.tab.border = 1
  context.style.tab.rounding = 0
  context.style.tab.colorFactor = 1.0
  context.style.tab.disabledFactor = nkWidgetDisabledFactor

  # tab button
  context.style.tab.tabMinimizeButton.normal = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[tabHeaderColor]))
  context.style.tab.tabMinimizeButton.hover = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[tabHeaderColor]))
  context.style.tab.tabMinimizeButton.active = StyleItem(iType: itemColor,
      data: StyleItemData(itype: itemColor, color: table[tabHeaderColor]))
  context.style.tab.tabMinimizeButton.borderColor = NkColor(r: 0, g: 0, b: 0,
      a: 0)
  context.style.tab.tabMinimizeButton.textBackground = table[tabHeaderColor]
  context.style.tab.tabMinimizeButton.textNormal = table[textColor]
  context.style.tab.tabMinimizeButton.textHover = table[textColor]
  context.style.tab.tabMinimizeButton.textActive = table[textColor]
  context.style.tab.tabMinimizeButton.padding = Vec2(x: 2.0, y: 2.0)
  context.style.tab.tabMinimizeButton.touchPadding = Vec2(x: 0.0, y: 0.0)
  context.style.tab.tabMinimizeButton.userData = Handle(handleType: handleInt,
      intValue: 0)

proc defaultStyle*() {.raises: [], tags: [], contractual.} =
  ## Reset the UI colors to the default Nuklear setting
  proc nk_style_default(ctx) {.importc, nodecl, raises: [], tags: [], contractual.}
    ## A binding to Nuklear's function. Internal use only
  nk_style_default(ctx = ctx)

