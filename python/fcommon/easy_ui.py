import appJar
import os


class EasyUI:
    gui = None

    def __init__(self, title):
        self.title = title
        self.widget_bg = None
        self.widget_fg = None

        self.set_map = {
            "*bg*": lambda name, value: self.gui.setBg(value, True),
            "*fg*": lambda name, value: self.gui.setFg(value, True),
            "*widget_bg*": lambda name, value: self.set_widget_bg(value),
            "*widget_fg*": lambda name, value: self.set_widget_fg(value),
            "*padding*": lambda name, value: self.gui.setGuiPadding(
                value, value
            ),
            "*expand*": lambda name, value: self.gui.setExpand(value),
            "*top*": lambda name, value: self.gui.setOnTop(value),
            "*fullscreen*": lambda name, value: self.gui.setSize("fullscreen")
            if value
            else self.gui.exitFullscreen(),
            "*size*": lambda name, value: self.gui.setSize(value[0], value[1]),
            "*icon*": lambda name, value: self.gui.setIcon(value),
            "*title*": lambda name, value: self.gui.setTitle(value),
        }
        self.get_map = {
            "*clipboard*": lambda name: self.gui.topLevel.clipboard_get()
        }

        if EasyUI.gui is not None:
            self.is_sub = True
            return

        self.is_sub = False
        EasyUI.gui = appJar.gui(title)

    def set_widget_bg(self, bg):
        self.widget_bg = bg

    def set_widget_fg(self, fg):
        self.widget_fg = fg

    def setup_status_bar(self, fields=1):
        self.gui.addStatusbar(fields=fields)
        self.gui.setStatusbarBg(self.widget_bg)
        self.gui.setStatusbarFg(self.widget_fg)

    def show_status(self, text, field=0):
        if text is None:
            self.gui.clearStatusbar(field=field)
        else:
            self.gui.setStatusbar(text, field=field)

        self.gui.topLevel.update()

    def __setitem__(self, attr, value):
        if attr in self.set_map:
            self.set_map[attr](attr, value)

    def __getitem__(self, attr):
        if attr in self.get_map:
            return self.get_map[attr](attr)

    def on_create(self):
        return

    def on_submit(self, button):
        return

    def on_drop(self, name, data):
        return

    def on_change(self, name):
        return

    def on_key(self, name):
        return

    def on_focus(self, name):
        return

    def run(self):
        if self.is_sub:
            self.gui.startSubWindow(self.title)

        self.on_create()
        self.gui.bindKey("<FocusIn>", self.on_focus)
        self.gui.bindKey("<FocusOut>", self.on_focus)

        if self.is_sub:
            self.gui.stopSubWindow()
            self.gui.showSubWindow(self.title)
        else:
            try:
                self.gui.go()
                os._exit(0)
            except KeyboardInterrupt:
                return

    def apply(self, config):
        for k, v in config.items():
            self[k] = v

    def bindkey(self, key):
        self.gui.bindKey(key, self.on_key)


class EasyGridUI(EasyUI):
    def __init__(self, title, cols=1):
        super().__init__(title)
        self.cols = cols
        self.row = 0
        self.col = 0

    def next_row(self):
        self.row += 1
        self.col = 0

    def cal_span(self, span):
        return span if span > 0 else (self.cols - self.col)

    def label(self, name, text, span=1, enable_drop=True):
        span = self.cal_span(span)

        self.gui.addLabel(name, text, self.row, self.col, colspan=span)
        self.gui.setLabelBg(name, self.widget_bg)
        self.gui.setLabelFg(name, self.widget_fg)

        self.col += span

        if enable_drop:
            self.gui.setLabelDropTarget(
                name, lambda data: self.on_drop(name, data)
            )

    def entry(self, name, span=1):
        span = self.cal_span(span)

        self.gui.addEntry(name, self.row, self.col, colspan=span)
        self.gui.setEntryBg(name, self.widget_bg)
        self.gui.setEntryFg(name, self.widget_fg)

        self.col += span

        self.get_map[name] = lambda name: self.gui.getEntry(name)
        self.set_map[name] = lambda name, value: self.gui.setEntry(name, value)

        self.gui.setEntrySubmitFunction(name, self.on_submit)

    def optionbox(self, name, data=[], span=1):
        span = self.cal_span(span)

        self.gui.addOptionBox(name, data, self.row, self.col, colspan=span)
        self.gui.setOptionBoxBg(name, self.widget_bg)
        self.gui.setOptionBoxFg(name, self.widget_fg)

        self.col += span

        self.get_map[name] = lambda name: self.gui.getOptionBox(name)
        self.set_map[name] = (
            lambda name, value: self.gui.changeOptionBox(name, value, 0)
            if type(value) is list
            else self.gui.setOptionBox(name, value)
        )

        self.gui.setOptionBoxChangeFunction(name, self.on_change)

    def button(self, name, span=1):
        span = self.cal_span(span)

        self.gui.addButton(
            name, self.on_submit, self.row, self.col, colspan=span
        )
        self.gui.setButtonBg(name, self.widget_bg)
        self.gui.setButtonFg(name, self.widget_fg)

        self.col += span

    def checkbox(self, name, span=1):
        span = self.cal_span(span)

        self.gui.addCheckBox(name, self.row, self.col, colspan=span)
        self.gui.setCheckBoxBg(name, self.widget_bg)
        self.gui.setCheckBoxFg(name, self.widget_fg)

        self.col += span
        self.get_map[name] = lambda name: self.gui.getCheckBox(name)
        self.set_map[name] = lambda name, value: self.gui.setCheckBox(
            name, ticked=value
        )

        self.gui.setCheckBoxChangeFunction(name, self.on_change)

    def scale(self, name, vertical=False, show=False, range=(100, 0), span=1):
        span = self.cal_span(span)

        self.gui.addScale(name, self.row, self.col, colspan=span)
        if vertical:
            self.gui.setScaleVertical(name)
        self.gui.showScaleValue(name, show=show)
        self.gui.setScaleRange(name, range[0], range[1])
        self.gui.setScaleBg(name, self.widget_bg)
        self.gui.setScaleFg(name, self.widget_fg)

        self.col += span

        self.get_map[name] = lambda name: self.gui.getScale(name)
        self.set_map[name] = lambda name, value: self.gui.setScale(name, value)

    def separator(self, span=-1):
        span = self.cal_span(span)

        self.gui.addHorizontalSeparator(
            self.row, self.col, span if span > 0 else self.cols
        )

    def listbox(self, name, data=[], height=2, span=1, enable_drop=True):
        span = self.cal_span(span)

        self.gui.addListBox(name, data, self.row, self.col, colspan=span)
        self.gui.setListBoxHeight(name, height)
        self.gui.setListBoxBg(name, self.widget_bg)
        self.gui.setListBoxFg(name, self.widget_fg)

        if enable_drop:
            self.gui.setListBoxDropTarget(
                name, lambda data: self.on_drop(name, data), replace=True
            )

        self.col += span

        self.get_map[name] = lambda name: self.gui.getAllListItems(name)
        self.get_map[name + "@"] = lambda name: self.gui.getListBox(name[:-1])
        self.get_map[name + "#"] = lambda name: self.gui.getListBoxPos(
            name[:-1]
        )[0]
        self.set_map[name] = lambda name, value: self.gui.updateListBox(
            name, value
        )
        self.set_map[
            name + "@"
        ] = lambda name, value: self.gui.selectListItemAtPos(name[:-1], value)
        self.set_map[name + "$"] = lambda name, value: self.gui.addListItem(
            name[:-1], value
        )
        self.set_map[
            name + "#"
        ] = lambda name, value: self.gui.removeListItemAtPos(
            name[:-1],
            value
            if value is not None
            else self.gui.getListBoxPos(name[:-1])[0],
        )

    def textarea(self, name, span=1):
        span = self.cal_span(span)

        self.gui.addScrolledTextArea(name, self.row, self.col, colspan=span)
        self.gui.setTextAreaBg(name, self.widget_bg)
        self.gui.setTextAreaFg(name, self.widget_fg)

        self.col += span

        self.get_map[name] = lambda name: self.gui.getTextArea(name)
        self.set_map[name] = lambda name, value: self.gui.setTextArea(
            name, value
        )

    def replace_table_data(self, name, data):
        self.gui.deleteAllTableRows(name)
        self.gui.addTableRows(name, data)

    def table(self, name, title, span=1, width=None, height=None):
        span = self.cal_span(span)

        self.gui.addTable(name, title, self.row, self.col, colspan=span)
        self.gui.setTableBg(name, self.widget_bg)
        self.gui.setTableFg(name, self.widget_fg)
        if width is not None:
            self.gui.setTableWidth(name, width)
        if height is not None:
            self.gui.setTableHeight(name, height)

        self.col += span

        self.set_map[name] = self.replace_table_data
