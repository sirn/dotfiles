require("awful")
require("beautiful")
require("naughty")
require("wicked")

-- General configurations ---------------------------------------------------

-- Beautiful theme
theme_path = "/usr/share/awesome/themes/default/theme.lua"
beautiful.init(theme_path)

-- Variables
terminal = "urxvt"
editor = "gvim"
editor_cmd = terminal .. " -e " .. editor

-- General configurations
modkey = "Mod4" -- modifier key (Mod4 = Windows key)
use_titlebar = false -- disable titlebar on all applications

-- Awful layout
layouts = {
  awful.layout.suit.tile,
  awful.layout.suit.tile.bottom,
  awful.layout.suit.max,
}

-- Float by default
floatapps = {
  ["MPlayer"] = true,
  ["ossxmix"] = true,
  ["gimpshop"] = true,
}

-- Predefined application tags
apptags = {
  ["task_radio"] = { screen = 1, tag = 6 },
  ["task_ssh"] = { screen = 1, tag = 6 },
  ["VirtualBox"] = { screen = 1, tag = 3 },
}

-- Define tags table.
tags = {}
for s = 1, screen.count() do
  tags[s] = {}
  for tagnumber = 1, 6 do
    tags[s][tagnumber] = tag(tagnumber)
    tags[s][tagnumber].screen = s
    awful.layout.set(layouts[1], tags[s][tagnumber])
  end
  tags[s][1].selected = true
end

-- Widgets ------------------------------------------------------------------

menu_awesome = {
  { "edit config", editor_cmd .. " " .. awful.util.getdir("config") .. "/rc.lua" },
  { "restart", awesome.restart },
  { "quit", awesome.quit },
}

menu_applications = {
  { "firefox", "firefox" },
  { "irssi", "urxvt -name irssi -e irssi" },
  { "mplayer", "gnome-mplayer" },
  { "keepassx", "keepassx" },
}

task_cmd = terminal .. " -name task_"
menu_tasks = {
  { "listen to radio", task_cmd .. "radio -e mplayer http_proxy://localhost:8888/http://www.animenfo.com/radio/listen.m3u" },
  { "ssh to futami", task_cmd .. "ssh -e ssh -L0.0.0.0:8888:proxy.csloxinfo.com:8080 sirn@210.1.31.215" },
}

mymainmenu = awful.menu.new({
  items = {
    { "awesome", menu_awesome, beautiful.awesome_icon },
    { "applications", menu_applications },
    { "tasks", menu_tasks },
    { "open terminal", terminal },
  }
})

mylauncher = awful.widget.launcher({
  image = image(beautiful.awesome_icon),
  menu = mymainmenu
})

-- Create a systray
mysystray = widget({ type = "systray", align = "right" })

mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
  awful.button({ }, 1, awful.tag.viewonly),
  awful.button({ modkey }, 1, awful.client.movetotag),
  awful.button({ }, 3, function (tag) tag.selected = not tag.selected end),
  awful.button({ modkey }, 3, awful.client.toggletag),
  awful.button({ }, 4, awful.tag.viewnext),
  awful.button({ }, 5, awful.tag.viewprev)
)

widget_date = widget({
  type = "textbox",
  name = "datewidget",
  align = "right"
})
wicked.register(widget_date, wicked.widgets.date, ' %c  ')

mytasklist = {}
for s = 1, screen.count() do
  mypromptbox[s] = awful.widget.prompt({ align = "left" })

  mylayoutbox[s] = widget({ type = "imagebox", align = "right" })
  mylayoutbox[s]:buttons(awful.util.table.join(
    awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
    awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
    awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
    awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)
  ))

  mytaglist[s] = awful.widget.taglist(
    s, awful.widget.taglist.label.all, mytaglist.buttons
  )
  mytasklist[s] = awful.widget.tasklist(
    function(c)
      return awful.widget.tasklist.label.currenttags(c, s)
    end, mytasklist.buttons)

  -- Create the wibox
  mywibox[s] = wibox({
    position = "top",
    fg = beautiful.fg_normal,
    bg = beautiful.bg_normal,
  })
  mywibox[s].widgets = {
    mylauncher,
    mytaglist[s],
    mytasklist[s],
    mypromptbox[s],
    widget_date,
    mylayoutbox[s],
    s == 1 and mysystray or nil
  }
  mywibox[s].screen = s
end

-- Mouse bindings -----------------------------------------------------------

root.buttons(awful.util.table.join(
  awful.button({ }, 3, function () mymainmenu:toggle() end),
  awful.button({ }, 4, awful.tag.viewnext),
  awful.button({ }, 5, awful.tag.viewprev)
))

-- Key bindings -------------------------------------------------------------

globalkeys = awful.util.table.join(
  awful.key({ modkey,           }, "Left",   awful.tag.viewprev       ),
  awful.key({ modkey,           }, "Right",  awful.tag.viewnext       ),
  awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

  awful.key({ modkey,           }, "j",
    function ()
      awful.client.focus.byidx( 1)
      if client.focus then client.focus:raise() end
    end),
  awful.key({ modkey,           }, "k",
    function ()
      awful.client.focus.byidx(-1)
        if client.focus then client.focus:raise() end
    end),
  awful.key({ modkey,           }, "w", function () mymainmenu:show(true)        end),

  -- Layout manipulation
  awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1) end),
  awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1) end),
  awful.key({ modkey, "Control" }, "j", function () awful.screen.focus( 1)       end),
  awful.key({ modkey, "Control" }, "k", function () awful.screen.focus(-1)       end),
  awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
  awful.key({ modkey,           }, "Tab",
    function ()
      awful.client.focus.history.previous()
        if client.focus then
          client.focus:raise()
        end
    end),

  -- Standard program
  awful.key({ modkey,           }, "Return", function () awful.util.spawn(terminal) end),
  awful.key({ modkey, "Control" }, "r", awesome.restart),
  awful.key({ modkey, "Shift"   }, "q", awesome.quit),

  awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.02)    end),
  awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.02)    end),
  awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end),
  awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end),
  awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end),
  awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end),
  awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
  awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

  -- Prompt
  awful.key({ modkey },            "r",     function () mypromptbox[mouse.screen]:run() end),

  awful.key({ modkey }, "x",
    function ()
      awful.prompt.run({ prompt = "Run Lua code: " },
      mypromptbox[mouse.screen].widget,
      awful.util.eval, nil,
      awful.util.getdir("cache") .. "/history_eval")
    end)
)

-- Client awful tagging: this is useful to tag some clients and then do stuff like move to tag on them
clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
    awful.key({ modkey, "Shift"   }, "r",      function (c) c:redraw()                       end),
    awful.key({ modkey }, "t", awful.client.togglemarked),
    awful.key({ modkey,}, "m",
      function (c)
        c.maximized_horizontal = not c.maximized_horizontal
        c.maximized_vertical   = not c.maximized_vertical
      end)
)

-- Compute the maximum number of digit we need, limited to 9
keynumber = 0
for s = 1, screen.count() do
  keynumber = math.min(9, math.max(#tags[s], keynumber));
end

for i = 1, keynumber do
  globalkeys = awful.util.table.join(globalkeys,
  awful.key({ modkey }, i,
            function ()
              local screen = mouse.screen
              if tags[screen][i] then
                awful.tag.viewonly(tags[screen][i])
              end
            end),
  awful.key({ modkey, "Control" }, i,
            function ()
              local screen = mouse.screen
              if tags[screen][i] then
                tags[screen][i].selected = not tags[screen][i].selected
              end
            end),
  awful.key({ modkey, "Shift" }, i,
            function ()
              if client.focus and tags[client.focus.screen][i] then
                awful.client.movetotag(tags[client.focus.screen][i])
              end
            end),
  awful.key({ modkey, "Control", "Shift" }, i,
            function ()
              if client.focus and tags[client.focus.screen][i] then
                awful.client.toggletag(tags[client.focus.screen][i])
              end
            end),
  awful.key({ modkey, "Shift" }, "F" .. i,
            function ()
              local screen = mouse.screen
              if tags[screen][i] then
                for k, c in pairs(awful.client.getmarked()) do
                  awful.client.movetotag(tags[screen][i], c)
                end
              end
           end))
end

-- Set keys
root.keys(globalkeys)

-- Hooks --------------------------------------------------------------------

-- Hook function to execute when marking a client
awful.hooks.marked.register(function (c)
  c.border_color = beautiful.border_marked
end)

-- Hook function to execute when unmarking a client.
awful.hooks.unmarked.register(function (c)
  c.border_color = beautiful.border_normal -- beautiful.border_focus
end)

-- Hook function to execute when the mouse enters a client.
awful.hooks.mouse_enter.register(function (c)
  -- Sloppy focus, but disabled for magnifier layout
  if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
    and awful.client.focus.filter(c) then
    client.focus = c
  end
end)

-- Hook function to execute when a new client appears.
awful.hooks.manage.register(function (c, startup)
  -- If we are not managing this application at startup,
  -- move it to the screen where the mouse is.
  -- We only do it for filtered windows (i.e. no dock, etc).
  if not startup and awful.client.focus.filter(c) then
    c.screen = mouse.screen
  end

  if use_titlebar then
    -- Add a titlebar
    awful.titlebar.add(c, { modkey = modkey })
  end
  -- Add mouse bindings
  c:buttons(awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize)
  ))
  -- New client may not receive focus
  -- if they're not focusable, so set border anyway.
  c.border_width = beautiful.border_width
  c.border_color = beautiful.border_normal

  -- Check if the application should be floating.
  local cls = c.class
  local inst = c.instance
  if floatapps[cls] ~= nil then
    awful.client.floating.set(c, floatapps[cls])
  elseif floatapps[inst] ~= nil then
    awful.client.floating.set(c, floatapps[inst])
  end

  -- Check application->screen/tag mappings.
  local target
  if apptags[cls] then
    target = apptags[cls]
  elseif apptags[inst] then
    target = apptags[inst]
  end
  if target then
    c.screen = target.screen
    awful.client.movetotag(tags[target.screen][target.tag], c)
  end

  -- Do this after tag mapping, so you don't see it on the wrong tag for a split second.
  client.focus = c

  -- Set key bindings
  c:keys(clientkeys)

  -- Set the windows at the slave,
  -- i.e. put it at the end of others instead of setting it master.
  -- awful.client.setslave(c)

  -- Honor size hints: if you want to drop the gaps between windows, set this to false.
  -- c.size_hints_honor = false
end)

-- Hook function to execute when arranging the screen.
-- (tag switch, new client, etc)
awful.hooks.arrange.register(function (screen)
  local layout = awful.layout.getname(awful.layout.get(screen))
  if layout and beautiful["layout_" ..layout] then
    mylayoutbox[screen].image = image(beautiful["layout_" .. layout])
  else
    mylayoutbox[screen].image = nil
  end

  -- Give focus to the latest client in history if no window has focus
  -- or if the current window is a desktop or a dock one.
  if not client.focus then
    local c = awful.client.focus.history.get(screen, 0)
    if c then client.focus = c end
  end
end)
