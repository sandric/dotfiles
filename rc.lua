local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
local wibox = require("wibox")
local beautiful = require("beautiful")
local naughty = require("naughty")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup").widget

if awesome.startup_errors then
  naughty.notify({ preset = naughty.config.presets.critical,
                   title = "Oops, there were errors during startup!",
                   text = awesome.startup_errors })
end

do
  local in_error = false
  awesome.connect_signal("debug::error", function (err)
                           if in_error then return end
                           in_error = true

                           naughty.notify({ preset = naughty.config.presets.critical,
                                            title = "Oops, an error happened!",
                                            text = tostring(err) })
                           in_error = false
  end)
end

beautiful.init(gears.filesystem.get_themes_dir() .. "default/theme.lua")

terminal = "urxvt"
editor = os.getenv("EDITOR") or "emacsclient -c"
editor_cmd = editor


awful.layout.layouts = {
  awful.layout.suit.floating,
  awful.layout.suit.tile,
  awful.layout.suit.tile.left,
  awful.layout.suit.tile.bottom,
  awful.layout.suit.tile.top,
  awful.layout.suit.fair,
  awful.layout.suit.fair.horizontal,
  awful.layout.suit.spiral,
  awful.layout.suit.spiral.dwindle,
  awful.layout.suit.max,
  awful.layout.suit.max.fullscreen,
  awful.layout.suit.magnifier,
  awful.layout.suit.corner.nw,
}

local function client_menu_toggle_fn()
  local instance = nil

  return function ()
    if instance and instance.wibox.visible then
      instance:hide()
      instance = nil
    else
      instance = awful.menu.clients({ theme = { width = 250 } })
    end
  end
end

myawesomemenu = {
  { "restart", awesome.restart },
  { "quit", function() awesome.quit() end}
}
mymainmenu = awful.menu({
    items = {
      { "awesome", myawesomemenu, beautiful.awesome_icon },
      { "open terminal", terminal }
    }
})
mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })
menubar.utils.terminal = terminal

local taglist_buttons = gears.table.join(
  awful.button({ }, 1, function(t) t:view_only() end),
  awful.button({ modkey }, 1, function(t)
      if client.focus then
        client.focus:move_to_tag(t)
      end
  end),
  awful.button({ }, 3, awful.tag.viewtoggle),
  awful.button({ modkey }, 3, function(t)
      if client.focus then
        client.focus:toggle_tag(t)
      end
  end),
  awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
  awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end))

local tasklist_buttons = gears.table.join(
  awful.button({ }, 1, function (c)
      if c == client.focus then
        c.minimized = true
      else
        c.minimized = false
        if not c:isvisible() and c.first_tag then
          c.first_tag:view_only()
        end
        client.focus = c
        c:raise()
      end
  end),
  awful.button({ }, 3, client_menu_toggle_fn()),
  awful.button({ }, 4, function ()
      awful.client.focus.byidx(1)
  end),
  awful.button({ }, 5, function ()
      awful.client.focus.byidx(-1)
end))

global_screen_width = 0
local function set_wallpaper(s)
  global_screen_width = s.geometry.width

  if beautiful.wallpaper then
    local wallpaper = beautiful.wallpaper
    if type(wallpaper) == "function" then wallpaper = wallpaper(s)
    end
    gears.wallpaper.maximized(wallpaper, s, true)
  end
end

screen.connect_signal("property::geometry", set_wallpaper)

awful.screen.connect_for_each_screen(function(s)
    set_wallpaper(s)

    awful.tag({ "Main" }, s, awful.layout.layouts[1])

    s.mypromptbox = awful.widget.prompt()

    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(gears.table.join(
                            awful.button({ }, 1, function () awful.layout.inc( 1) end),
                            awful.button({ }, 3, function () awful.layout.inc(-1) end),
                            awful.button({ }, 4, function () awful.layout.inc( 1) end),
                            awful.button({ }, 5, function () awful.layout.inc(-1) end)))

    s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all, taglist_buttons)

    s.mytasklist = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, tasklist_buttons)

    s.mywibox = awful.wibar({position = "top", screen = s, visible = false})

    s.mywibox:setup {
      layout = wibox.layout.align.horizontal,
      { layout = wibox.layout.fixed.horizontal,
        mylauncher,
        s.mytaglist,
        s.mypromptbox,
      },
      s.mytasklist,
      { layout = wibox.layout.fixed.horizontal,
        mykeyboardlayout,
        wibox.widget.systray(),
        mytextclock,
        s.mylayoutbox,
      },
    }
end)

kblayout = "us"
function toggle_kblayout()
  if kblayout == "ru" then
    kblayout = "sandric"
    awful.util.spawn("sh -c './keyboard'")
  else
    kblayout = "ru"
    awful.util.spawn("sh -c 'setxkbmap ru'")
  end
end

function switch_to_terminal()
  local matcher = function (c)
    return awful.rules.match(c, {class = 'URxvt'})
  end
  awful.client.run_or_raise(terminal, matcher)
end

function switch_to_emacs()
  local matcher = function (c)
    return awful.rules.match(c, {class = 'Emacs'})
  end
  awful.client.run_or_raise("emacsclient -c", matcher)
end

function switch_to_skype()
  local matcher = function (c)
    return awful.rules.match(c, {class = 'Skypeforlinux'})
  end
  awful.client.run_or_raise("skypeforlinux", matcher)
end

function switch_to_browser()
  local matcher = function (c)
    return awful.rules.match(c, {class = 'Vivaldi-stable'})
  end
  awful.client.run_or_raise('vivaldi', matcher)
end


function press_escape()
  awful.util.spawn("sh -c 'xdotool keyup Meta_L Meta_R Alt_L Alt_R; xdotool key --clearmodifiers Escape'")
end


globalkeys = gears.table.join(
  awful.key({}, "#90", awesome.quit,
    {description = "quit awesome on KP_0", group = "awesome"}),
  awful.key({}, "#86", awesome.restart,
    {description = "quit awesome on KP_PLUS", group = "awesome"}),

  awful.key({"Mod4", "Shift"}, "e", switch_to_terminal),

  awful.key({"Mod4", "Shift"}, 'n', switch_to_emacs),

  awful.key({"Mod4", "Shift"}, 'h', switch_to_skype),

  awful.key({"Mod4", "Shift"}, 'i', switch_to_browser),

  awful.key({"Mod4", "Shift"}, "q", function ()
      awful.util.spawn("rofi -show run")
  end),

  awful.key({"Mod4", "Shift"}, "c", function ()
      awful.util.spawn("rofi -show window")
  end)
)

clientkeys = gears.table.join(
  awful.key({"Mod4", "Shift"}, "u",
    function (c)
      if c.width <= global_screen_width / 2 then
        c.x = 0
        c.y = 0
        c.width = global_screen_width
        c.maximized_vertical = true

        if c.class == "Emacs" then
          awful.util.spawn('sh /home/sandric/.config/enlarge_emacs.sh')
        end
      end
    end,
    {description = "maximize", group = "client"}),

  awful.key({"Mod4", "Shift"}, "a",
    function (c)
      c:kill()
    end,
    {description = "kill", group = "client"}),

  awful.key({"Mod4", "Shift"}, "l",
    function (c)
      c.x = 0
      c.y = 0
      c.width = global_screen_width / 2
      c.maximized_vertical = true

      if c.class == "Emacs" then
        awful.util.spawn('sh /home/sandric/.config/shrink_emacs.sh')
      end
    end,
    {description = "To left side", group = "client"}),

  awful.key({"Mod4", "Shift"}, "y",
    function (c)
      c.x = global_screen_width / 2
      c.y = 0
      c.width = global_screen_width / 2
      c.maximized_vertical = true

      if c.class == "Emacs" then
        awful.util.spawn('sh /home/sandric/.config/shrink_emacs.sh')
      end
    end,
    {description = "To right side", group = "client"})

--   awful.key({"Mod1"}, "q",
--     function (c)
--       if c.class == "Vivaldi-stable" then
--         press_escape()
--       end
--     end,
--     {description = "To right side", group = "client"})
)

root.keys(globalkeys)

awful.rules.rules = {
  { rule = { },
    properties = { border_width = beautiful.border_width,
                   border_color = beautiful.border_normal,
                   focus = awful.client.focus.filter,
                   raise = true,
                   keys = clientkeys,
                   buttons = clientbuttons,
                   screen = awful.screen.preferred,
                   placement = awful.placement.no_overlap+awful.placement.no_offscreen,
                   size_hints_honor = false
    }
  },
  { rule_any = {
      instance = {
        "DTA",
        "copyq",
      },
      class = {
        "Arandr",
        "Gpick",
        "Kruler",
        "MessageWin",
        "Sxiv",
        "Wpa_gui",
        "pinentry",
        "veromix",
        "xtightvncviewer"},

      name = {
        "Event Tester",
      },
      role = {
        "AlarmWindow",
        "pop-up",
      }
  }, properties = { floating = true }},

  { rule_any = {
      type = { "normal", "dialog" }
    }, properties = { titlebars_enabled = false }
  },
  {
    rule = {
      class = "URxvt"
    },
    properties = {
      x = 444
    }
  }
}

client.connect_signal("manage", function (c)
                        if awesome.startup and
                          not c.size_hints.user_position
                        and not c.size_hints.program_position then
                          awful.placement.no_offscreen(c)
                        end
end)

client.connect_signal("mouse::enter", function(c)
                        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
                        and awful.client.focus.filter(c) then
                          client.focus = c
                        end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
