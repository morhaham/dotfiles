local wezterm = require("wezterm")
local colors = require("lua/rose-pine").colors()
local window_frame = require("lua/rose-pine").window_frame()
local act = wezterm.action

-- This table will hold the configuration.
local config = {}
local mux = wezterm.mux

-- This is a hook that is called when the GUI process starts up.
wezterm.on("gui-attached", function(domain)
	-- maximize all displayed windows on startup
	local workspace = mux.get_active_workspace()
	for _, window in ipairs(mux.all_windows()) do
		if window:get_workspace() == workspace then
			window:gui_window():maximize()
		end
	end
end)

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
	config = wezterm.config_builder()
end

-- config.font = wezterm.font("FiraCode Nerd Font", { weight = "Medium" })
-- config.font = wezterm.font("IosevkaTerm Nerd Font Propo", { weight = "Medium" })
-- config.font = wezterm.font("Fragment Mono", { weight = "Medium" })
config.font = wezterm.font("JetbrainsMono Nerd Font", { weight = "Medium" })

config.window_decorations = "RESIZE"
config.initial_cols = 180
config.initial_rows = 80
config.cell_width = 0.9
config.freetype_load_flags = "NO_HINTING"
config.font_size = 14.0
config.line_height = 1.2
-- config.window_decorations = "RESIZE"
-- config.window_padding = {
-- 	left = 0,
-- 	right = 0,
-- 	top = 0,
-- 	bottom = 0,
-- }
config.enable_tab_bar = false
config.window_frame = window_frame
config.macos_window_background_blur = 50
config.colors = colors
config.text_background_opacity = 1
local dimmer = { brightness = 0.1 }
config.background = {
	{
		source = {
			Color = colors.background,
		},
		width = "100%",
		height = "100%",
		repeat_x = "NoRepeat",
		opacity = 1,
	},
	{
		source = {
			Color = "black",
		},
		width = "100%",
		height = "100%",
		repeat_x = "NoRepeat",
		opacity = 0.2,
	},

	-- {
	-- 	source = {
	-- 		File = "/Users/morh/wallpapers/starwars9.jpeg",
	-- 	},
	-- 	width = "3900",
	-- 	repeat_x = "NoRepeat",
	-- 	horizontal_align = "Center",
	-- 	opacity = 0.3,
	-- 	hsb = dimmer,
	-- },
}

-- Keybindings
config.keys = {
	{ mods = "OPT", key = "LeftArrow", action = act.SendKey({ mods = "ALT", key = "b" }) },
	{ mods = "OPT", key = "RightArrow", action = act.SendKey({ mods = "ALT", key = "f" }) },
	{ mods = "CMD", key = "LeftArrow", action = act.SendKey({ mods = "CTRL", key = "a" }) },
	{ mods = "CMD", key = "RightArrow", action = act.SendKey({ mods = "CTRL", key = "e" }) },
	{ mods = "CMD", key = "Backspace", action = act.SendKey({ mods = "CTRL", key = "u" }) },
	{ key = "d", mods = "CMD", action = act.SplitPane({ direction = "Right", size = { Percent = 30 } }) },
	{ key = "D", mods = "CMD", action = act.SplitPane({ direction = "Down", size = { Percent = 30 } }) },
	{ key = "w", mods = "CMD", action = act.CloseCurrentPane({ confirm = true }) },
	{ key = "H", mods = "CMD", action = act.AdjustPaneSize({ "Left", 10 }) },
	{ key = "J", mods = "CMD", action = act.AdjustPaneSize({ "Down", 10 }) },
	{ key = "K", mods = "CMD", action = act.AdjustPaneSize({ "Up", 10 }) },
	{ key = "L", mods = "CMD", action = act.AdjustPaneSize({ "Right", 10 }) },
	{ key = "h", mods = "CMD", action = act.ActivatePaneDirection("Left") },
	{ key = "l", mods = "CMD", action = act.ActivatePaneDirection("Right") },
	{ key = "k", mods = "CMD", action = act.ActivatePaneDirection("Up") },
	{ key = "j", mods = "CMD", action = act.ActivatePaneDirection("Down") },
}

return config
