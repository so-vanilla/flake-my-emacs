local wezterm = require("wezterm")
local act = wezterm.action

local config = wezterm.config_builder()

local function emacs_copy_mode_keys()
	local copy_mode = wezterm.gui.default_key_tables().copy_mode
	local keys = {
		{ key = "e", mods = "CTRL", action = act.CopyMode("MoveToEndOfLineContent") },
		{ key = "a", mods = "CTRL", action = act.CopyMode("MoveToStartOfLine") },
		{ key = "f", mods = "CTRL", action = act.CopyMode("MoveRight") },
		{ key = "b", mods = "CTRL", action = act.CopyMode("MoveLeft") },
		{ key = "n", mods = "CTRL", action = act.CopyMode("MoveDown") },
		{ key = "p", mods = "CTRL", action = act.CopyMode("MoveUp") },
		{ key = "f", mods = "ALT", action = act.CopyMode("MoveForwardWord") },
		{ key = "b", mods = "ALT", action = act.CopyMode("MoveBackwardWord") },
		{ key = "g", mods = "CTRL", action = act.CopyMode("Close") },
	}

	for _, key in ipairs(keys) do
		table.insert(copy_mode, key)
	end

	return copy_mode
end

config.color_scheme = "Catppuccin Latte"
config.font = wezterm.font("DejaVuSansM Nerd Font Mono")
config.term = "wezterm"
config.hide_tab_bar_if_only_one_tab = true
config.use_fancy_tab_bar = false
config.window_close_confirmation = "NeverPrompt"
config.scrollback_lines = 20000

config.disable_default_key_bindings = true
config.enable_kitty_keyboard = true
config.allow_win32_input_mode = false
config.enable_csi_u_key_encoding = false
config.treat_left_ctrlalt_as_altgr = false

config.default_prog = { "wsl.exe", "--distribution", "Ubuntu", "--cd", "~" }

config.leader = {
	key = "phys:o",
	mods = "ALT|SHIFT",
	timeout_milliseconds = 1500,
}

config.keys = {
	{
		key = "e",
		mods = "ALT",
		action = act.ActivateCopyMode,
	},
	{
		key = "o",
		mods = "LEADER",
		action = act.ActivatePaneDirection("Next"),
	},
	{
		key = "2",
		mods = "LEADER",
		action = act.SplitPane({
			direction = "Down",
			size = { Percent = 35 },
		}),
	},
	{
		key = "3",
		mods = "LEADER",
		action = act.SplitPane({
			direction = "Right",
			size = { Percent = 35 },
		}),
	},
	{
		key = "0",
		mods = "LEADER",
		action = act.CloseCurrentPane({ confirm = true }),
	},
	{
		key = "f",
		mods = "LEADER|CTRL",
		action = act.ActivatePaneDirection("Right"),
	},
	{
		key = "b",
		mods = "LEADER|CTRL",
		action = act.ActivatePaneDirection("Left"),
	},
	{
		key = "p",
		mods = "LEADER|CTRL",
		action = act.ActivatePaneDirection("Up"),
	},
	{
		key = "n",
		mods = "LEADER|CTRL",
		action = act.ActivatePaneDirection("Down"),
	},
	{
		key = "d",
		mods = "LEADER",
		action = act.ShowDebugOverlay,
	},
	{
		key = "r",
		mods = "LEADER",
		action = act.ReloadConfiguration,
	},
	{
		key = "phys:v",
		mods = "CTRL|SHIFT",
		action = act.PasteFrom("Clipboard"),
	},
}

config.key_tables = {
	copy_mode = emacs_copy_mode_keys(),
}

config.mouse_bindings = {
	{
		event = { Down = { streak = 1, button = "Right" } },
		mods = "NONE",
		action = act.PasteFrom("Clipboard"),
	},
}

return config
