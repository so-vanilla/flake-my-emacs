local wezterm = require("wezterm")
local act = wezterm.action
local mux = wezterm.mux

local config = wezterm.config_builder()
local ZELLIJ = MY_EMACS_ZELLIJ or "zellij"

local ZELLIJ_WORKSPACES = {
	"wz-01",
	"wz-02",
	"wz-03",
	"wz-04",
	"wz-05",
	"wz-06",
	"wz-07",
	"wz-08",
	"wz-09",
	"wz-10",
}

local function zellij_command(...)
	local command = { "env", "-u", "ZELLIJ", "-u", "ZELLIJ_SESSION_NAME", ZELLIJ }
	for _, arg in ipairs({ ... }) do
		table.insert(command, arg)
	end
	return command
end

local function zellij_attach_or_create_args(session_name)
	return zellij_command("attach", "--create", session_name, "options", "--default-layout", "workspace")
end

local function zellij_spawn_command(args)
	return {
		args = args,
		set_environment_variables = {
			ZELLIJ = "",
			ZELLIJ_SESSION_NAME = "",
		},
	}
end

config.color_scheme = "Catppuccin Latte"
config.font = wezterm.font("DejaVuSansM Nerd Font Mono")
config.term = "xterm-256color"
config.hide_tab_bar_if_only_one_tab = false
config.use_fancy_tab_bar = false
config.window_close_confirmation = "NeverPrompt"
config.scrollback_lines = 20000
config.automatically_reload_config = true

config.disable_default_key_bindings = true
config.disable_default_mouse_bindings = true
config.show_new_tab_button_in_tab_bar = false
config.show_close_tab_button_in_tabs = false

wezterm.on("gui-startup", function(cmd)
	if cmd and cmd.args and #cmd.args > 0 then
		mux.spawn_window(cmd)
		return
	end

	local first_session = ZELLIJ_WORKSPACES[1]
	local first_tab, _, window = mux.spawn_window(zellij_spawn_command(zellij_attach_or_create_args(first_session)))
	first_tab:set_title(first_session)

	for i = 2, #ZELLIJ_WORKSPACES do
		local session_name = ZELLIJ_WORKSPACES[i]
		local tab = window:spawn_tab(zellij_spawn_command(zellij_attach_or_create_args(session_name)))
		tab:set_title(session_name)
	end
end)

config.keys = {
	{ key = "1", mods = "ALT", action = act.ActivateTab(0) },
	{ key = "2", mods = "ALT", action = act.ActivateTab(1) },
	{ key = "3", mods = "ALT", action = act.ActivateTab(2) },
	{ key = "4", mods = "ALT", action = act.ActivateTab(3) },
	{ key = "5", mods = "ALT", action = act.ActivateTab(4) },
	{ key = "6", mods = "ALT", action = act.ActivateTab(5) },
	{ key = "7", mods = "ALT", action = act.ActivateTab(6) },
	{ key = "8", mods = "ALT", action = act.ActivateTab(7) },
	{ key = "9", mods = "ALT", action = act.ActivateTab(8) },
	{ key = "0", mods = "ALT", action = act.ActivateTab(9) },
	{ key = "1", mods = "CMD", action = act.ActivateTab(0) },
	{ key = "2", mods = "CMD", action = act.ActivateTab(1) },
	{ key = "3", mods = "CMD", action = act.ActivateTab(2) },
	{ key = "4", mods = "CMD", action = act.ActivateTab(3) },
	{ key = "5", mods = "CMD", action = act.ActivateTab(4) },
	{ key = "6", mods = "CMD", action = act.ActivateTab(5) },
	{ key = "7", mods = "CMD", action = act.ActivateTab(6) },
	{ key = "8", mods = "CMD", action = act.ActivateTab(7) },
	{ key = "9", mods = "CMD", action = act.ActivateTab(8) },
	{ key = "0", mods = "CMD", action = act.ActivateTab(9) },

	{ key = "v", mods = "CTRL|SHIFT", action = act.PasteFrom("Clipboard") },
}

return config
