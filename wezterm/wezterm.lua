local wezterm = require("wezterm")
local act = wezterm.action
local mux = wezterm.mux

local config = wezterm.config_builder()
local zellij_session_counter = 0

math.randomseed(os.time())

local function zellij_command(...)
	local command = { "env", "-u", "ZELLIJ", "-u", "ZELLIJ_SESSION_NAME", "zellij" }
	for _, arg in ipairs({ ... }) do
		table.insert(command, arg)
	end
	return command
end

local function zellij_attach_args(session_name)
	return zellij_command("attach", session_name)
end

local function zellij_attach_or_create_args(session_name)
	return zellij_command("attach", "--create", session_name)
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

local function unique_zellij_session_name()
	zellij_session_counter = zellij_session_counter + 1
	local timestamp = os.date("%Y%m%d-%H%M%S")
	local random_suffix = math.random(0, 0xffff)
	return string.format("wz-%s-%04x-%03d", timestamp, random_suffix, zellij_session_counter)
end

local function zellij_session_name_from_line(line)
	return line:match("^(.-)%s+%[Created .-%]")
end

local function running_zellij_sessions()
	local success, stdout, stderr = wezterm.run_child_process(zellij_command("list-sessions", "--no-formatting"))
	if not success then
		if stderr and stderr:find("No active zellij sessions found.", 1, true) then
			return {}
		end
		wezterm.log_warn("zellij list-sessions failed: " .. (stderr or ""))
		return nil
	end

	local sessions = {}
	for line in stdout:gmatch("[^\r\n]+") do
		if not line:find("(EXITED - attach to resurrect)", 1, true) then
			local session_name = zellij_session_name_from_line(line)
			if session_name and session_name ~= "" then
				table.insert(sessions, session_name)
			end
		end
	end
	return sessions
end

local function spawn_default_tab(window, pane)
	window:perform_action(act.SpawnTab("CurrentPaneDomain"), pane)
end

local function spawn_zellij_tab(window, pane)
	if not running_zellij_sessions() then
		spawn_default_tab(window, pane)
		return
	end

	window:mux_window():spawn_tab(zellij_spawn_command(zellij_attach_or_create_args(unique_zellij_session_name())))
end

-- Define emacs-like copy mode keys with extended functions
local function emacs_copy_mode_keys()
	-- start from the default copy_mode key table
	local copy_mode = wezterm.gui.default_key_tables().copy_mode
	-- keys to add/override
	local keys = {
		-- basic movement keys
		{ key = "e", mods = "CTRL", action = act.CopyMode("MoveToEndOfLineContent") },
		{ key = "a", mods = "CTRL", action = act.CopyMode("MoveToStartOfLine") },
		{ key = "f", mods = "CTRL", action = act.CopyMode("MoveRight") },
		{ key = "b", mods = "CTRL", action = act.CopyMode("MoveLeft") },
		{ key = "n", mods = "CTRL", action = act.CopyMode("MoveDown") },
		{ key = "p", mods = "CTRL", action = act.CopyMode("MoveUp") },
		{ key = "f", mods = "ALT", action = act.CopyMode("MoveForwardWord") },
		{ key = "b", mods = "ALT", action = act.CopyMode("MoveBackwardWord") },
		{ key = "g", mods = "CTRL", action = act.CopyMode("Close") },
		-- page scrolling
		{ key = "v", mods = "CTRL", action = act.CopyMode({ MoveByPage = 1.0 }) },
		{ key = "v", mods = "ALT", action = act.CopyMode({ MoveByPage = -1.0 }) },
		-- start selection (mark) using C-SPACE
		{ key = "Space", mods = "CTRL", action = act.CopyMode({ SetSelectionMode = "Cell" }) },
		-- copy selection and exit copy mode using M-w
		{
			key = "w",
			mods = "ALT",
			action = act.Multiple({
				act.CopyTo("ClipboardAndPrimarySelection"),
				act.CopyMode("Close"),
			}),
		},
		-- disable default y to avoid vim-like copy
		{ key = "y", mods = "NONE", action = act.Nop },
	}
	-- insert new mappings at the front of the key table so they take precedence
	for _, key in ipairs(keys) do
		table.insert(copy_mode, 1, key)
	end
	return copy_mode
end

-- Basic configuration
config.color_scheme = "Catppuccin Latte"
config.font = wezterm.font("DejaVuSansM Nerd Font Mono")
config.term = "wezterm"
config.hide_tab_bar_if_only_one_tab = true
config.use_fancy_tab_bar = false
config.window_close_confirmation = "NeverPrompt"
config.scrollback_lines = 20000
-- reload config automatically on save
config.automatically_reload_config = true

-- Use Ctrl+\ as leader key
config.leader = {
	key = "\\",
	mods = "CTRL",
	timeout_milliseconds = 1500,
}

wezterm.on("gui-startup", function(cmd)
	local sessions = running_zellij_sessions()
	if not sessions then
		mux.spawn_window(cmd or {})
		return
	end

	local first_args = #sessions == 0 and zellij_attach_or_create_args(unique_zellij_session_name())
		or zellij_attach_args(sessions[1])
	local _, _, window = mux.spawn_window(zellij_spawn_command(first_args))

	for i = 2, #sessions do
		window:spawn_tab(zellij_spawn_command(zellij_attach_args(sessions[i])))
	end
end)

wezterm.on("new-tab-button-click", function(window, pane)
	spawn_zellij_tab(window, pane)
	return false
end)

-- Global key bindings
config.keys = {
	-- Enter copy mode via leader key (Ctrl+\)
	{ key = "\\", mods = "CTRL", action = act.ActivateCopyMode },
	-- Pane splits
	{ key = "2", mods = "LEADER", action = act.SplitPane({ direction = "Down", size = { Percent = 35 } }) },
	{ key = "3", mods = "LEADER", action = act.SplitPane({ direction = "Right", size = { Percent = 35 } }) },
	-- Close current pane
	{ key = "0", mods = "LEADER", action = act.CloseCurrentPane({ confirm = true }) },
	-- Pane navigation using hjkl
	{ key = "h", mods = "LEADER", action = act.ActivatePaneDirection("Left") },
	{ key = "j", mods = "LEADER", action = act.ActivatePaneDirection("Down") },
	{ key = "k", mods = "LEADER", action = act.ActivatePaneDirection("Up") },
	{ key = "l", mods = "LEADER", action = act.ActivatePaneDirection("Right") },
	-- Pane select with labels (like Avy for panes)
	{ key = "[", mods = "LEADER", action = act.PaneSelect({}) },
	-- Spawn a new tab with a fresh Zellij session
	{ key = "t", mods = "LEADER", action = wezterm.action_callback(spawn_zellij_tab) },
	{ key = "t", mods = "CTRL|SHIFT", action = wezterm.action_callback(spawn_zellij_tab) },
	-- Show workspace (tab group) launcher
	{ key = "w", mods = "LEADER", action = act.ShowLauncherArgs({ flags = "WORKSPACES" }) },
	-- Debug overlay and reload configuration
	{ key = "d", mods = "LEADER", action = act.ShowDebugOverlay },
	{ key = "r", mods = "LEADER", action = act.ReloadConfiguration },
	-- Copy selection when present, otherwise send Ctrl-C
	{
		key = "c",
		mods = "CTRL",
		action = wezterm.action_callback(function(window, pane)
			local has_selection = window:get_selection_text_for_pane(pane) ~= ""
			if has_selection then
				window:perform_action(act.CopyTo("ClipboardAndPrimarySelection"), pane)
				window:perform_action(act.ClearSelection, pane)
			else
				window:perform_action(act.SendKey({ key = "c", mods = "CTRL" }), pane)
			end
		end),
	},
	-- Tab selection via Alt+1…Alt+9
	{ key = "1", mods = "ALT", action = act.ActivateTab(0) },
	{ key = "2", mods = "ALT", action = act.ActivateTab(1) },
	{ key = "3", mods = "ALT", action = act.ActivateTab(2) },
	{ key = "4", mods = "ALT", action = act.ActivateTab(3) },
	{ key = "5", mods = "ALT", action = act.ActivateTab(4) },
	{ key = "6", mods = "ALT", action = act.ActivateTab(5) },
	{ key = "7", mods = "ALT", action = act.ActivateTab(6) },
	{ key = "8", mods = "ALT", action = act.ActivateTab(7) },
	{ key = "9", mods = "ALT", action = act.ActivateTab(8) },
}

-- Assign the customized copy mode key table
config.key_tables = {
	copy_mode = emacs_copy_mode_keys(),
}

return config
