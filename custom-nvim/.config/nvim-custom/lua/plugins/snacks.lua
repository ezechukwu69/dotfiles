return {
	"folke/snacks.nvim",
	name = "snacks.nvim",
	--enabled = false,
	init = function()
		local del = vim.keymap.del
		-- Normal mode Telescope keybindings
	end,
	keys = {
		{
			"+",
			function()
				Snacks.picker.explorer()
			end,
			desc = "Open Snacks explorer (Directory of Current File)",
		},
		{
			"gO",
			function()
				Snacks.picker.lsp_symbols()
				-- vim.lsp.buf.document_symbol()
			end,
			desc = "Open Snacks lsp symbols",
		},
		{
			"gS",
			function()
				Snacks.picker.lsp_workspace_symbols()
				-- vim.lsp.buf.workspace_symbol()
			end,
			desc = "Open Snacks lsp workspace symbols",
		},
		{
			"<leader>sP",
			function()
				Snacks.picker()
			end,
			desc = "Open Snack picker",
		},
		{ "<leader>,",       function() Snacks.picker.buffers() end,                                 desc = "Buffers" },
		{ "<leader>/",       function() Snacks.picker.files() end,                                   desc = "Grep (Root Dir)" },
		{ "<leader>:",       function() Snacks.picker.command_history() end,                         desc = "Command History" },
		{ "<leader><space>", function() Snacks.picker.files() end,                                   desc = "Find Files (Root Dir)" },
		{ "<leader>fn",      function() Snacks.picker.notifications() end,                           desc = "Notification History" },
		-- find
		{ "<leader>fb",      function() Snacks.picker.buffers() end,                                 desc = "Buffers" },
		{ "<leader>fB",      function() Snacks.picker.buffers({ hidden = true, nofile = true }) end, desc = "Buffers (all)" },
		-- { "<leader>fc", function () Snacks.picker.config_files() end, desc = "Find Config File" },
		{ "<leader>ff",      function() Snacks.picker.files() end,                                   desc = "Find Files (Root Dir)" },
		{ "<leader>fF",      function() Snacks.picker.files({ root = true }) end,                    desc = "Find Files (cwd)" },
		{ "<leader>fg",      function() Snacks.picker.git_files() end,                               desc = "Find Files (git-files)" },
		{ "<leader>fr",      function() Snacks.picker.recent() end,                                  desc = "Recent" },
		{ "<leader>fR",      function() Snacks.picker.recent({ filter = { cwd = true } }) end,       desc = "Recent (cwd)" },
		{ "<leader>fp",      function() Snacks.picker.projects() end,                                desc = "Projects" },
		-- git
		{ "<leader>gd",      function() Snacks.picker.git_diff() end,                                desc = "Git Diff (hunks)" },
		{ "<leader>gs",      function() Snacks.picker.git_status() end,                              desc = "Git Status" },
		{ "<leader>gS",      function() Snacks.picker.git_stash() end,                               desc = "Git Stash" },
		-- Grep
		{ "<leader>sb",      function() Snacks.picker.lines() end,                                   desc = "Buffer Lines" },
		{ "<leader>sB",      function() Snacks.picker.grep_buffers() end,                            desc = "Grep Open Buffers" },
		{ "<leader>sg",      function() Snacks.picker.grep() end,                                    desc = "Grep (Root Dir)" },
		{ "<leader>sP",      function() Snacks.picker.lazy() end,                                    desc = "Search for Plugin Spec" },
		{ "<leader>sw",      function() Snacks.picker.grep_word() end,                               desc = "Visual selection or word (Root Dir)", mode = { "n", "x" } },
		-- search
		{ '<leader>s"',      function() Snacks.picker.registers() end,                               desc = "Registers" },
		{ '<leader>s/',      function() Snacks.picker.search_history() end,                          desc = "Search History" },
		{ "<leader>sa",      function() Snacks.picker.autocmds() end,                                desc = "Autocmds" },
		{ "<leader>sc",      function() Snacks.picker.command_history() end,                         desc = "Command History" },
		{ "<leader>sC",      function() Snacks.picker.commands() end,                                desc = "Commands" },
		{ "<leader>sd",      function() Snacks.picker.diagnostics() end,                             desc = "Diagnostics" },
		{ "<leader>sD",      function() Snacks.picker.diagnostics_buffer() end,                      desc = "Buffer Diagnostics" },
		{ "<leader>sh",      function() Snacks.picker.help() end,                                    desc = "Help Pages" },
		{ "<leader>sH",      function() Snacks.picker.highlights() end,                              desc = "Highlights" },
		{ "<leader>si",      function() Snacks.picker.icons() end,                                   desc = "Icons" },
		{ "<leader>sj",      function() Snacks.picker.jumps() end,                                   desc = "Jumps" },
		{ "<leader>sk",      function() Snacks.picker.keymaps() end,                                 desc = "Keymaps" },
		{ "<leader>sl",      function() Snacks.picker.loclist() end,                                 desc = "Location List" },
		{ "<leader>sM",      function() Snacks.picker.man() end,                                     desc = "Man Pages" },
		{ "<leader>sm",      function() Snacks.picker.marks() end,                                   desc = "Marks" },
		{ "<leader>sR",      function() Snacks.picker.resume() end,                                  desc = "Resume" },
		{ "<leader>sq",      function() Snacks.picker.qflist() end,                                  desc = "Quickfix List" },
		{ "<leader>su",      function() Snacks.picker.undo() end,                                    desc = "Undotree" },
		-- ui
		{ "<leader>uC",      function() Snacks.picker.colorschemes() end,                            desc = "Colorschemes" },
		{ "<leader>un",      function() Snacks.notifier.hide() end,                                  desc = "Dismiss All Notifications" },

	},
	---@type snacks.Config
	config = function()
		require("snacks").setup {
			words = {},
			git = {},
			gitbrowse = {},
			dim = {},
			notifier = {},
			image = {},
			scope = {},
			statuscolumn = {
				left = { "mark", "sign" }, -- priority of signs on the left (high to low)
				right = { "fold", "git" }, -- priority of signs on the right (high to low)
				folds = {
					open = true, -- show open fold icons
					git_hl = true, -- use Git Signs hl for fold icons
				},
				git = {
					-- patterns to match Git signs
					patterns = { "GitSign", "MiniDiffSign" },
				},
				refresh = 50, -- refresh at most every 50ms
			},
			picker = {
				layout = {
					preset = "ivy", -- "ivy" | "vscode" | "select" | "sidebar" | "top" | "left" | "right" | "default" | "telescope" | "dropdown"
				},
				matcher = {
					fuzzy = true,
					frecency = true, -- frecency bonus
				},
				-- ui_select = true,
				debug = {
					scores = true,
				},
				layouts = {
					telescope = {
						reverse = false,
						layout = {
							box = "horizontal",
							backdrop = false,
							width = 0.8,
							height = 0.9,
							border = "none",
							{
								box = "vertical",
								{ win = "list",  title = " Results ", title_pos = "center", border = "rounded" },
								{ win = "input", height = 1,          border = "rounded",   title = "{title} {live} {flags}", title_pos = "center" },
							},
							{
								win = "preview",
								title = "{preview:Preview}",
								width = 0.45,
								border = "rounded",
								title_pos = "center",
							},
						},
					},
					default = {
						layout = {
							backdrop = false,
							row = 1,
							width = 0.8,
							min_width = 80,
							height = 0.8,
							border = "rounded",
							box = "horizontal",
							{
								box = "vertical",
								border = "rounded",
								title = "{title} {live} {flags}",
								title_pos = "center",
								{ win = "list",  border = "bottom" },
								{ win = "input", height = 1,       border = "top" },
							},
							{ win = "preview", title = "{preview}", width = 0.4, border = "rounded" },
						},
					},
					ivy = {
						layout = {
							box = "vertical",
							backdrop = false,
							row = -1,
							width = 0,
							height = 0.7,
							border = "top",
							title = " {title} {live} {flags}",
							title_pos = "left",
							{ win = "input", height = 1, border = "bottom" },
							{
								box = "horizontal",
								{ win = "list",    border = "none" },
								{ win = "preview", title = "{preview}", width = 0.5, border = "none" },
							},
						},
					},
					vscode = {
						preview = "main",
						layout = {
							backdrop = false,
							row = 1,
							width = 0.4,
							min_width = 80,
							height = 0.4,
							border = "none",
							box = "vertical",
							{ win = "input",   height = 1,          border = "rounded", title = "{title} {live} {flags}", title_pos = "center" },
							{ win = "list",    border = "hpad" },
							{ win = "preview", title = "{preview}", height = 0.9,       border = "rounded" },
						},
					},
				},
				sources = {
					explorer = {
						finder = "explorer",
						sort = { fields = { "sort" } },
						tree = true,
						supports_live = true,
						follow_file = true,
						auto_close = true,
						jump = { close = false },
						-- layout = { preset = "ivy", preview = false },
					},
				},
			},
		}
	end
}
