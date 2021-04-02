return { setup = function()
	vim.g.zig_fmt_autosave = false
	vim.api.nvim_command'packadd zig.vim'
end }
