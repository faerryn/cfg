return { setup = function()
	vim.g.c_syntax_for_h = 1

	vim.api.nvim_command'packadd zig.vim'
	vim.g.zig_fmt_autosave = 0

	vim.api.nvim_command'packadd vim-sxhkdrc'

	vim.api.nvim_exec([[
	augroup custom_filetypes
	autocmd!
	autocmd FileType rust compiler cargo
	augroup END
	]], false)
end }
