return { setup = function()
	vim.g.netrw_banner = 0
	vim.g.netrw_hide = 1
	vim.g.netrw_keepdir = 0
	vim.g.netrw_list_hide = [[^\.\.\?/$]]
	vim.g.netrw_winsize = 25
end }
