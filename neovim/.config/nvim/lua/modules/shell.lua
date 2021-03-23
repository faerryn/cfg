return { setup = function()
	vim.api.nvim_command[[command! Shell edit term://$SHELL]]
	vim.api.nvim_command[[command! Hshell vsplit term://$SHELL]]
	vim.api.nvim_command[[command! Lshell topleft vsplit term://$SHELL]]
	vim.api.nvim_command[[command! Sshell split term://$SHELL]]
	vim.api.nvim_command[[command! Vshell vsplit term://$SHELL]]
	vim.api.nvim_command[[command! Tshell tabnew term://$SHELL]]
end }
