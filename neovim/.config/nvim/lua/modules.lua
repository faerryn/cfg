local modules = {
	require'modules.core',
	require'modules.unimpaired',
	require'modules.filetypes',
	require'modules.netrw',
	require'modules.shell',

	require'modules.colorscheme',
	require'modules.statusline',
	require'modules.colorizer',
	require'modules.treesitter',

	require'modules.align',
	require'modules.comments',

	require'modules.git',
	require'modules.telescope',
}

return { setup = function()
	for _, module in ipairs(modules) do
		module.setup()
	end
end }
