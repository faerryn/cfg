local modules = {
	'core',
	'unimpaired',
	'netrw',
	'shell',
	'filetypes',

	'colorscheme',
	'statusline',
	'colorizer',
	'treesitter',

	'align',
	'comments',

	'git',
	'telescope',
	'undotree',
}

return { setup = function()
	for _, name in ipairs(modules) do
		local module = require('modules.'..name)
		local status, err = pcall(module.setup)
		if not status then
			print(string.format("error in module %s:\n%s", name, err))
		end
	end
end }
