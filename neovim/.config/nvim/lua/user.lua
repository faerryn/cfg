local packs = {}

local function parse(args)
	local pack = {}

	if type(args) == "string" then
		pack.short_url = args
	elseif type(args) == "table" then
		pack.short_url = args[1]
		pack.init = args.init
		pack.config = args.config
		pack.defer = args.defer
	else
		error("TODO")
	end

	pack.url = "https://github.com/"..pack.short_url..".git"

	pack.author = pack.short_url:gsub("/.*", "")
	pack.name = pack.short_url:gsub(".*/", "")
	pack.short_install_path = pack.author.."-"..pack.name
	pack.install_path = vim.fn.stdpath"data".."/site/pack/use/opt/"..pack.short_install_path

	return pack
end

local function packadd(args)
	local pack
	if type(args) == "string" then
		pack = packs[args]
	elseif type(args) == "table" then
		pack = args
	end
	if pack then
		vim.api.nvim_command("packadd "..pack.short_install_path)
		if pack.config then pack.config() end
	else
		vim.api.nvim_command("packadd "..args)
	end
end

local function use(args)
	local pack = parse(args)

	packs[pack.short_url] = pack

	if vim.fn.empty(vim.fn.glob(pack.install_path)) > 0 then
		os.execute([[git clone --depth 1 --recurse-submodules ']]..pack.url..[[' ']]..pack.install_path..[[']])
	end
	if pack.init then pack.init() end
	if not pack.defer then packadd(pack) end

	return pack
end

return {
	use = use,
	packadd = packadd,
}
